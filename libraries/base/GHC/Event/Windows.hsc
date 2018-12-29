{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.Windows
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- WinIO Windows event manager.
--
-------------------------------------------------------------------------------

module GHC.Event.Windows (
    -- * Manager
    Manager,
    getSystemManager,
    interruptSystemManager,
    wakeupIOManager,
    processRemoteCompletion,

    -- * Overlapped I/O
    associateHandle,
    associateHandle',
    withOverlapped,
    withOverlappedEx,
    StartCallback,
    StartIOCallback,
    CbResult(..),
    CompletionCallback,
    LPOVERLAPPED,

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,

    -- * Utilities
    withException,
    ioSuccess,
    ioFailed,
    getErrorCode,

    -- * I/O Result type
    IOResult(..),

    -- * I/O Event notifications
    HandleData,
    HandleKey (handleValue),
    registerHandle,
    unregisterHandle,

    -- * Console events
    module GHC.Event.Windows.ConsoleEvent
) where

##include "windows_cconv.h"
#include <windows.h>
#include <ntstatus.h>

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (OVERLAPPED, LPOVERLAPPED, OVERLAPPED_ENTRY(..))
import GHC.Event.Internal.Types
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.PSQ            as Q
import qualified GHC.Event.IntTable       as IT
import qualified GHC.Event.Internal as I

import {-# SOURCE #-} Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.IORef
import Data.Foldable (mapM_)
import Data.Maybe
import Data.Word
import Data.Semigroup.Internal (stimesMonoid)
import Data.OldList (deleteBy)
import Foreign       hiding (new)
import Foreign.C
import Foreign.Ptr (ptrToIntPtr)
import Foreign.ForeignPtr.Unsafe
import qualified GHC.Event.Array    as A
import GHC.Arr (Array, (!), listArray, numElements)
import GHC.Base
import {-# SOURCE #-} GHC.Conc.Sync (forkIO, myThreadId, showThreadId,
                                     ThreadId(..), ThreadStatus(..),
                                     threadStatus, sharedCAF)
import GHC.List (replicate, length)
import GHC.Event.Unique
import GHC.Event.TimeOut
import GHC.Event.Windows.ConsoleEvent
import GHC.IOPort
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Enum (Enum)
import GHC.Windows
import System.IO.Unsafe     (unsafePerformIO)
import Text.Show
import System.Posix.Internals (c_write)
import GHC.RTS.Flags

import qualified GHC.Windows as Win32

c_DEBUG_DUMP :: IO Bool
c_DEBUG_DUMP = return True -- scheduler `fmap` getDebugFlags


-- ---------------------------------------------------------------------------
-- I/O manager resume/suspend code

{-# NOINLINE ioManagerThread #-}
ioManagerThread :: MVar (Maybe ThreadId)
ioManagerThread = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetGHCConcWindowsIOManagerThreadStore

foreign import ccall unsafe "getOrSetGHCConcWindowsIOManagerThreadStore"
  getOrSetGHCConcWindowsIOManagerThreadStore :: Ptr a -> IO (Ptr a)

foreign import ccall safe "registerNewIOCPHandle"
  registerNewIOCPHandle :: FFI.IOCP -> IO ()

foreign import ccall safe "registerAlertableWait"
  registerAlertableWait :: FFI.IOCP -> DWORD -> Word64 -> IO ()

foreign import ccall safe "getOverlappedEntries"
  getOverlappedEntries :: Ptr DWORD -> IO (Ptr OVERLAPPED_ENTRY)

foreign import ccall safe "servicedIOEntries"
  servicedIOEntries :: Word64 -> IO ()

------------------------------------------------------------------------
-- Manager

type IOCallback = CompletionCallback ()

data CompletionData = CompletionData {-# UNPACK #-} !(ForeignPtr OVERLAPPED)
                                     !HANDLE !IOCallback

-- I don't expect a lot of events, so a simple linked lists should be enough.
type EventElements = [(Event, HandleData)]
data EventData = EventData { evtTopLevel :: !Event, evtElems :: !EventElements }

instance Monoid EventData where
  mempty  = EventData evtNothing []
  mappend = \a b -> EventData (evtTopLevel a <> evtTopLevel b)
                              (evtElems a ++ evtElems b)

instance Semigroup EventData where
  (<>)   = mappend
  stimes = stimesMonoid

data IOResult a
  = IOSuccess { ioValue :: a }
  | IOFailed  { ioErrCode :: Maybe Int }

data Manager = Manager
    { mgrIOCP         :: {-# UNPACK #-} !FFI.IOCP
    , mgrClock        ::                !Clock
    , mgrUniqueSource :: {-# UNPACK #-} !UniqueSource
    , mgrTimeouts     :: {-# UNPACK #-} !(IORef TimeoutQueue)
    , mgrCallbacks    :: {-# UNPACK #-}
                         !(MVar (IT.IntTable CompletionData))
    , mgrEvntHandlers :: {-# UNPACK #-}
                         !(MVar (IT.IntTable EventData))
    , mgrOverlappedEntries
                      :: {-#UNPACK #-} !(A.Array OVERLAPPED_ENTRY)
    }

-- This needs to finish without making any calls to anything requiring the I/O
-- manager otherwise we'll get into some weird synchronization issues.
new :: IO Manager
new = do
    debugIO "Starting io-manager..."
    mgrIOCP         <- FFI.newIOCP
    when (not threaded) $
      registerNewIOCPHandle mgrIOCP
    debugIO $ "iocp: " ++ show mgrIOCP
    mgrClock        <- getClock
    mgrUniqueSource <- newSource
    mgrTimeouts     <- newIORef Q.empty
    mgrCallbacks    <- newMVar =<< IT.new callbackArraySize
    mgrOverlappedEntries <- A.new 64
    mgrEvntHandlers <- newMVar =<< IT.new callbackArraySize
    let !mgr = Manager{..}
    return mgr
      where
        replicateM n x = sequence (replicate n x)

{-# INLINE startIOManagerThread #-}
startIOManagerThread :: IO () -> IO ()
startIOManagerThread loop = do
  modifyMVar_ ioManagerThread $ \old -> do
    let create = do debugIO "spawning thread.."
                    t <- if threaded
                            then forkOS loop
                            else forkIO loop
                    setStatus WinIORunning
                    debugIO $ "created io-manager thread."
                    return (Just t)
    case old of
      Nothing -> create
      Just t  -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> do status <- getStatus
                               case status of
                                WinIOBlocked  -> do
                                  c_sendIOManagerEvent io_MANAGER_WAKEUP
                                  debugIO $ "woke up manager on thread: "
                                            ++ showThreadId t
                                WinIOScanning -> do
                                  debugIO $ "interrupted IOCP timeout wait on thread: "
                                            ++ showThreadId t
                                WinIOWaiting -> do
                                  debugIO $ "interrupted IOCP long wait on thread: "
                                            ++ showThreadId t
                                _             -> return ()
                               when (status /= WinIORunning)
                                    interruptSystemManager
                               return (Just t)

data WinIOStatus
  = WinIORunning
  | WinIOScanning
  | WinIOWaiting
  | WinIOBlocked
  | WinIODone
  deriving Eq


statusWinIO :: MVar WinIOStatus
statusWinIO = unsafePerformIO $ newMVar WinIODone

setStatus :: WinIOStatus -> IO ()
setStatus val = modifyMVar_ statusWinIO (\a -> return val)

getStatus :: IO WinIOStatus
getStatus = readMVar statusWinIO

requests :: MVar Word64
requests = unsafePerformIO $ newMVar 0

addRequest :: IO Word64
addRequest = modifyMVar requests (\x -> return (x + 1, x + 1))

removeRequest :: IO Word64
removeRequest = modifyMVar requests (\x -> return (x - 1, x - 1))

outstandingRequests :: IO Word64
outstandingRequests = withMVar requests return

getSystemManager :: IO Manager
getSystemManager = readMVar managerRef

managerRef :: MVar Manager
managerRef = unsafePerformIO $ new >>= newMVar
{-# NOINLINE managerRef #-}

interruptSystemManager :: IO ()
interruptSystemManager = do
  mgr <- getSystemManager
  status <- getStatus
  when (status /= WinIORunning) $
        do debugIO "interrupt received.."
           FFI.postQueuedCompletionStatus (mgrIOCP mgr) 0 0 nullPtr
  when (status == WinIODone) $
        do debugIO $  "I/O manager is dead. You need to revive it first. "
                   ++ "Try wakeupIOManager instead."


-- must be power of 2
callbackArraySize :: Int
callbackArraySize = 32

lpoverlappedToInt :: LPOVERLAPPED -> Int
lpoverlappedToInt lpol = fromIntegral (ptrToIntPtr lpol)
{-# INLINE lpoverlappedToInt #-}

hashOverlapped :: LPOVERLAPPED -> Int
hashOverlapped lpol = (lpoverlappedToInt lpol) .&. (callbackArraySize - 1)
{-# INLINE hashOverlapped #-}

callbackTableVar :: Manager -> MVar (IT.IntTable CompletionData)
callbackTableVar = mgrCallbacks
{-# INLINE callbackTableVar #-}


-----------------------------------------------------------------------
-- Time utilities

secondsToNanoSeconds :: Seconds -> Q.Prio
secondsToNanoSeconds s = ceiling $ s * 1000000000

nanoSecondsToSeconds :: Q.Prio -> Seconds
nanoSecondsToSeconds n = fromIntegral n / 1000000000.0

------------------------------------------------------------------------
-- Overlapped I/O

-- | Callback that starts the overlapped I/O operation.
-- It must return successfully if and only if an I/O completion has been
-- queued.  Otherwise, it must throw an exception, which 'withOverlapped'
-- will rethrow.
type StartCallback a = LPOVERLAPPED -> IO a

-- | Specialized callback type for I/O Completion Ports calls using
-- withOverlapped.
type StartIOCallback a = StartCallback (CbResult a)

-- | CallBack result type to disambiguate between the different states
-- an I/O Completion call could be in.
data CbResult a = CbDone (Maybe DWORD) -- ^ Request was handled immediately, no queue.
                | CbPending            -- ^ Queued and handled by I/O manager
                | CbError a            -- ^ I/O request abort, return failure immediately
                | CbNone Bool          -- ^ The caller did not do any checking, the I/O
                                       --   manager will perform additional checks.

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                          -> DWORD     -- ^ Number of bytes transferred
                          -> IO a

associateHandle' :: HANDLE -> IO ()
associateHandle' hwnd
  = do mngr <- getSystemManager
       associateHandle mngr hwnd

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    -- Use as completion key the file handle itself, so we can track completion
    FFI.associateHandleWithIOCP mgrIOCP h (fromIntegral $ ptrToWordPtr h)

-- Note [RTS Scheduler error propagation]
--
-- The RTS scheduler will save and restore Last Error across Haskell threads
-- when it suspends or resumes threads.  This is normally fine since the errors
-- are mostly informative.  The problem starts with I/O CP calls, where in
-- particular ERROR_IO_PENDING is a dangerous one.  During long running threads
-- the scheduler will incorrectly restore this error, which indicates there are
-- still pending I/O operations.  If we're doing lazy reads then this tricks
-- the I/O manager into thinking we still have things we need to read, while
-- there is none.  So it just keeps looping.  Coincidentally it also tricks
-- itself into not resuming the main thread, as that also thinks events are
-- pending.
--
-- To mitigate this, we purge the error immediately after making the any I/O
-- call that may result in pending I/O operations.
--
-- Crude but effective, however since this is still Haskell code, need to
-- figure out a more thread-safe way.  Perhaps I should just prevent this code
-- from being saved/restored.  But that may break an I/O read that was in
-- progress.. Grrr

getErrorCode :: IO ErrCode
getErrorCode = do
    err <- getLastError
    -- See Note [RTS Scheduler error propagation]
    -- when (err /= 0) $ FFI.setLastError 0
    return err

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIoEx@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlappedEx :: Manager
                 -> String
                 -> HANDLE
                 -> Word64 -- ^ Value to use for the @OVERLAPPED@
                           --   structure's Offset/OffsetHigh members.
                 -> StartIOCallback Int
                 -> CompletionCallback (IOResult a)
                 -> IO (IOResult a)
withOverlappedEx mgr fname h offset startCB completionCB = do
    signal <- newIOPort :: IO (IOPort (IOResult a))
    let dbg s = s ++ " (" ++ show h ++ ":" ++ show offset ++ ")"
    let signalReturn a = failIfFalse_ (dbg "signalReturn") $
                            writeIOPort signal (IOSuccess a)
        signalThrow ex = failIfFalse_ (dbg "signalThrow") $
                            writeIOPort signal (IOFailed ex)
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        fptr <- FFI.allocOverlapped offset
        let lpol = unsafeForeignPtrToPtr fptr

        execute <- startCB lpol `onException`
                        (CbError `fmap` Win32.getLastError) >>= \result -> do
          -- Check to see if the operation was completed on a
          -- non-overlapping handle or was completed immediately.
          -- e.g. stdio redirection or data in cache, FAST I/O.
          success <- FFI.overlappedIOStatus lpol
          err     <- fmap fromIntegral getErrorCode
          -- Determine if the caller has done any checking.  If not then check
          -- to see if the request was completed on synchronously.  We have to
          -- in order to prevent deadlocks as if it has completed synchronously
          -- the completion wouldn't have been queued.
          let result' =
                case result of
                  CbNone ret | success == #{const ERROR_SUCCESS}       -> CbDone Nothing
                             | success == #{const STATUS_END_OF_FILE}  -> CbDone Nothing
                             | err     == #{const ERROR_IO_PENDING}    -> CbPending
                             | err     == #{const ERROR_IO_INCOMPLETE} -> CbPending
                             | err     == #{const ERROR_HANDLE_EOF}    -> CbDone Nothing
                             | not ret                                 -> CbError err
                             | otherwise                               -> CbPending
                  _                                                    -> result
          case result' of
            CbNone    _ -> error "shouldn't happen."
            CbPending   -> do
              -- | Before we enqueue check to see if operation finished in the
              -- mean time, since caller may not have done this.
              finished <- FFI.getOverlappedResult h lpol False
              debugIO $ "== " ++ show (finished)
              status <- FFI.overlappedIOStatus lpol
              debugIO $ "== >< " ++ show (status)
              let done_early =  status == #{const ERROR_SUCCESS}
                             || status == #{const STATUS_END_OF_FILE}

              debugIO $ "== >*< " ++ show (finished, done_early)
              case (finished, done_early) of
                (Nothing, False) -> do
                    _ <- withMVar (callbackTableVar mgr) $
                            IT.insertWith (flip const) (lpoverlappedToInt lpol)
                                          (CompletionData fptr h completionCB')
                    reqs <- addRequest
                    debugIO $ "+1.. " ++ show reqs ++ " requests queued. | " ++ show lpol
                    wakeupIOManager
                    return result'
                _                -> do
                  debugIO "request handled immediately (o/b), not queued."
                  return $ CbDone Nothing
            CbError err -> signalThrow (Just err) >> return result'
            CbDone  _   -> debugIO "request handled immediately (o), not queued." >> return result'

        let cancel e = do
                        debugIO $ "## Exception occurred. Cancelling request... "
                        debugIO $ show (e :: SomeException)
                        _ <- uninterruptibleMask_ $ FFI.cancelIoEx' h lpol
                        -- we need to wait for the cancellation before removing
                        -- the pointer.
                        debugIO $ "## Waiting for cancellation record... "
                        FFI.getOverlappedResult h lpol True
                        mCD <- withMVar (callbackTableVar mgr) $
                                IT.delete (lpoverlappedToInt lpol)
                        case mCD of
                          Nothing                              -> return ()
                          Just (CompletionData _fptr _hwnd cb) -> do
                            reqs <- removeRequest
                            debugIO $ "-1.. " ++ show reqs ++ " requests queued after error."
                            status <- fmap fromIntegral getErrorCode
                            cb status 0
                        when (not threaded) $
                          do num_remaining <- outstandingRequests
                             servicedIOEntries num_remaining
                        return $ IOFailed Nothing
        let runner = do debugIO $ (dbg ":: waiting ") ++ " | "  ++ show lpol
                        res <- readIOPort signal `catch` cancel
                        debugIO $ dbg ":: signaled "
                        case res of
                          IOFailed err -> FFI.throwWinErr fname (maybe 0 fromIntegral err)
                          _            -> return res

        -- Sometimes we shouldn't bother with the I/O manager as the call has
        -- failed or is done.
        case execute of
          CbPending    -> runner
          CbDone rdata -> do
            debugIO $ dbg $ ":: done " ++ show lpol ++ " - " ++ show rdata
            bytes <- if isJust rdata
                        then return rdata
                        else FFI.getOverlappedResult h lpol False
            case bytes of
              Just res -> completionCB 0 res
              Nothing  -> do err <- FFI.overlappedIOStatus lpol
                             completionCB err 0
          CbError err  -> do let err' = fromIntegral err
                             completionCB err' 0
          _            -> do error "unexpected case in `execute'"

-- Safe version of function
withOverlapped :: String
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartIOCallback Int
               -> CompletionCallback (IOResult a)
               -> IO (IOResult a)
withOverlapped fname h offset startCB completionCB = do
  mngr <- getSystemManager
  withOverlappedEx mngr fname h offset startCB completionCB

------------------------------------------------------------------------
-- I/O Utilities

withException :: String -> IO (IOResult a) -> IO a
withException name fn
 = do res <- fn
      case res of
       IOSuccess a         -> return a
       IOFailed (Just err) -> FFI.throwWinErr name $ fromIntegral err
       IOFailed Nothing    -> FFI.throwWinErr name 0

ioSuccess :: a -> IO (IOResult a)
ioSuccess = return . IOSuccess

ioFailed :: Integral a => a -> IO (IOResult a)
ioFailed = return . IOFailed . Just . fromIntegral

------------------------------------------------------------------------
-- Timeouts

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later un-register or update the timeout.
-- The timeout is automatically unregistered when it fires.
--
-- The 'TimeoutCallback' will not be called more than once.
registerTimeout :: Manager -> Seconds -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr@Manager{..} relTime cb = do
    key <- newUnique mgrUniqueSource
    if relTime <= 0 then cb
    else do
      now <- getTime mgrClock
      let !expTime = secondsToNanoSeconds $ now + relTime
      editTimeouts mgr (Q.unsafeInsertNew key expTime cb)
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = secondsToNanoSeconds $ now + relTime
    editTimeouts mgr (Q.adjust (const expTime) key)

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
    editTimeouts mgr (Q.delete key)

editTimeouts :: Manager -> TimeoutEdit -> IO ()
editTimeouts mgr g = do
  atomicModifyIORef' (mgrTimeouts mgr) $ \tq -> (g tq, ())
  wakeupIOManager

------------------------------------------------------------------------
-- I/O manager loop

-- | Call all expired timeouts, and return how much time until the next
-- | expiration.
runExpiredTimeouts :: Manager -> IO (Maybe Seconds)
runExpiredTimeouts Manager{..} = do
    now <- getTime mgrClock
    (expired, delay) <- atomicModifyIORef' mgrTimeouts (mkTimeout now)
    -- Execute timeout callbacks.
    mapM_ Q.value expired
    debugIO $ "expired calls: " ++ show (length expired)
    return delay
      where
        mkTimeout :: Seconds -> TimeoutQueue ->
                     (TimeoutQueue, ([Q.Elem TimeoutCallback], Maybe Seconds))
        mkTimeout now tq =
            let (tq', (expired, sec)) = mkTimeout' (secondsToNanoSeconds now) tq
            in (tq', (expired, fmap nanoSecondsToSeconds sec))
        mkTimeout' :: Q.Prio -> TimeoutQueue ->
                     (TimeoutQueue, ([Q.Elem TimeoutCallback], Maybe Q.Prio))
        mkTimeout' now tq =
           -- Remove timeouts with expiration <= now.
           let (expired, tq') = Q.atMost now tq in
           -- See how soon the next timeout expires.
           case Q.prio `fmap` Q.findMin tq' of
            Nothing ->
                (tq', (expired, Nothing))
            Just t ->
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let !t' = t - now
                in (tq', (expired, Just t'))

-- | Return the delay argument to pass to GetQueuedCompletionStatus.
fromTimeout :: Maybe Seconds -> Word32
fromTimeout Nothing                 = 120000
fromTimeout (Just sec) | sec > 120  = 120000
                       | sec > 0    = ceiling (sec * 1000)
                       | otherwise  = 0

step :: Bool -> Manager -> IO (Bool, Maybe Seconds)
step maxDelay mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    let timer = if maxDelay && delay == Nothing
                   then #{const INFINITE}
                   else fromTimeout delay
    debugIO $ "next timeout: " ++ show delay
    debugIO $ "next timer: " ++ show timer -- todo: print as hex
    -- The getQueuedCompletionStatusEx call will remove entries queud by the OS
    -- and returns the finished ones in mgrOverlappedEntries and the number of
    -- entries removed.
    case (maxDelay, delay) of
      (_    , Just{} ) -> do setStatus WinIOWaiting
                             debugIO "I/O manager waiting."
      (False, Nothing) -> do setStatus WinIOScanning
                             debugIO "I/O manager pausing."
      (True , Nothing) -> do setStatus WinIOBlocked
                             debugIO "I/O manager deep sleep."
    n <- if threaded
            then FFI.getQueuedCompletionStatusEx mgrIOCP mgrOverlappedEntries timer
            else do num_req <- outstandingRequests
                    registerAlertableWait mgrIOCP timer num_req
                    return 0
    setStatus WinIORunning
    processCompletion mgr n delay

processCompletion :: Manager -> Int -> Maybe Seconds -> IO (Bool, Maybe Seconds)
processCompletion mgr@Manager{..} n delay = do
    debugIO $ "completed completions: " ++ show n

    -- If some completions are done, we need to process them and call their
    -- callbacks.  We then remove the callbacks from the bookkeeping and resize
    -- the index if required.
    when (n > 0) $ do
      A.forM_ mgrOverlappedEntries $ \oe -> do
          debugIO $ " $ checking " ++ show (lpOverlapped oe)
          mCD <- withMVar (callbackTableVar mgr) $ \tbl ->
                   IT.delete (lpoverlappedToInt (lpOverlapped oe)) tbl
          case mCD of
            Nothing                        -> return ()
            Just (CompletionData _fptr _hwnd cb) -> do
              reqs <- removeRequest
              debugIO $ "-1.. " ++ show reqs ++ " requests queued."
              -- It's safe to block here since the operation completed it will
              -- return immediately in most cases.
              -- _ <- FFI.getOverlappedResult _hwnd (lpOverlapped oe) True
              status <- FFI.overlappedIOStatus (lpOverlapped oe)
              cb status (dwNumberOfBytesTransferred oe)

      -- clear the array so we don't erroneously interpret the output, in
      -- certain circumstances like lockFileEx the code could return 1 entry
      -- removed but the file data not been filled in.
      -- TODO: Maybe not needed..
      A.clear mgrOverlappedEntries

      -- Check to see if we received the maximum amount of entries we could
      -- this likely indicates a high number of I/O requests have been queued.
      -- In which case we should process more at a time.
      cap <- A.capacity mgrOverlappedEntries
      when (cap == n) $ A.ensureCapacity mgrOverlappedEntries (2*cap)

    -- Keep running if we still have some work queued or
    -- if we have a pending delay.
    reqs <- outstandingRequests
    debugIO $ "outstanding requests: " ++ show reqs
    let more = reqs > 0
    debugIO $ "has more: " ++ show more ++ " - removed: " ++  show n
    return (more || (isJust delay && threaded), delay)

processRemoteCompletion :: IO ()
processRemoteCompletion = do
  alloca $ \ptr_n -> do
    debugIO "processRemoteCompletion :: start ()"
    entries <- getOverlappedEntries ptr_n
    n <- fromIntegral `fmap` peek ptr_n
    completed <- peekArray n entries
    mngr <- getSystemManager
    let arr = mgrOverlappedEntries mngr
    A.unsafeSplat arr entries n
    _ <- processCompletion mngr n Nothing
    num_left <- outstandingRequests
    servicedIOEntries num_left
    setStatus WinIOBlocked
    debugIO "processRemoteCompletion :: done ()"
    return ()

io_mngr_loop :: HANDLE -> Manager -> IO ()
io_mngr_loop event mgr = go False
    where
      go maxDelay =
          do setStatus WinIORunning
             (more, delay) <- step maxDelay mgr
             debugIO "I/O manager stepping."
             r2 <- c_readIOManagerEvent
             exit <-
               case r2 of
                 _ | r2 == io_MANAGER_WAKEUP -> return False
                 _ | r2 == io_MANAGER_DIE    -> return True
                 0 -> return False -- spurious wakeup
                 _ -> do debugIO $ "handling console event: " ++ show (r2 `shiftR` 1)
                         start_console_handler (r2 `shiftR` 1)
                         return False

             -- If we have no more work to do, or something from the outside
             -- told us to stop then we let the thread die and stop the I/O
             -- manager.  It will be woken up again when there is more to do.
             case () of
               _ | exit -> do setStatus WinIODone
                              debugIO "I/O manager shutting down."
               _ | not threaded -> do setStatus WinIOBlocked
                                      debugIO "I/O manager single threaded halt."
               _ | isJust delay -> go False
               -- We seem to have more work but no ETA for it.
               -- So just retry until we run out of work.
               _ | more -> go False
               _        -> go True

-- TODO: Do some refactoring to share this between here and GHC.Conc.POSIX
-- must agree with rts/win32/ThrIOManager.c
io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = 0xffffffff
io_MANAGER_DIE    = 0xfffffffe

wakeupIOManager :: IO ()
wakeupIOManager
  = do mngr <- getSystemManager
       status <- getStatus
       when (status /= WinIORunning) $ do
         event <- c_getIOManagerEvent
         debugIO "waking up I/O manager."
         startIOManagerThread (io_mngr_loop event mngr)

foreign import ccall unsafe "getIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_getIOManagerEvent :: IO HANDLE

foreign import ccall unsafe "readIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_readIOManagerEvent :: IO Word32

foreign import ccall unsafe "sendIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_sendIOManagerEvent :: Word32 -> IO ()

foreign import WINDOWS_CCONV "WaitForSingleObject"
   c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool


-- ---------------------------------------------------------------------------
-- I/O manager event notifications


data HandleData = HandleData {
      tokenKey        :: {-# UNPACK #-} !HandleKey
    , tokenEvents     :: {-# UNPACK #-} !EventLifetime
    , _handleCallback :: !EventCallback
    }

-- | A file handle registration cookie.
data HandleKey = HandleKey {
      handleValue  :: {-# UNPACK #-} !HANDLE
    , handleUnique :: {-# UNPACK #-} !Unique
    } deriving ( Eq   -- ^ @since 4.4.0.0
               , Show -- ^ @since 4.4.0.0
               )

-- | Callback invoked on I/O events.
type EventCallback = HandleKey -> Event -> IO ()

registerHandle :: Manager -> EventCallback -> HANDLE -> Event -> Lifetime
               -> IO HandleKey
registerHandle mgr@(Manager{..}) cb hwnd evs lt = do
  u <- newUnique mgrUniqueSource
  let reg   = HandleKey hwnd u
      hwnd' = fromIntegral $ ptrToIntPtr hwnd
      el    = I.eventLifetime evs lt
      !hwdd = HandleData reg el cb
      event = EventData evs [(evs, hwdd)]
  _ <- withMVar mgrEvntHandlers $ \evts -> do
          IT.insertWith mappend hwnd' event evts
  wakeupIOManager
  return reg

unregisterHandle :: Manager -> HandleKey -> IO ()
unregisterHandle mgr@(Manager{..}) key@HandleKey{..} = do
  withMVar mgrEvntHandlers $ \evts -> do
    let hwnd' = fromIntegral $ ptrToIntPtr handleValue
    val <- IT.lookup hwnd' evts
    case val of
      Nothing -> return ()
      Just (EventData evs lst) -> do
        let cmp (_, a) (_, b) = tokenKey a == tokenKey b
            key'    = (undefined, HandleData key undefined undefined)
            updated = deleteBy cmp key' lst
            new_lst = EventData evs updated
        _ <- IT.updateWith (\_ -> return new_lst) hwnd' evts
        return ()


-- ---------------------------------------------------------------------------
-- debugging

debugIO :: String -> IO ()
debugIO s
  = do debug <- c_DEBUG_DUMP
       if debug
          then do tid <- myThreadId
                  let pref = if threaded then "\t" else ""
                  _   <- withCStringLen (pref ++ "winio: " ++ s ++ " (" ++
                                         showThreadId tid ++ ")\n") $
                         \(p, len) -> c_write 2 (castPtr p) (fromIntegral len)
                  return ()
          else do return ()

