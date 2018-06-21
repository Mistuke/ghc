{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module GHC.Event.Windows (
    -- * Manager
    Manager,
    getSystemManager,

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

    -- * IO Result type
    IOResult(..)
) where

#include "windows_cconv.h"

import GHC.Event.Windows.Clock   (Clock, Seconds, getClock, getTime)
import GHC.Event.Windows.FFI     (OVERLAPPED, LPOVERLAPPED, OVERLAPPED_ENTRY(..))
import qualified GHC.Event.Windows.FFI    as FFI
import qualified GHC.Event.PSQ            as Q
import qualified GHC.Event.IntTable       as IT

import {-# SOURCE #-} Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.IORef
import Data.Foldable (mapM_)
import Data.Maybe
import Data.Word
import Foreign       hiding (new)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import qualified GHC.Event.Array    as A
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import {-# SOURCE #-} GHC.Conc.Sync (forkIO, myThreadId, showThreadId,
                                     ThreadId(..), ThreadStatus(..),
                                     threadStatus, sharedCAF)
import GHC.List (replicate, length)
import GHC.Event.Unique
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
c_DEBUG_DUMP = scheduler `fmap` getDebugFlags


-- ---------------------------------------------------------------------------
-- I/O manager resume/suspend code

{-# NOINLINE ioManagerThread #-}
ioManagerThread :: MVar (Maybe ThreadId)
ioManagerThread = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetGHCConcWindowsIOManagerThreadStore

foreign import ccall unsafe "getOrSetGHCConcWindowsIOManagerThreadStore"
    getOrSetGHCConcWindowsIOManagerThreadStore :: Ptr a -> IO (Ptr a)

------------------------------------------------------------------------
-- Manager

type IOCallback = CompletionCallback ()

data CompletionData = CompletionData {-# UNPACK #-} !(ForeignPtr OVERLAPPED)
                                     !IOCallback

data IOResult a
  = IOSuccess { ioValue :: a }
  | IOFailed  { ioErrCode :: Maybe Int }


data Manager = Manager
    { mgrIOCP         :: {-# UNPACK #-} !FFI.IOCP
    , mgrClock        ::                !Clock
    , mgrUniqueSource :: {-# UNPACK #-} !UniqueSource
    , mgrTimeouts     :: {-# UNPACK #-} !(IORef TimeoutQueue)
    , mgrCallbacks    :: {-# UNPACK #-}
                         !(Array Int (MVar (IT.IntTable CompletionData)))
    , mgrOverlappedEntries
                      :: {-#UNPACK #-} !(A.Array OVERLAPPED_ENTRY)
    }

new :: IO Manager
new = do
    debugIO "Starting io-manager..."
    mgrIOCP         <- FFI.newIOCP
    mgrClock        <- getClock
    mgrUniqueSource <- newSource
    mgrTimeouts     <- newIORef Q.empty
    mgrCallbacks    <- fmap (listArray (0, callbackArraySize-1)) $
           replicateM callbackArraySize (newMVar =<< IT.new 8)
    mgrOverlappedEntries <- A.new 64
    let !mgr = Manager{..}
    event <- c_getIOManagerEvent
    startIOManagerThread (io_mngr_loop event mgr)
    return mgr
      where
        replicateM n x = sequence (replicate n x)

{-# INLINE startIOManagerThread #-}
startIOManagerThread :: IO () -> IO ()
startIOManagerThread loop = do
  modifyMVar_ ioManagerThread $ \old -> do
    let create = do debugIO "spawning thread.."
                    t <- forkIO loop
                    debugIO $ "created io-manager thread."
                    return (Just t)
    case old of
      Nothing -> create
      Just t  -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> do c_sendIOManagerEvent io_MANAGER_WAKEUP
                               return (Just t)


getSystemManager :: IO (Maybe Manager)
getSystemManager = readIORef managerRef

managerRef :: IORef (Maybe Manager)
managerRef = unsafePerformIO $
  if threaded
     then new >>= newIORef . Just
     else new >>= newIORef . Just -- newIORef Nothing
{-# NOINLINE managerRef #-}

-- must be power of 2
callbackArraySize :: Int
callbackArraySize = 32

lpoverlappedToInt :: LPOVERLAPPED -> Int
lpoverlappedToInt lpol = fromIntegral (ptrToIntPtr lpol)
{-# INLINE lpoverlappedToInt #-}

hashOverlapped :: LPOVERLAPPED -> Int
hashOverlapped lpol = (lpoverlappedToInt lpol) .&. (callbackArraySize - 1)
{-# INLINE hashOverlapped #-}

callbackTableVar :: Manager -> LPOVERLAPPED -> MVar (IT.IntTable CompletionData)
callbackTableVar mgr lpol = mgrCallbacks mgr ! hashOverlapped lpol
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
data CbResult a = CbDone    -- ^ Request was handled immediately, no queue.
                | CbPending -- ^ Queued and handled by I/O manager
                | CbError a -- ^ I/O request abort, return failure immediately

-- | Called when the completion is delivered.
type CompletionCallback a = ErrCode   -- ^ 0 indicates success
                          -> DWORD     -- ^ Number of bytes transferred
                          -> IO a

associateHandle' :: HANDLE -> IO ()
associateHandle' hwnd
  = do mngr <- getSystemManager
       maybe (return ()) (flip associateHandle hwnd) mngr

-- | Associate a 'HANDLE' with the I/O manager's completion port.  This must be
-- done before using the handle with 'withOverlapped'.
associateHandle :: Manager -> HANDLE -> IO ()
associateHandle Manager{..} h =
    -- Use as completion key the file handle itself, so we can track completion
    FFI.associateHandleWithIOCP mgrIOCP h (fromIntegral $ ptrToWordPtr h)

-- | Start an overlapped I/O operation, and wait for its completion.  If
-- 'withOverlapped' is interrupted by an asynchronous exception, the operation
-- will be canceled using @CancelIoEx@.
--
-- 'withOverlapped' waits for a completion to arrive before returning or
-- throwing an exception.  This means you can use functions like
-- 'Foreign.Marshal.Alloc.alloca' to allocate buffers for the operation.
withOverlappedThreaded_ :: Manager
                        -> String
                        -> HANDLE
                        -> Word64 -- ^ Value to use for the @OVERLAPPED@
                                  --   structure's Offset/OffsetHigh members.
                        -> StartIOCallback Int
                        -> CompletionCallback (IOResult a)
                        -> IO (IOResult a)
withOverlappedThreaded_ mgr fname h offset startCB completionCB = do
    signal <- newEmptyMVar :: IO (MVar (IOResult a))
    let signalReturn a = tryPutMVar signal (IOSuccess a) >> return ()
        signalThrow ex = tryPutMVar signal (IOFailed ex) >> return ()
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        fptr <- FFI.allocOverlapped offset
        let lpol = unsafeForeignPtrToPtr fptr
        _ <- withMVar (callbackTableVar mgr lpol) $ \tbl ->
             IT.insertWith (flip const) (lpoverlappedToInt lpol)
               (CompletionData fptr completionCB') tbl

        execute <- (startCB lpol) `onException`
                        (CbError `fmap` Win32.getLastError) >>= \result -> do
          case result of
            CbPending   -> wakeupIOManager
            CbError err -> do
              _ <- withMVar (callbackTableVar mgr lpol) $ \tbl ->
                    IT.delete (lpoverlappedToInt lpol) tbl
              signalThrow (Just err)
            CbDone      -> do
              _ <- withMVar (callbackTableVar mgr lpol) $ \tbl ->
                    IT.delete (lpoverlappedToInt lpol) tbl
              return ()
          return result

        let cancel = uninterruptibleMask_ $ FFI.cancelIoEx h lpol
        let runner = do res <- takeMVar signal `onException` cancel
                        case res of
                          IOFailed err -> FFI.throwWinErr fname (maybe 0 fromIntegral err)
                          _            -> return res

        -- Sometimes we shouldn't bother with the I/O manager as the call has
        -- failed or is done.
        case execute of
          CbPending   -> runner
          CbDone      -> do
            -- It's safe to block here since the operation completed it will
            -- return immediately.
            bytes <- FFI.getOverlappedResult h lpol True
            case bytes of
              Just res -> completionCB 0 res
              Nothing  -> do err <- FFI.overlappedIOStatus lpol
                             completionCB err 0
          CbError err -> do let err' = fromIntegral err
                            completionCB err' 0

-- This will block the current haskell thread
-- but will allow you to cancel the operation from
-- another haskell thread since they are on the same
-- OS thread.
withOverlappedNonThreaded_ :: String
                           -> HANDLE
                           -> Word64 -- ^ Value to use for the @OVERLAPPED@
                                     --   structure's Offset/OffsetHigh members.
                           -> StartIOCallback Int
                           -> CompletionCallback (IOResult a)
                           -> IO (IOResult a)
withOverlappedNonThreaded_ _fname h offset startCB completionCB = do
    let signalReturn a = return $ IOSuccess a
        signalThrow ex = return $ IOFailed ex
    mask_ $ do
        let completionCB' e b = completionCB e b >>= \result ->
                                  case result of
                                    IOSuccess val -> signalReturn val
                                    IOFailed  err -> signalThrow err
        fptr <- FFI.allocOverlapped offset
        let lpol = unsafeForeignPtrToPtr fptr

        startCB lpol `onException` (Just `fmap` Win32.getLastError) >>= \result ->
          case result of
            CbError err -> signalThrow $ Just err
            _           -> do
              bytes <- FFI.getOverlappedResult h lpol True
              case bytes of
                Just num_bytes -> completionCB' 0 num_bytes
                Nothing        -> do err <- FFI.overlappedIOStatus lpol
                                     completionCB' err 0

-- Safe version of function
withOverlapped :: String
               -> HANDLE
               -> Word64 -- ^ Value to use for the @OVERLAPPED@
                         --   structure's Offset/OffsetHigh members.
               -> StartIOCallback Int
               -> CompletionCallback (IOResult a)
               -> IO (IOResult a)
withOverlapped fname h offset startCB completionCB
  = do mngr <- getSystemManager
       case mngr of
         Nothing    -> withOverlappedNonThreaded_    fname h offset startCB completionCB
         Just mngr' -> withOverlappedThreaded_ mngr' fname h offset startCB completionCB

withOverlappedEx :: Maybe Manager
                 -> String
                 -> HANDLE
                 -> Word64 -- ^ Value to use for the @OVERLAPPED@
                           --   structure's Offset/OffsetHigh members.
                 -> StartIOCallback Int
                 -> CompletionCallback (IOResult a)
                 -> IO (IOResult a)
withOverlappedEx mngr fname h offset startCB completionCB
  = do case mngr of
         Nothing    -> withOverlappedNonThreaded_    fname h offset startCB completionCB
         Just mngr' -> withOverlappedThreaded_ mngr' fname h offset startCB completionCB

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

-- | A priority search queue, with timeouts as priorities.
type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

-- | A timeout registration cookie.
newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)

-- | Register an action to be performed in the given number of seconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the timeout.
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
      editTimeouts mgr (Q.insert key expTime cb)
      wakeupIOManager
    return $ TK key

-- | Update an active timeout to fire in the given number of seconds (from the
-- time 'updateTimeout' is called), instead of when it was going to fire.
-- This has no effect if the timeout has already fired.
updateTimeout :: Manager -> TimeoutKey -> Seconds -> IO ()
updateTimeout mgr (TK key) relTime = do
    now <- getTime (mgrClock mgr)
    let !expTime = secondsToNanoSeconds $ now + relTime
    editTimeouts mgr (Q.adjust (const expTime) key)
    wakeupIOManager

-- | Unregister an active timeout.  This is a harmless no-op if the timeout is
-- already unregistered or has already fired.
--
-- Warning: the timeout callback may fire even after
-- 'unregisterTimeout' completes.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
    editTimeouts mgr (Q.delete key)
    wakeupIOManager

editTimeouts :: Manager -> TimeoutEdit -> IO ()
editTimeouts mgr g = atomicModifyIORef' (mgrTimeouts mgr) $ \tq -> (g tq, ())

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

step :: Manager -> IO (Bool, Maybe Seconds)
step mgr@Manager{..} = do
    delay <- runExpiredTimeouts mgr
    debugIO $ "next timeout: " ++ show delay
    -- The getQueuedCompletionStatusEx call will remove entries queud by the OS
    -- and returns the finished ones in mgrOverlappedEntries and the number of
    -- entries removed.
    n <- FFI.getQueuedCompletionStatusEx mgrIOCP mgrOverlappedEntries
                                         (fromTimeout delay)
    debugIO $ "completed completions: " ++ show n

    -- If some completions are done, we need to process them and call their
    -- callbacks.  We then remove the callbacks from the bookkeeping and resize
    -- the index if required.
    when (n > 0) $ do
      A.forM_ mgrOverlappedEntries $ \oe -> do
          mCD <- withMVar (callbackTableVar mgr (lpOverlapped oe)) $ \tbl ->
                   IT.delete (lpoverlappedToInt (lpOverlapped oe)) tbl
          case mCD of
            Nothing                        -> return ()
            Just (CompletionData _fptr cb) -> do
                         status <- FFI.overlappedIOStatus (lpOverlapped oe)
                         cb status (dwNumberOfBytesTransferred oe)

      -- clear the array so we don't erronously interpret the output, in
      -- certain circumstances like lockFileEx the code could return 1 entry
      -- removed but the file data not been filled in.
      -- TODO: Maybe not needed..
      A.clear mgrOverlappedEntries

      -- Check to see if we received the maximum amount of entried we could
      -- this likely indicates a high number of I/O requests have been queued.
      -- In which case we should process more at a time.
      cap <- A.capacity mgrOverlappedEntries
      when (cap == n) $ A.ensureCapacity mgrOverlappedEntries (2*cap)

    -- Keep running if we did some work just now or if we have a pending delay.
    return (n > 0 || isJust delay, delay)

io_mngr_loop :: HANDLE -> Manager -> IO ()
io_mngr_loop event mgr = go
    where
      go = do (more, delay) <- step mgr
              debugIO "I/O manager stepping."
              r2 <- c_readIOManagerEvent
              exit <-
                    case r2 of
                      _ | r2 == io_MANAGER_WAKEUP -> return False
                      _ | r2 == io_MANAGER_DIE    -> return True
                      0 -> return False -- spurious wakeup
                      _ -> do start_console_handler (r2 `shiftR` 1)
                              return False
              -- If we have no more work to do, or something from the outside
              -- told us to stop then we let the thread die and stop the I/O
              -- manager.  It will be woken up again when there is more to do.
              case () of
                _ | exit -> debugIO "I/O manager shutting down."
                _ | isJust delay -> do
                      let timeout = fromTimeout delay
                      debugIO "I/O manager pausing."
                      r <- c_WaitForSingleObject event timeout
                      when (r == 0xffffffff) $ throwGetLastError "io_mngr_loop"
                      go
                _ | more -> go -- We seem to have more work but no ETA for it.
                               -- So just retry until we run out of work.
                _ -> do
                  debugIO "I/O manager deep sleep."
                  r <- c_WaitForSingleObject event 0xFFFFFFFF
                  when (r == 0xffffffff) $ throwGetLastError "io_mngr_loop"
                  go

-- TODO: Do some refactoring to share this between here and GHC.Conc.POSIX
-- must agree with rts/win32/ThrIOManager.c
io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = 0xffffffff
io_MANAGER_DIE    = 0xfffffffe

data ConsoleEvent
 = ControlC
 | Break
 | Close
    -- these are sent to Services only.
 | Logoff
 | Shutdown
 deriving ( Eq   -- ^ @since 4.3.0.0
          , Ord  -- ^ @since 4.3.0.0
          , Enum -- ^ @since 4.3.0.0
          , Show -- ^ @since 4.3.0.0
          , Read -- ^ @since 4.3.0.0
          )

start_console_handler :: Word32 -> IO ()
start_console_handler r =
  case toWin32ConsoleEvent r of
     Just x  -> withMVar win32ConsoleHandler $ \handler -> do
                    _ <- forkIO (handler x)
                    return ()
     Nothing -> return ()

toWin32ConsoleEvent :: (Eq a, Num a) => a -> Maybe ConsoleEvent
toWin32ConsoleEvent ev =
   case ev of
       0 {- CTRL_C_EVENT-}        -> Just ControlC
       1 {- CTRL_BREAK_EVENT-}    -> Just Break
       2 {- CTRL_CLOSE_EVENT-}    -> Just Close
       5 {- CTRL_LOGOFF_EVENT-}   -> Just Logoff
       6 {- CTRL_SHUTDOWN_EVENT-} -> Just Shutdown
       _ -> Nothing

win32ConsoleHandler :: MVar (ConsoleEvent -> IO ())
win32ConsoleHandler = unsafePerformIO (newMVar (errorWithoutStackTrace "win32ConsoleHandler"))

wakeupIOManager :: IO ()
wakeupIOManager
  = do mngr <- getSystemManager
       event <- c_getIOManagerEvent
       debugIO "waking up I/O manager."
       case mngr of
         Nothing  -> error "cannot happen."
         Just mgr -> startIOManagerThread (io_mngr_loop event mgr)


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

