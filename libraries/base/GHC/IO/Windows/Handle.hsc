{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Windows.Handle
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on Windows Handles
--
-----------------------------------------------------------------------------

module GHC.IO.Windows.Handle
 ( -- * Basic Types
   NativeHandle(),
   ConsoleHandle(),
   IoHandle(),
   HANDLE,
   Io(),

   -- * Utility functions
   convertHandle,
   toHANDLE,

   -- * Standard Handles
   stdin,
   stdout,
   stderr,

   -- * File utilities
   openFile,
   release
 ) where

#include <windows.h>
#include <ntstatus.h>
##include "windows_cconv.h"

import Data.Bits ((.|.), shiftL)
import Data.Word (Word8, Word16, Word64)
import Data.Functor ((<$>))
import Data.Typeable

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real

import GHC.IO
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..), IODevice(), devType, setSize)
import GHC.IO.Exception
import GHC.IO.Unsafe
import GHC.IO.IOMode
import GHC.IO.Windows.Encoding (withGhcInternalToUTF16, withUTF16ToGhcInternal)
import GHC.IO.Windows.Paths (getDevicePath)
import GHC.IO.Handle.Internals (debugIO)
import GHC.Event.Windows (LPOVERLAPPED, withOverlapped, IOResult(..))
import GHC.Event.Windows.FFI (overlappedIOStatus)
import Foreign.Ptr
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with, fromBool)
import Foreign.Storable (Storable (..))
import qualified GHC.Event.Windows as Mgr

import GHC.Windows (LPVOID, LPDWORD, DWORD, HANDLE, BOOL, LPCTSTR, ULONG,
                    failIf, iNVALID_HANDLE_VALUE, failIf_, failWith,
                    failIfFalse_, getLastError)
import qualified GHC.Windows as Win32
import Text.Show

-- -----------------------------------------------------------------------------
-- The Windows IO device handles

data NativeHandle
data ConsoleHandle

data IoHandle a where
  NativeHandle  :: { getNativeHandle  :: HANDLE } -> IoHandle NativeHandle
  ConsoleHandle :: { getConsoleHandle :: HANDLE } -> IoHandle ConsoleHandle

type Io a = IoHandle a

-- | Convert a ConsoleHandle into a general FileHandle
--   This will change which DeviceIO is used.
convertHandle :: Io ConsoleHandle -> Io NativeHandle
convertHandle = fromHANDLE . toHANDLE

-- | @since 4.11.0.0
instance Show (Io NativeHandle) where
  show = show . toHANDLE

-- | @since 4.11.0.0
instance Show (Io ConsoleHandle) where
  show = show . getConsoleHandle

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO (Io NativeHandle) where
  read             = hwndRead
  readNonBlocking  = hwndReadNonBlocking
  write            = hwndWrite
  writeNonBlocking = hwndWriteNonBlocking

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO (Io ConsoleHandle) where
  read             = consoleRead
  readNonBlocking  = consoleReadNonBlocking
  write            = consoleWrite
  writeNonBlocking = consoleWriteNonBlocking

-- | Generalize a way to get and create handles.
class (GHC.IO.Device.RawIO a, IODevice a, BufferedIO a, Typeable a)
      => RawHandle a where
  toHANDLE   :: a -> HANDLE
  fromHANDLE :: HANDLE -> a
  isLockable :: a -> Bool

instance RawHandle (Io NativeHandle) where
  toHANDLE     = getNativeHandle
  fromHANDLE   = NativeHandle
  isLockable _ = True

instance RawHandle (Io ConsoleHandle) where
  toHANDLE     = getConsoleHandle
  fromHANDLE   = ConsoleHandle
  isLockable _ = False

-- -----------------------------------------------------------------------------
-- The Windows IO device implementation

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice (Io NativeHandle) where
  ready      = handle_ready
  close      = handle_close
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_seek
  tell       = handle_tell
  getSize    = handle_get_size
  setSize    = handle_set_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = handle_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice (Io ConsoleHandle) where
  ready      = handle_ready
  close      = handle_close . convertHandle
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_console_seek
  tell       = handle_console_tell
  getSize    = handle_get_console_size
  setSize    = handle_set_console_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = handle_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- Default sequential read buffer size.
-- for Windows 8k seems to be the optimal
-- buffer size.
dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = 8192

-- | @since 4.11.0.0
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO (Io NativeHandle) where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf'
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf'
  flushWriteBuffer0    = writeBufNonBlocking

-- | @since 4.11.0.0
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO (Io ConsoleHandle) where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf'
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf'
  flushWriteBuffer0    = writeBufNonBlocking


readBuf' :: RawHandle a => a -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf' hnd buf = do
  debugIO ("readBuf handle=" ++ show (toHANDLE hnd) ++ " " ++
           summaryBuffer buf ++ "\n")
  (r,buf') <- readBuf hnd buf
  debugIO ("after: " ++ summaryBuffer buf' ++ "\n")
  return (r,buf')

writeBuf' :: RawHandle a => a -> Buffer Word8 -> IO (Buffer Word8)
writeBuf' hnd buf = do
  debugIO ("writeBuf handle=" ++ show (toHANDLE hnd) ++ " " ++
           summaryBuffer buf ++ "\n")
  writeBuf hnd buf

-- -----------------------------------------------------------------------------
-- Standard I/O handles

type StdHandleId  = DWORD

#{enum StdHandleId,
 , sTD_INPUT_HANDLE  = STD_INPUT_HANDLE
 , sTD_OUTPUT_HANDLE = STD_OUTPUT_HANDLE
 , sTD_ERROR_HANDLE  = STD_ERROR_HANDLE
}

getStdHandle :: StdHandleId -> IO HANDLE
getStdHandle hid =
  failIf (== iNVALID_HANDLE_VALUE) "GetStdHandle" $ c_GetStdHandle hid

stdin, stdout, stderr :: Io ConsoleHandle
stdin  = unsafePerformIO $ ConsoleHandle <$> getStdHandle sTD_INPUT_HANDLE
stdout = unsafePerformIO $ ConsoleHandle <$> getStdHandle sTD_OUTPUT_HANDLE
stderr = unsafePerformIO $ ConsoleHandle <$> getStdHandle sTD_ERROR_HANDLE

-- -----------------------------------------------------------------------------
-- Some console internal types to detect EOF.

-- ASCII Ctrl+D (EOT) character.  Typically used by Unix consoles.
-- use for cross platform compatibility and to adhere to the ASCII standard.
acCtrlD = 0x04
-- ASCII Ctrl+Z (SUB) character. Typically used by Windows consoles to denote
-- EOT.  Use for compatibility with user expectations.
acCtrlZ = 0x1A

-- Mask to use to trigger ReadConsole input processing end.
acEotMask = (1 `shiftL` acCtrlD) .|. (1 `shiftL` acCtrlZ)

-- Structure to hold the control character masks
type PCONSOLE_READCONSOLE_CONTROL = Ptr CONSOLE_READCONSOLE_CONTROL
data CONSOLE_READCONSOLE_CONTROL = CONSOLE_READCONSOLE_CONTROL
  { crcNLength           :: ULONG
  , crcNInitialChars     :: ULONG
  , crcDwCtrlWakeupMask  :: ULONG
  , crcDwControlKeyState :: ULONG
  } deriving Show

instance Storable CONSOLE_READCONSOLE_CONTROL where
  sizeOf = const #size CONSOLE_READCONSOLE_CONTROL
  alignment = const #alignment CONSOLE_READCONSOLE_CONTROL
  poke buf crc = do
    (#poke CONSOLE_READCONSOLE_CONTROL, nLength)           buf
        (crcNLength           crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, nInitialChars)     buf
        (crcNInitialChars     crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask)  buf
        (crcDwCtrlWakeupMask  crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, dwControlKeyState) buf
        (crcDwControlKeyState crc)

  peek buf = do
    vNLength           <-
      (#peek CONSOLE_READCONSOLE_CONTROL, nLength)           buf
    vNInitialChars     <-
      (#peek CONSOLE_READCONSOLE_CONTROL, nInitialChars)     buf
    vDwCtrlWakeupMask  <-
      (#peek CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask)  buf
    vDwControlKeyState <-
      (#peek CONSOLE_READCONSOLE_CONTROL, dwControlKeyState) buf
    return $ CONSOLE_READCONSOLE_CONTROL {
        crcNLength           = vNLength,
        crcNInitialChars     = vNInitialChars,
        crcDwCtrlWakeupMask  = vDwCtrlWakeupMask,
        crcDwControlKeyState = vDwControlKeyState
      }

-- Create CONSOLE_READCONSOLE_CONTROL for breaking on control characters
-- specified by acEotMask
eotControl :: CONSOLE_READCONSOLE_CONTROL
eotControl =
  CONSOLE_READCONSOLE_CONTROL
    { crcNLength           = fromIntegral $
                               sizeOf (undefined :: CONSOLE_READCONSOLE_CONTROL)
    , crcNInitialChars     = 0
    , crcDwCtrlWakeupMask  = acEotMask
    , crcDwControlKeyState = 0
    }

-- -----------------------------------------------------------------------------
-- Foreign imports


foreign import WINDOWS_CCONV safe "windows.h CreateFileW"
    c_CreateFile :: LPCTSTR -> DWORD -> DWORD -> LPSECURITY_ATTRIBUTES
                 -> DWORD -> DWORD -> HANDLE
                 -> IO HANDLE

foreign import WINDOWS_CCONV safe "windows.h ReadFile"
    c_ReadFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
               -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h WriteFile"
    c_WriteFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
                -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h GetStdHandle"
    c_GetStdHandle :: StdHandleId -> IO HANDLE

foreign import ccall safe "__handle_ready"
    c_handle_ready :: HANDLE -> BOOL -> CInt -> IO CInt

foreign import ccall safe "__is_console"
    c_is_console :: HANDLE -> IO BOOL

foreign import ccall safe "__set_console_buffering"
    c_set_console_buffering :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__set_console_echo"
    c_set_console_echo :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__get_console_echo"
    c_get_console_echo :: HANDLE -> IO BOOL

foreign import ccall safe "__close_handle"
    c_close_handle :: HANDLE -> IO Bool

foreign import ccall safe "__handle_type"
    c_handle_type :: HANDLE -> IO Int

foreign import ccall safe "__set_file_pointer"
  c_set_file_pointer :: HANDLE -> CLong -> DWORD -> Ptr CLong -> IO BOOL

foreign import ccall safe "__get_file_pointer"
  c_get_file_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_file_size"
  c_get_file_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_file_size"
  c_set_file_size :: HANDLE -> CLong -> IO BOOL

foreign import ccall safe "__duplicate_handle"
  c_duplicate_handle :: HANDLE -> Ptr HANDLE -> IO BOOL

foreign import ccall safe "__set_console_pointer"
  c_set_console_pointer :: HANDLE -> CLong -> DWORD -> Ptr CLong -> IO BOOL

foreign import ccall safe "__get_console_pointer"
  c_get_console_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_console_buffer_size"
  c_get_console_buffer_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_console_buffer_size"
  c_set_console_buffer_size :: HANDLE -> CLong -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h ReadConsoleW"
  c_read_console :: HANDLE -> Ptr Word16 -> DWORD -> Ptr DWORD
                 -> PCONSOLE_READCONSOLE_CONTROL -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h WriteConsoleW"
  c_write_console :: HANDLE -> Ptr Word16 -> DWORD -> Ptr DWORD -> Ptr ()
                  -> IO BOOL

type LPSECURITY_ATTRIBUTES = LPVOID

-- -----------------------------------------------------------------------------
-- Reading and Writing

-- For this to actually block, the file handle must have
-- been created with FILE_FLAG_OVERLAPPED not set.
hwndRead :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
hwndRead hwnd ptr offset bytes
  = fmap fromIntegral $ Mgr.withException "hwndRead" $
      withOverlapped "hwndRead" (toHANDLE hwnd) offset (startCB ptr) completionCB
  where
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndRead"
      ret <- c_ReadFile (toHANDLE hwnd) (castPtr outBuf)
                        (fromIntegral bytes) nullPtr lpOverlapped
      err <- Win32.getLastError
      let err' = fromIntegral err

      case () of
        _ | err == #{const ERROR_IO_PENDING} -> return Mgr.CbPending
          | err == #{const ERROR_HANDLE_EOF} -> return Mgr.CbDone
          | not ret                          -> return (Mgr.CbError err')
          | otherwise -> do
              success <- overlappedIOStatus lpOverlapped
          -- Check to see if the operation was completed on a
          -- non-overlapping handle. e.g. stdio redirection or similar.
              if success == #{const ERROR_SUCCESS}
                  then return Mgr.CbDone
                  else return (Mgr.CbError err')

    completionCB err dwBytes
      | err == #{const ERROR_SUCCESS}      = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == #{const ERROR_HANDLE_EOF}   = Mgr.ioSuccess 0
      | err == #{const STATUS_END_OF_FILE} = Mgr.ioSuccess 0
      | err == #{const ERROR_MORE_DATA}    = Mgr.ioSuccess $ fromIntegral dwBytes
      | otherwise                          = Mgr.ioFailed err

-- There's no non-blocking file I/O on Windows I think..
-- But sockets etc should be possible.
-- Revisit this when implementing sockets and pipes.
hwndReadNonBlocking :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int
                    -> IO (Maybe Int)
hwndReadNonBlocking hwnd ptr offset bytes
  = do val <- withOverlapped "hwndReadNonBlocking" (toHANDLE hwnd) offset
                              (startCB ptr) completionCB
       return $ Just $ fromIntegral $ ioValue val
  where
    startCB inputBuf lpOverlapped = do
      debugIO ":: hwndReadNonBlocking"
      ret <- c_ReadFile (toHANDLE hwnd) (castPtr inputBuf)
                        (fromIntegral bytes) nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError
      case () of
        _ | err == #{const ERROR_IO_PENDING} -> return Mgr.CbPending
          | err == #{const ERROR_HANDLE_EOF} -> return Mgr.CbDone
          | not ret                          -> return (Mgr.CbError err)
          | otherwise -> do
              success <- overlappedIOStatus lpOverlapped
          -- Check to see if the operation was completed on a
          -- non-overlapping handle. e.g. stdio redirection or similar.
              if success == #{const ERROR_SUCCESS}
                  then return Mgr.CbDone
                  else return (Mgr.CbError err)

    completionCB err dwBytes
      | err == #{const ERROR_SUCCESS}      = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == #{const ERROR_HANDLE_EOF}   = Mgr.ioSuccess 0
      | err == #{const STATUS_END_OF_FILE} = Mgr.ioSuccess 0
      | otherwise                          = Mgr.ioFailed err

hwndWrite :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO ()
hwndWrite hwnd ptr offset bytes
  = do _ <- Mgr.withException "hwndWrite" $
          withOverlapped "hwndWrite" (toHANDLE hwnd) offset (startCB ptr)
                         completionCB
       return ()
  where
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndWrite"
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError

      case () of
        _ | err == #{const ERROR_SUCCESS}    -> return Mgr.CbDone
          | ret                              -> return Mgr.CbPending
          | err == #{const ERROR_IO_PENDING} -> return Mgr.CbPending
          | otherwise                        -> return (Mgr.CbError err)

    completionCB err dwBytes
        | err == #{const ERROR_SUCCESS}  =   Mgr.ioSuccess $ fromIntegral dwBytes
        | err == #{const ERROR_HANDLE_EOF} = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise                        = Mgr.ioFailed err

hwndWriteNonBlocking :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
hwndWriteNonBlocking hwnd ptr offset bytes
  = do val <- withOverlapped "hwndReadNonBlocking" (toHANDLE hwnd) offset
                             (startCB ptr) completionCB
       return $ fromIntegral $ ioValue val
  where
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndWriteNonBlocking"
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError

      case () of
        _ | err == #{const ERROR_SUCCESS}    -> return Mgr.CbDone
          | ret                              -> return Mgr.CbPending
          | err == #{const ERROR_IO_PENDING} -> return Mgr.CbPending
          | otherwise                        -> return (Mgr.CbError err)

    completionCB err dwBytes
        | err == #{const ERROR_SUCCESS}    = Mgr.ioSuccess $ fromIntegral dwBytes
        | err == #{const ERROR_HANDLE_EOF} = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise                        = Mgr.ioFailed err

consoleWrite :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO ()
consoleWrite hwnd ptr _offset bytes
  = alloca $ \res ->
      do failIfFalse_ "GHC.IO.Handle.consoleWrite" $ do
           debugIO ":: consoleWrite"
           withGhcInternalToUTF16 ptr bytes $ \(w_ptr, w_len) -> do
              success <- c_write_console (toHANDLE hwnd) w_ptr
                                         (fromIntegral w_len) res nullPtr
              if not success
                 then return False
                 else do val <- fromIntegral <$> peek res
                         return $ val==w_len

consoleWriteNonBlocking :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
consoleWriteNonBlocking hwnd ptr _offset bytes
  = alloca $ \res ->
      do failIfFalse_ "GHC.IO.Handle.consoleWriteNonBlocking" $ do
            debugIO ":: consoleWriteNonBlocking"
            withGhcInternalToUTF16 ptr bytes $ \(w_ptr, w_len) -> do
              c_write_console (toHANDLE hwnd) w_ptr (fromIntegral w_len)
                              res nullPtr
         val <- fromIntegral <$> peek res
         return val

consoleRead :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
consoleRead hwnd ptr _offset bytes
  = withUTF16ToGhcInternal ptr bytes $ \reqBytes w_ptr ->
      alloca $ \res ->
       do -- eotControl allows us to handle control characters like EOL
          -- without needing a newline, which would sort of defeat the point
          -- of an EOL.
          res_code <- with eotControl $ \p_eotControl ->
                c_read_console (toHANDLE hwnd) w_ptr (fromIntegral reqBytes) res
                               p_eotControl

          -- Restore a quirk of the POSIX read call, which only returns a fail
          -- when the handle is invalid, e.g. closed or not a handle.  It how-
          -- ever returns 0 when the handle is valid but unreadable, such as
          -- passing a handle with no GENERIC_READ permission, like /dev/null
          err <- getLastError
          when (not res_code) $
            case () of
             _ | err == #{const ERROR_INVALID_FUNCTION} -> return ()
               | otherwise -> failWith "GHC.IO.Handle.consoleRead" err
          b_read <- fromIntegral <$> peek res
          if b_read /= 1
              then return b_read
              else do w_first <- peekElemOff w_ptr 0
                      case () of
                        -- Handle Ctrl+Z which is the actual EOL sequence on
                        -- windows, but also hanlde Ctrl+D which is what the
                        -- ASCII standard defines as EOL.
                        _ | w_first == fromIntegral acCtrlD -> return 0
                          | w_first == fromIntegral acCtrlZ -> return 0
                          | otherwise                       -> return b_read


consoleReadNonBlocking :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int
                       -> IO (Maybe Int)
consoleReadNonBlocking hwnd ptr offset bytes
  = Just <$> consoleRead hwnd ptr offset bytes

-- -----------------------------------------------------------------------------
-- Operations on file handles

handle_ready :: RawHandle a => a -> Bool -> Int -> IO Bool
handle_ready hwnd write msecs = do
  r <- throwErrnoIfMinus1Retry "GHC.IO.Windows.Handle.handle_ready" $
          c_handle_ready (toHANDLE hwnd) write (fromIntegral msecs)
  return (toEnum (fromIntegral r))

handle_is_console :: RawHandle a => a -> IO Bool
handle_is_console = c_is_console . toHANDLE

handle_close :: RawHandle a => a -> IO ()
handle_close h = do release h
                    failIfFalse_ "handle_close" $ c_close_handle (toHANDLE h)

handle_dev_type :: RawHandle a => a -> IO IODeviceType
handle_dev_type hwnd = do _type <- c_handle_type $ toHANDLE hwnd
                          return $ case _type of
                                     _ | _type == 3 -> Stream
                                       | _type == 5 -> RawDevice
                                       | otherwise  -> RegularFile

handle_is_seekable :: RawHandle a => a -> IO Bool
handle_is_seekable hwnd = do
  t <- handle_dev_type hwnd
  return (t == RegularFile || t == RawDevice)

handle_seek :: RawHandle a => a -> SeekMode -> Integer -> IO Integer
handle_seek hwnd mode off =
  with 0 $ \off_rel -> do
    failIfFalse_ "GHC.IO.Handle.handle_seek" $
        c_set_file_pointer (toHANDLE hwnd) (fromIntegral off) seektype off_rel
    fromIntegral <$> peek off_rel
 where
    seektype :: DWORD
    seektype = case mode of
                   AbsoluteSeek -> #{const FILE_BEGIN}
                   RelativeSeek -> #{const FILE_CURRENT}
                   SeekFromEnd  -> #{const FILE_END}

handle_tell :: RawHandle a => a -> IO Integer
handle_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_tell" $
          c_get_file_pointer (toHANDLE hwnd))

handle_set_size :: RawHandle a => a -> Integer -> IO ()
handle_set_size hwnd size =
  failIfFalse_ "GHC.IO.Handle.handle_set_size" $
      c_set_file_size (toHANDLE hwnd) (fromIntegral size)

handle_get_size :: RawHandle a => a -> IO Integer
handle_get_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_set_size" $
          c_get_file_size (toHANDLE hwnd))

handle_set_echo :: RawHandle a => a -> Bool -> IO ()
handle_set_echo hwnd value =
  failIfFalse_ "GHC.IO.Handle.handle_set_echo" $
      c_set_console_echo (toHANDLE hwnd) value

handle_get_echo :: RawHandle a => a -> IO Bool
handle_get_echo = c_get_console_echo . toHANDLE

handle_duplicate :: RawHandle a => a -> IO a
handle_duplicate hwnd = alloca $ \ptr -> do
  failIfFalse_ "GHC.IO.Handle.handle_duplicate" $
      c_duplicate_handle (toHANDLE hwnd) ptr
  fromHANDLE <$> peek ptr

handle_set_buffering :: RawHandle a => a -> Bool -> IO ()
handle_set_buffering hwnd value =
  failIfFalse_ "GHC.IO.Handle.handle_set_buffering" $
      c_set_console_buffering (toHANDLE hwnd) value

handle_console_seek :: RawHandle a => a -> SeekMode -> Integer -> IO Integer
handle_console_seek hwnd mode off =
  with 0 $ \loc_ptr -> do
    failIfFalse_ "GHC.IO.Handle.handle_console_seek" $
      c_set_console_pointer (toHANDLE hwnd) (fromIntegral off) seektype loc_ptr
    fromIntegral <$> peek loc_ptr
 where
    seektype :: DWORD
    seektype = case mode of
                 AbsoluteSeek -> #{const FILE_BEGIN}
                 RelativeSeek -> #{const FILE_CURRENT}
                 SeekFromEnd  -> #{const FILE_END}

handle_console_tell :: RawHandle a => a -> IO Integer
handle_console_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_console_tell" $
          c_get_console_pointer (toHANDLE hwnd))

handle_set_console_size :: RawHandle a => a -> Integer -> IO ()
handle_set_console_size hwnd size =
  failIfFalse_ "GHC.IO.Handle.handle_set_console_size" $
      c_set_console_buffer_size (toHANDLE hwnd) (fromIntegral size)

handle_get_console_size :: RawHandle a => a -> IO Integer
handle_get_console_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_get_console_size" $
          c_get_console_buffer_size (toHANDLE hwnd))

-- -----------------------------------------------------------------------------
-- opening files

-- | Open a file and make an 'FD' for it.  Truncates the file to zero
-- size when the `IOMode` is `WriteMode`.
openFile
  :: FilePath -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
  -> IO (Io NativeHandle, IODeviceType)
openFile filepath iomode non_blocking =
   do devicepath <- getDevicePath filepath
      h <- createFile devicepath
      Mgr.associateHandle' h
      let hwnd = fromHANDLE h
      _type <- devType hwnd

      -- Use the rts to enforce any file locking we may need.
      let write_lock = iomode /= ReadMode

      case _type of
        -- Regular files need to be locked.
        RegularFile -> do
          (unique_dev, unique_ino) <- getUniqueFileInfo hwnd
          r <- lockFile (fromIntegral $ ptrToWordPtr h) unique_dev unique_ino
                        (fromBool write_lock)
          when (r == -1)  $
               ioException (IOError Nothing ResourceBusy "openFile"
                                  "file is locked" Nothing Nothing)

        -- I don't see a reason for blocking directories.  So unlike the FD
        -- implementation I'll allow it.
        _ -> return ()

      -- We want to truncate() if this is an open in WriteMode, but only
      -- if the target is a RegularFile.  but TRUNCATE_EXISTING would fail if
      -- the file didn't exit.  So just set the size afterwards.
      when (iomode == WriteMode && _type == RegularFile) $
        setSize hwnd 0

      return (hwnd, _type)
        where
          -- We have to use in-process locking (e.g. use the locking mechanism
          -- in the rts) so we're consistent with the linux behaviour and the
          -- rts knows about the lock.  See #4363 for more.
          file_share_mode =  #{const FILE_SHARE_READ}
                         .|. #{const FILE_SHARE_WRITE}
                         .|. #{const FILE_SHARE_DELETE}

          file_access_mode =
            case iomode of
              ReadMode      -> #{const GENERIC_READ}
              WriteMode     -> #{const GENERIC_WRITE}
              ReadWriteMode -> #{const GENERIC_READ}
                            .|. #{const GENERIC_WRITE}
              AppendMode    -> #{const GENERIC_WRITE}
                            .|. #{const FILE_APPEND_DATA}

          file_open_mode =
            case iomode of
              ReadMode      -> #{const OPEN_EXISTING} -- O_RDONLY
              WriteMode     -> #{const OPEN_ALWAYS}   -- O_CREAT | O_WRONLY | O_TRUNC
              ReadWriteMode -> #{const OPEN_ALWAYS}   -- O_CREAT | O_RDWR
              AppendMode    -> #{const OPEN_ALWAYS}   -- O_APPEND

          file_create_flags =
            if non_blocking
               then #{const FILE_FLAG_OVERLAPPED}
                    -- I beleive most haskell programs do sequential scans, so
                    -- optimize for the common case.  Though ideally, this would
                    -- be parameterized by openFile.  This will absolutely trash
                    -- the cache on reverse scans.
                    --
                    -- TODO: make a parameter to openFile and specify only for
                    -- operations we know are sequential.  This parameter should
                    -- be usable by madvise too.
                    .|. #{const FILE_FLAG_SEQUENTIAL_SCAN}
               else #{const FILE_ATTRIBUTE_NORMAL}

          createFile devicepath =
            withCWString devicepath $ \fp ->
                failIf (== iNVALID_HANDLE_VALUE) "CreateFile failed" $
                      c_CreateFile fp file_access_mode
                                      file_share_mode
                                      nullPtr
                                      file_open_mode
                                      file_create_flags
                                      nullPtr

release :: RawHandle a => a -> IO ()
release h = if isLockable h
               then do let handle = fromIntegral $ ptrToWordPtr $ toHANDLE h
                       _ <- unlockFile handle
                       return ()
               else return ()

-- -----------------------------------------------------------------------------
-- Locking/unlocking

foreign import ccall unsafe "lockFile"
  lockFile :: CUIntPtr -> Word64 -> Word64 -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CUIntPtr -> IO CInt

foreign import ccall unsafe "get_unique_file_info_hwnd"
  c_getUniqueFileInfo :: HANDLE -> Ptr Word64 -> Ptr Word64 -> IO ()

getUniqueFileInfo :: RawHandle a => a -> IO (Word64, Word64)
getUniqueFileInfo handle = do
  with 0 $ \devptr -> do
    with 0 $ \inoptr -> do
      c_getUniqueFileInfo (toHANDLE handle) devptr inoptr
      liftM2 (,) (peek devptr) (peek inoptr)