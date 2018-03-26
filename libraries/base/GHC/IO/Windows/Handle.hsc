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

   -- * Utility functions
   convertHandle,

   -- * Standard Handles
   stdin,
   stdout,
   stderr,

   -- * File utilities
   openFile
 ) where

#include <windows.h>
#include <ntstatus.h>
##include "windows_cconv.h"

import Data.Bits ((.|.))
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
import GHC.IO.Device (SeekMode(..), IODeviceType(..), IODevice(), devType)
import GHC.IO.Unsafe
import GHC.IO.IOMode
import GHC.IO.Windows.Encoding (withGhcInternalToUTF16, withUTF16ToGhcInternal)
import GHC.IO.Handle.Internals (debugIO)
import GHC.Event.Windows (LPOVERLAPPED, withOverlapped, IOResult(..))
import Foreign.Ptr
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import qualified GHC.Event.Windows as Mgr

import GHC.Windows (LPVOID, LPDWORD, DWORD, HANDLE, BOOL, LPCTSTR,
                    failIf, iNVALID_HANDLE_VALUE, failIf_, failWith)
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
class (IODevice a, BufferedIO a, Typeable a) => RawHandle a where
  toHANDLE   :: a -> HANDLE
  fromHANDLE :: HANDLE -> a

instance RawHandle (Io NativeHandle) where
  toHANDLE   = getNativeHandle
  fromHANDLE = NativeHandle

instance RawHandle (Io ConsoleHandle) where
  toHANDLE   = getConsoleHandle
  fromHANDLE = ConsoleHandle

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
  fillReadBuffer       = readBuf
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf
  flushWriteBuffer0    = writeBufNonBlocking

-- | @since 4.11.0.0
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO (Io ConsoleHandle) where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf
  flushWriteBuffer0    = writeBufNonBlocking

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
-- Foreign imports


foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
    c_CreateFile :: LPCTSTR -> DWORD -> DWORD -> LPSECURITY_ATTRIBUTES
                 -> DWORD -> DWORD -> HANDLE
                 -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h ReadFile"
    c_ReadFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
               -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h WriteFile"
    c_WriteFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
                -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetStdHandle"
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
    c_close_handle :: HANDLE -> IO ()

foreign import ccall safe "__handle_type"
    c_handle_type :: HANDLE -> IO Int

foreign import ccall safe "__set_file_pointer"
  c_set_file_pointer :: HANDLE -> CLong -> DWORD -> IO BOOL

foreign import ccall safe "__get_file_pointer"
  c_get_file_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_file_size"
  c_get_file_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_file_size"
  c_set_file_size :: HANDLE -> CLong -> IO BOOL

foreign import ccall safe "__duplicate_handle"
  c_duplicate_handle :: HANDLE -> Ptr HANDLE -> IO BOOL

foreign import ccall safe "__set_console_pointer"
  c_set_console_pointer :: HANDLE -> CLong -> DWORD -> IO BOOL

foreign import ccall safe "__get_console_pointer"
  c_get_console_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_console_buffer_size"
  c_get_console_buffer_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_console_buffer_size"
  c_set_console_buffer_size :: HANDLE -> CLong -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h ReadConsoleW"
  c_read_console :: HANDLE -> Ptr Word16 -> DWORD -> Ptr DWORD -> Ptr ()
                 -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h WriteConsoleW"
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

      case ret of
        False -> return Nothing
        True  ->
          do err <- Win32.getLastError
             case () of
              _ | err == #{const ERROR_IO_PENDING} -> return Nothing
                | otherwise -> failWith "ReadFile failed" err

    completionCB err dwBytes
      | err == 0                           = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == #{const ERROR_HANDLE_EOF}   = Mgr.ioSuccess 0
      | err == #{const STATUS_END_OF_FILE} = Mgr.ioSuccess 0
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
      if not ret
        && (err == #{const ERROR_IO_PENDING}
            || err == #{const ERROR_HANDLE_EOF})
        then return Nothing
        else return (Just err)

    completionCB err dwBytes
      | err == 0                           = Mgr.ioSuccess $ fromIntegral dwBytes
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
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      when (not ret) $
            failIf_ (/= #{const ERROR_IO_PENDING}) "WriteFile failed" $
                    Win32.getLastError
      return Nothing

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

hwndWriteNonBlocking :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
hwndWriteNonBlocking hwnd ptr offset bytes
  = do val <- withOverlapped "hwndReadNonBlocking" (toHANDLE hwnd) offset
                             (startCB ptr) completionCB
       return $ fromIntegral $ ioValue val
  where
    startCB outBuf lpOverlapped = do
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      err <- fmap fromIntegral Win32.getLastError

      if not ret
        && (err == #{const ERROR_IO_PENDING}
            || err == #{const ERROR_HANDLE_EOF})
        then return Nothing
        else return (Just err)

    completionCB err dwBytes
        | err == 0  = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise = Mgr.ioFailed err

consoleWrite :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO ()
consoleWrite hwnd ptr _offset bytes
  = alloca $ \res ->
      do throwErrnoIf_ not "GHC.IO.Handle.consoleWrite" $
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
      do throwErrnoIf_ not "GHC.IO.Handle.consoleWriteNonBlocking" $
            withGhcInternalToUTF16 ptr bytes $ \(w_ptr, w_len) -> do
              c_write_console (toHANDLE hwnd) w_ptr (fromIntegral w_len)
                              res nullPtr
         val <- fromIntegral <$> peek res
         return val

consoleRead :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
consoleRead hwnd ptr _offset bytes
  = withUTF16ToGhcInternal ptr bytes $ \reqBytes w_ptr ->
      alloca $ \res ->
        do throwErrnoIf_ not "GHC.IO.Handle.consoleRead" $
             c_read_console (toHANDLE hwnd) w_ptr (fromIntegral reqBytes) res
                            nullPtr
           fromIntegral <$> peek res

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
handle_close = c_close_handle . toHANDLE

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

handle_seek :: RawHandle a => a -> SeekMode -> Integer -> IO ()
handle_seek hwnd mode off =
  throwErrnoIf_ not "GHC.IO.Handle.handle_seek" $
      c_set_file_pointer (toHANDLE hwnd) (fromIntegral off) seektype
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
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_size" $
      c_set_file_size (toHANDLE hwnd) (fromIntegral size)

handle_get_size :: RawHandle a => a -> IO Integer
handle_get_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_set_size" $
          c_get_file_size (toHANDLE hwnd))

handle_set_echo :: RawHandle a => a -> Bool -> IO ()
handle_set_echo hwnd value =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_echo" $
      c_set_console_echo (toHANDLE hwnd) value

handle_get_echo :: RawHandle a => a -> IO Bool
handle_get_echo = c_get_console_echo . toHANDLE

handle_duplicate :: RawHandle a => a -> IO a
handle_duplicate hwnd = alloca $ \ptr -> do
  throwErrnoIf_ not "GHC.IO.Handle.handle_duplicate" $
      c_duplicate_handle (toHANDLE hwnd) ptr
  fromHANDLE <$> peek ptr

handle_set_buffering :: RawHandle a => a -> Bool -> IO ()
handle_set_buffering hwnd value =
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_buffering" $
      c_set_console_buffering (toHANDLE hwnd) value

handle_console_seek :: RawHandle a => a -> SeekMode -> Integer -> IO ()
handle_console_seek hwnd mode off =
  throwErrnoIf_ not "GHC.IO.Handle.handle_console_seek" $
      c_set_console_pointer (toHANDLE hwnd) (fromIntegral off) seektype
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
  throwErrnoIf_ not "GHC.IO.Handle.handle_set_console_size" $
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
   do h <- createFile
      Mgr.associateHandle' h
      let hwnd = fromHANDLE h
      _type <- devType hwnd
      return (hwnd, _type)
        where
          createFile =
            withCWString filepath $ \fp ->
                failIf (== iNVALID_HANDLE_VALUE) "CreateFile failed" $
                      c_CreateFile fp #{const GENERIC_READ}
                                      #{const FILE_SHARE_READ}
                                      nullPtr
                                      #{const OPEN_EXISTING}
                                      (#{const FILE_FLAG_OVERLAPPED} .|. #{const FILE_FLAG_SEQUENTIAL_SCAN })
                                      nullPtr