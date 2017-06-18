{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle.Windows
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Handle operations implemented by Windows native handles
--
-----------------------------------------------------------------------------

module GHC.IO.Handle.Windows (
  stdin, stdout, stderr
 ) where

import GHC.Base
import GHC.Show
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import GHC.MVar
import GHC.IO
import GHC.IO.Encoding
import GHC.IO.Device as IODevice
import GHC.IO.Exception
import GHC.IO.IOMode
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.Windows.Handle as Win

-- ---------------------------------------------------------------------------
-- Standard Handles

-- Three handles are allocated during program initialisation.  The first
-- two manage input or output from the Haskell program's standard input
-- or output channel respectively.  The third manages output to the
-- standard error channel. These handles are initially open.

-- | A handle managing input from the Haskell program's standard input channel.
stdin :: Handle
{-# NOINLINE stdin #-}
stdin = unsafePerformIO $ do
   -- ToDo: acquire lock
   enc <- getLocaleEncoding
   mkHandle Win.stdin "<stdin>" ReadHandle True (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

-- | A handle managing output to the Haskell program's standard output channel.
stdout :: Handle
{-# NOINLINE stdout #-}
stdout = unsafePerformIO $ do
   -- ToDo: acquire lock
   enc <- getLocaleEncoding
   mkHandle Win.stdout "<stdout>" WriteHandle True (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

-- | A handle managing output to the Haskell program's standard error channel.
stderr :: Handle
{-# NOINLINE stderr #-}
stderr = unsafePerformIO $ do
    -- ToDo: acquire lock
   enc <- getLocaleEncoding
   mkHandle Win.stderr "<stderr>" WriteHandle False{-stderr is unbuffered-}
                (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of
      ClosedHandle -> return ()
      _other       -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)

-- ---------------------------------------------------------------------------
-- Opening and Closing Files

addFilePathToIOError :: String -> FilePath -> IOException -> IOException
addFilePathToIOError fun fp ioe
  = ioe{ ioe_location = fun, ioe_filename = Just fp }

-- | Computation 'openFile' @file mode@ allocates and returns a new, open
-- handle to manage the file @file@.  It manages input if @mode@
-- is 'ReadMode', output if @mode@ is 'WriteMode' or 'AppendMode',
-- and both input and output if mode is 'ReadWriteMode'.
--
-- If the file does not exist and it is opened for output, it should be
-- created as a new file.  If @mode@ is 'WriteMode' and the file
-- already exists, then it should be truncated to zero length.
-- Some operating systems delete empty files, so there is no guarantee
-- that the file will exist following an 'openFile' with @mode@
-- 'WriteMode' unless it is subsequently written to successfully.
-- The handle is positioned at the end of the file if @mode@ is
-- 'AppendMode', and otherwise at the beginning (in which case its
-- internal position is 0).
-- The initial buffer mode is implementation-dependent.
--
-- This operation may fail with:
--
--  * 'isAlreadyInUseError' if the file is already open and cannot be reopened;
--
--  * 'isDoesNotExistError' if the file does not exist; or
--
--  * 'isPermissionError' if the user does not have permission to open the file.
--
-- Note: if you will be working with files containing binary data, you'll want to
-- be using 'openBinaryFile'.
openFile :: FilePath -> IOMode -> IO Handle
openFile fp im =
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE True)
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but opens the file in ordinary blocking mode.
-- This can be useful for opening a FIFO for writing: if we open in
-- non-blocking mode then the open will fail if there are no readers,
-- whereas a blocking open will block until a reader appear.
--
-- @since 4.4.0.0
openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking fp im =
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE False)
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but open the file in binary mode.
-- On Windows, reading a file in text mode (which is the default)
-- will translate CRLF to LF, and writing will translate LF to CRLF.
-- This is usually what you want with text files.  With binary files
-- this is undesirable; also, as usual under Microsoft operating systems,
-- text mode treats control-Z as EOF.  Binary mode turns off all special
-- treatment of end-of-line and end-of-file characters.
-- (See also 'hSetBinaryMode'.)

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile fp m =
  catchException
    (openFile' fp m True True)
    (\e -> ioError (addFilePathToIOError "openBinaryFile" fp e))

openFile' :: String -> IOMode -> Bool -> Bool -> IO Handle
openFile' = undefined
{-}
openFile' filepath iomode binary non_blocking = do
  -- first open the file to get an FD
  (fd, fd_type) <- FD.openFile filepath iomode non_blocking

  mb_codec <- if binary then return Nothing else fmap Just getLocaleEncoding

  -- then use it to make a Handle
  mkHandleFromFD fd fd_type filepath iomode
                   False {- do not *set* non-blocking mode -}
                   mb_codec
            `onException` IODevice.close fd
        -- NB. don't forget to close the FD if mkHandleFromFD fails, otherwise
        -- this FD leaks.
        -- ASSERT: if we just created the file, then fdToHandle' won't fail
        -- (so we don't need to worry about removing the newly created file
        --  in the event of an error).
-}

-- ---------------------------------------------------------------------------
-- Converting file descriptors from/to Handles

mkHandleFromHANDLE
   :: (IODevice.IODevice dev, BufferedIO dev, Typeable dev) => dev
   -> IODeviceType
   -> FilePath  -- a string describing this file descriptor (e.g. the filename)
   -> IOMode
   -> Maybe TextEncoding
   -> IO Handle

mkHandleFromHANDLE dev hw_type filepath iomode mb_codec
  = do
    let nl | isJust mb_codec = nativeNewlineMode
           | otherwise       = noNewlineTranslation

    case hw_type of
        Directory ->
           ioException (IOError Nothing InappropriateType "openFile"
                           "is a directory" Nothing Nothing)

        Stream
           -- only *Streams* can be DuplexHandles.  Other read/write
           -- Handles must share a buffer.
           | ReadWriteMode <- iomode ->
                mkDuplexHandle dev filepath mb_codec nl


        _other ->
           mkFileHandle dev filepath iomode mb_codec nl

-- ---------------------------------------------------------------------------
-- Are files opened by default in text or binary mode, if the user doesn't
-- specify?

dEFAULT_OPEN_IN_BINARY_MODE :: Bool
dEFAULT_OPEN_IN_BINARY_MODE = False
