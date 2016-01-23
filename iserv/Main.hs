{-# LANGUAGE RecordWildCards, GADTs, ScopedTypeVariables, RankNTypes, CPP #-}
module Main (main) where

import GHCi.Run
import GHCi.TH
import GHCi.Message
import GHCi.Signals

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import Data.IORef
import System.Environment
import System.Exit
#ifdef mingw32_HOST_OS
import GHC.IO.Handle.FD (fdToHandle)
import Foreign.C
#else
import System.Posix
#endif
import Text.Printf

#ifdef mingw32_HOST_OS
_O_BINARY :: CInt
_O_BINARY = 0x8000 
#endif

main :: IO ()
main = do
  (arg0:arg1:rest) <- getArgs
  let wfd1 = read arg0; rfd2 = read arg1
  verbose <- case rest of
    ["-v"] -> return True
    []     -> return False
    _      -> die "iserv: syntax: iserv <write-fd> <read-fd> [-v]"
  when verbose $ do
    printf "GHC iserv starting (in: %d; out: %d)\n"
      (fromIntegral rfd2 :: Int) (fromIntegral wfd1 :: Int)
#ifdef mingw32_HOST_OS
  rfd2' <- _open_osfhandle rfd2 _O_BINARY 
  wfd1' <- _open_osfhandle wfd1 _O_BINARY
#else
  let rfd2' = rfd2 
      wfd1' = wfd1
#endif
  inh   <- fdToHandle rfd2'
  outh  <- fdToHandle wfd1'
  installSignalHandlers
  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
  uninterruptibleMask $ serv verbose pipe
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.
     
#ifdef mingw32_HOST_OS
foreign import ccall "io.h _open_osfhandle" _open_osfhandle ::
    CInt -> CInt -> IO CInt
#endif

serv :: Bool -> Pipe -> (forall a .IO a -> IO a) -> IO ()
serv verbose pipe@Pipe{..} restore = loop
 where
  loop = do
    Msg msg <- readPipe pipe getMessage
    discardCtrlC
    when verbose $ putStrLn ("iserv: " ++ show msg)
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> wrapRunTH $ runTH pipe st q ty loc
      FinishTH st -> wrapRunTH $ finishTH pipe st
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    when verbose $ putStrLn ("iserv: return: " ++ show r)
    writePipe pipe (put r)
    loop

  wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
  wrapRunTH io = do
    r <- try io
    case r of
      Left e
        | Just (GHCiQException _ err) <- fromException e  -> do
           when verbose $ putStrLn "iserv: QFail"
           writePipe pipe (putMessage (QFail err))
           loop
        | otherwise -> do
           when verbose $ putStrLn "iserv: QException"
           str <- showException e
           writePipe pipe (putMessage (QException str))
           loop
      Right a -> do
        when verbose $ putStrLn "iserv: QDone"
        writePipe pipe (putMessage QDone)
        reply a

  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
  showException :: SomeException -> IO String
  showException e0 = do
     r <- try $ evaluate (force (show (e0::SomeException)))
     case r of
       Left e -> showException e
       Right str -> return str

  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
  discardCtrlC = do
    r <- try $ restore $ return ()
    case r of
      Left UserInterrupt -> return () >> discardCtrlC
      Left e -> throwIO e
      _ -> return ()
