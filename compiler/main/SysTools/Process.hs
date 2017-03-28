{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Misc process handling code for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module SysTools.Process where

#include "HsVersions.h"

import Exception
import ErrUtils
import DynFlags
import FastString
import Outputable
import Panic
import Util
import SrcLoc           ( SrcLoc, mkSrcLoc, noSrcSpan, mkSrcSpan )

import Control.Concurrent
import Data.Char

import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Process

import SysTools.TempFiles

-- Similar to System.Process.readCreateProcessWithExitCode, but stderr is
-- inherited from the parent process, and output to stderr is not captured.
readCreateProcessWithExitCode'
    :: CreateProcess
    -> IO (ExitCode, String)    -- ^ stdout
readCreateProcessWithExitCode' proc = do
    (_, Just outh, _, pid) <-
        createProcess proc{ std_out = CreatePipe }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, output)

replaceVar :: (String, String) -> [(String, String)] -> [(String, String)]
replaceVar (var, value) env =
    (var, value) : filter (\(var',_) -> var /= var') env

-- | Version of @System.Process.readProcessWithExitCode@ that takes a
-- key-value tuple to insert into the environment.
readProcessEnvWithExitCode
    :: String -- ^ program path
    -> [String] -- ^ program args
    -> (String, String) -- ^ addition to the environment
    -> IO (ExitCode, String, String) -- ^ (exit_code, stdout, stderr)
readProcessEnvWithExitCode prog args env_update = do
    current_env <- getEnvironment
    readCreateProcessWithExitCode (proc prog args) {
        env = Just (replaceVar env_update current_env) } ""

-- Don't let gcc localize version info string, #8825
c_locale_env :: (String, String)
c_locale_env = ("LANGUAGE", "C")

-- If the -B<dir> option is set, add <dir> to PATH.  This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries (see bug #1110).
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
  if null b_dirs
     then return Nothing
     else do env <- getEnvironment
             return (Just (map mangle_path env))
 where
  (b_dirs, _) = partitionWith get_b_opt opts

  get_b_opt (Option ('-':'B':dir)) = Left dir
  get_b_opt other = Right other

  mangle_path (path,paths) | map toUpper path == "PATH"
        = (path, '\"' : head b_dirs ++ "\";" ++ paths)
  mangle_path other = other


-----------------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String          -- For -v message
             -> String          -- Command name (possibly a full path)
                                --      assumed already dos-ified
             -> [Option]        -- Arguments
                                --      runSomething will dos-ify them
             -> IO ()

runSomething dflags phase_name pgm args =
  runSomethingFiltered dflags id phase_name pgm args Nothing

-- | Run a command, placing the arguments in an external response file.
--
-- This command is used in order to avoid overlong command line arguments on
-- Windows. The command line arguments are first written to an external,
-- temporary response file, and then passed to the linker via @filepath.
-- response files for passing them in. See:
--
--     https://gcc.gnu.org/wiki/Response_Files
--     https://ghc.haskell.org/trac/ghc/ticket/10777
runSomethingResponseFile
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingResponseFile dflags filter_fn phase_name pgm args mb_env =
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        fp <- getResponseFile real_args
        let args = ['@':fp]
        r <- builderMainLoop dflags filter_fn pgm args mb_env
        return (r,())
  where
    getResponseFile args = do
      fp <- newTempName dflags "rsp"
      withFile fp WriteMode $ \h -> do
#if defined(mingw32_HOST_OS)
          hSetEncoding h latin1
#else
          hSetEncoding h utf8
#endif
          hPutStr h $ unlines $ map escape args
      return fp

    -- Note: Response files have backslash-escaping, double quoting, and are
    -- whitespace separated (some implementations use newline, others any
    -- whitespace character). Therefore, escape any backslashes, newlines, and
    -- double quotes in the argument, and surround the content with double
    -- quotes.
    --
    -- Another possibility that could be considered would be to convert
    -- backslashes in the argument to forward slashes. This would generally do
    -- the right thing, since backslashes in general only appear in arguments
    -- as part of file paths on Windows, and the forward slash is accepted for
    -- those. However, escaping is more reliable, in case somehow a backslash
    -- appears in a non-file.
    escape x = concat
        [ "\""
        , concatMap
            (\c ->
                case c of
                    '\\' -> "\\\\"
                    '\n' -> "\\n"
                    '\"' -> "\\\""
                    _    -> [c])
            x
        , "\""
        ]

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingFiltered dflags filter_fn phase_name pgm args mb_env = do
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        r <- builderMainLoop dflags filter_fn pgm real_args mb_env
        return (r,())

runSomethingWith
  :: DynFlags -> String -> String -> [Option]
  -> ([String] -> IO (ExitCode, a))
  -> IO a

runSomethingWith dflags phase_name pgm args io = do
  let real_args = filter notNull (map showOpt args)
      cmdLine = showCommandForUser pgm real_args
  traceCmd dflags phase_name cmdLine $ handleProc pgm phase_name $ io real_args

handleProc :: String -> String -> IO (ExitCode, r) -> IO r
handleProc pgm phase_name proc = do
    (rc, r) <- proc `catchIO` handler
    case rc of
      ExitSuccess{} -> return r
      ExitFailure n -> throwGhcExceptionIO (
            ProgramError ("`" ++ takeFileName pgm ++ "'" ++
                          " failed in phase `" ++ phase_name ++ "'." ++
                          " (Exit code: " ++ show n ++ ")"))
  where
    handler err =
       if IO.isDoesNotExistError err
          then does_not_exist
          else throwGhcExceptionIO (ProgramError $ show err)

    does_not_exist = throwGhcExceptionIO (InstallationError ("could not execute: " ++ pgm))


builderMainLoop :: DynFlags -> (String -> String) -> FilePath
                -> [String] -> Maybe [(String, String)]
                -> IO ExitCode
builderMainLoop dflags filter_fn pgm real_args mb_env = do
  chan <- newChan
  (hStdIn, hStdOut, hStdErr, hProcess) <- runInteractiveProcess pgm real_args Nothing mb_env

  -- and run a loop piping the output from the compiler to the log_action in DynFlags
  hSetBuffering hStdOut LineBuffering
  hSetBuffering hStdErr LineBuffering
  _ <- forkIO (readerProc chan hStdOut filter_fn)
  _ <- forkIO (readerProc chan hStdErr filter_fn)
  -- we don't want to finish until 2 streams have been completed
  -- (stdout and stderr)
  -- nor until 1 exit code has been retrieved.
  rc <- loop chan hProcess (2::Integer) (1::Integer) ExitSuccess
  -- after that, we're done here.
  hClose hStdIn
  hClose hStdOut
  hClose hStdErr
  return rc
  where
    -- status starts at zero, and increments each time either
    -- a reader process gets EOF, or the build proc exits.  We wait
    -- for all of these to happen (status==3).
    -- ToDo: we should really have a contingency plan in case any of
    -- the threads dies, such as a timeout.
    loop _    _        0 0 exitcode = return exitcode
    loop chan hProcess t p exitcode = do
      mb_code <- if p > 0
                   then getProcessExitCode hProcess
                   else return Nothing
      case mb_code of
        Just code -> loop chan hProcess t (p-1) code
        Nothing
          | t > 0 -> do
              msg <- readChan chan
              case msg of
                BuildMsg msg -> do
                  putLogMsg dflags NoReason SevInfo noSrcSpan
                     (defaultUserStyle dflags) msg
                  loop chan hProcess t p exitcode
                BuildError loc msg -> do
                  putLogMsg dflags NoReason SevError (mkSrcSpan loc loc)
                     (defaultUserStyle dflags) msg
                  loop chan hProcess t p exitcode
                EOF ->
                  loop chan hProcess (t-1) p exitcode
          | otherwise -> loop chan hProcess t p exitcode

readerProc :: Chan BuildMessage -> Handle -> (String -> String) -> IO ()
readerProc chan hdl filter_fn =
    (do str <- hGetContents hdl
        loop (linesPlatform (filter_fn str)) Nothing)
    `finally`
       writeChan chan EOF
        -- ToDo: check errors more carefully
        -- ToDo: in the future, the filter should be implemented as
        -- a stream transformer.
    where
        loop []     Nothing    = return ()
        loop []     (Just err) = writeChan chan err
        loop (l:ls) in_err     =
                case in_err of
                  Just err@(BuildError srcLoc msg)
                    | leading_whitespace l -> do
                        loop ls (Just (BuildError srcLoc (msg $$ text l)))
                    | otherwise -> do
                        writeChan chan err
                        checkError l ls
                  Nothing -> do
                        checkError l ls
                  _ -> panic "readerProc/loop"

        checkError l ls
           = case parseError l of
                Nothing -> do
                    writeChan chan (BuildMsg (text l))
                    loop ls Nothing
                Just (file, lineNum, colNum, msg) -> do
                    let srcLoc = mkSrcLoc (mkFastString file) lineNum colNum
                    loop ls (Just (BuildError srcLoc (text msg)))

        leading_whitespace []    = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (String, Int, Int, String)
parseError s0 = case breakColon s0 of
                Just (filename, s1) ->
                    case breakIntColon s1 of
                    Just (lineNum, s2) ->
                        case breakIntColon s2 of
                        Just (columnNum, s3) ->
                            Just (filename, lineNum, columnNum, s3)
                        Nothing ->
                            Just (filename, lineNum, 0, s2)
                    Nothing -> Nothing
                Nothing -> Nothing

breakColon :: String -> Maybe (String, String)
breakColon xs = case break (':' ==) xs of
                    (ys, _:zs) -> Just (ys, zs)
                    _ -> Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs = case break (':' ==) xs of
                       (ys, _:zs)
                        | not (null ys) && all isAscii ys && all isDigit ys ->
                           Just (read ys, zs)
                       _ -> Nothing

data BuildMessage
  = BuildMsg   !SDoc
  | BuildError !SrcLoc !SDoc
  | EOF

-- Divvy up text stream into lines, taking platform dependent
-- line termination into account.
linesPlatform :: String -> [String]
#if !defined(mingw32_HOST_OS)
linesPlatform ls = lines ls
#else
linesPlatform "" = []
linesPlatform xs =
  case lineBreak xs of
    (as,xs1) -> as : linesPlatform xs1
  where
   lineBreak "" = ("","")
   lineBreak ('\r':'\n':xs) = ([],xs)
   lineBreak ('\n':xs) = ([],xs)
   lineBreak (x:xs) = let (as,bs) = lineBreak xs in (x:as,bs)

#endif