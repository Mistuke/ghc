{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Tasks running external programs for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module SysTools.Tasks where

import Exception
import ErrUtils
import DynFlags
import Outputable
import Util

import Data.Char
import Data.List

import System.IO
import System.Process

import LlvmCodeGen.Base (llvmVersionStr, supportedLlvmVersion)

import SysTools.Process

{-
************************************************************************
*                                                                      *
\subsection{Running an external program}
*                                                                      *
************************************************************************
-}

runUnlit :: DynFlags -> [Option] -> IO ()
runUnlit dflags args = do
  let prog = pgm_L dflags
      opts = getOpts dflags opt_L
  runSomething dflags "Literate pre-processor" prog
               (map Option opts ++ args)

runCpp :: DynFlags -> [Option] -> IO ()
runCpp dflags args =   do
  let (p,args0) = pgm_P dflags
      args1 = map Option (getOpts dflags opt_P)
      args2 = [Option "-Werror" | gopt Opt_WarnIsError dflags]
                ++ [Option "-Wundef" | wopt Opt_WarnCPPUndef dflags]
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id  "C pre-processor" p
                       (args0 ++ args1 ++ args2 ++ args) mb_env

runPp :: DynFlags -> [Option] -> IO ()
runPp dflags args =   do
  let prog = pgm_F dflags
      opts = map Option (getOpts dflags opt_F)
  runSomething dflags "Haskell pre-processor" prog (args ++ opts)

runCc :: DynFlags -> [Option] -> IO ()
runCc dflags args =   do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingResponseFile dflags cc_filter "C Compiler" p args2 mb_env
 where
  -- discard some harmless warnings from gcc that we can't turn off
  cc_filter = unlines . doFilter . lines

  {-
  gcc gives warnings in chunks like so:
      In file included from /foo/bar/baz.h:11,
                       from /foo/bar/baz2.h:22,
                       from wibble.c:33:
      /foo/flibble:14: global register variable ...
      /foo/flibble:15: warning: call-clobbered r...
  We break it up into its chunks, remove any call-clobbered register
  warnings from each chunk, and then delete any chunks that we have
  emptied of warnings.
  -}
  doFilter = unChunkWarnings . filterWarnings . chunkWarnings []
  -- We can't assume that the output will start with an "In file inc..."
  -- line, so we start off expecting a list of warnings rather than a
  -- location stack.
  chunkWarnings :: [String] -- The location stack to use for the next
                            -- list of warnings
                -> [String] -- The remaining lines to look at
                -> [([String], [String])]
  chunkWarnings loc_stack [] = [(loc_stack, [])]
  chunkWarnings loc_stack xs
      = case break loc_stack_start xs of
        (warnings, lss:xs') ->
            case span loc_start_continuation xs' of
            (lsc, xs'') ->
                (loc_stack, warnings) : chunkWarnings (lss : lsc) xs''
        _ -> [(loc_stack, xs)]

  filterWarnings :: [([String], [String])] -> [([String], [String])]
  filterWarnings [] = []
  -- If the warnings are already empty then we are probably doing
  -- something wrong, so don't delete anything
  filterWarnings ((xs, []) : zs) = (xs, []) : filterWarnings zs
  filterWarnings ((xs, ys) : zs) = case filter wantedWarning ys of
                                       [] -> filterWarnings zs
                                       ys' -> (xs, ys') : filterWarnings zs

  unChunkWarnings :: [([String], [String])] -> [String]
  unChunkWarnings [] = []
  unChunkWarnings ((xs, ys) : zs) = xs ++ ys ++ unChunkWarnings zs

  loc_stack_start        s = "In file included from " `isPrefixOf` s
  loc_start_continuation s = "                 from " `isPrefixOf` s
  wantedWarning w
   | "warning: call-clobbered register used" `isContainedIn` w = False
   | otherwise = True

isContainedIn :: String -> String -> Bool
xs `isContainedIn` ys = any (xs `isPrefixOf`) (tails ys)

askCc :: DynFlags -> [Option] -> IO String
askCc dflags args = do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith dflags "gcc" p args2 $ \real_args ->
    readCreateProcessWithExitCode' (proc p real_args){ env = mb_env }

runSplit :: DynFlags -> [Option] -> IO ()
runSplit dflags args = do
  let (p,args0) = pgm_s dflags
  runSomething dflags "Splitter" p (args0++args)

runAs :: DynFlags -> [Option] -> IO ()
runAs dflags args = do
  let (p,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Assembler" p args2 mb_env

-- | Run the LLVM Optimiser
runLlvmOpt :: DynFlags -> [Option] -> IO ()
runLlvmOpt dflags args = do
  let (p,args0) = pgm_lo dflags
      args1 = map Option (getOpts dflags opt_lo)
  runSomething dflags "LLVM Optimiser" p (args0 ++ args1 ++ args)

-- | Run the LLVM Compiler
runLlvmLlc :: DynFlags -> [Option] -> IO ()
runLlvmLlc dflags args = do
  let (p,args0) = pgm_lc dflags
      args1 = map Option (getOpts dflags opt_lc)
  runSomething dflags "LLVM Compiler" p (args0 ++ args1 ++ args)

-- | Run the clang compiler (used as an assembler for the LLVM
-- backend on OS X as LLVM doesn't support the OS X system
-- assembler)
runClang :: DynFlags -> [Option] -> IO ()
runClang dflags args = do
  -- we simply assume its available on the PATH
  let clang = "clang"
      -- be careful what options we call clang with
      -- see #5903 and #7617 for bugs caused by this.
      (_,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  Exception.catch (do
        runSomethingFiltered dflags id "Clang (Assembler)" clang args2 mb_env
    )
    (\(err :: SomeException) -> do
        errorMsg dflags $
            text ("Error running clang! you need clang installed to use the" ++
                  " LLVM backend") $+$
            text "(or GHC tried to execute clang incorrectly)"
        throwIO err
    )

-- | Run the linker with some arguments and return the output
askLd :: DynFlags -> [Option] -> IO String
askLd dflags args = do
  let (p,args0) = pgm_l dflags
      args1     = map Option (getOpts dflags opt_l)
      args2     = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith dflags "gcc" p args2 $ \real_args ->
    readCreateProcessWithExitCode' (proc p real_args){ env = mb_env }

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: DynFlags -> IO (Maybe (Int, Int))
figureLlvmVersion dflags = do
  let (pgm,opts) = pgm_lc dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  ver <- catchIO (do
             (pin, pout, perr, _) <- runInteractiveProcess pgm args'
                                             Nothing Nothing
             {- > llc -version
                  LLVM (http://llvm.org/):
                    LLVM version 3.5.2
                    ...
             -}
             hSetBinaryMode pout False
             _     <- hGetLine pout
             vline <- dropWhile (not . isDigit) `fmap` hGetLine pout
             v     <- case span (/= '.') vline of
                        ("",_)  -> fail "no digits!"
                        (x,y) -> return (read x
                                        , read $ takeWhile isDigit $ drop 1 y)

             hClose pin
             hClose pout
             hClose perr
             return $ Just v
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out LLVM version):" <+>
                     text (show err))
                errorMsg dflags $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text ("Make sure you have installed LLVM " ++
                                llvmVersionStr supportedLlvmVersion) ]
                return Nothing)
  return ver