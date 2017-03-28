{-# LANGUAGE CPP #-}
{-
-----------------------------------------------------------------------------
--
-- (c) The GHC Team 2017
--
-- Manages temporary files
--
-----------------------------------------------------------------------------
-}

module  SysTools.TempFiles (
        -- Temporary-file management
        setTmpDir, getTempDir,
        newTempName, newTempLibName,
        cleanTempDirs, cleanTempFiles, cleanTempFilesExcept,
        addFilesToClean, removeWith, removeTmpFiles,
        removeTmpDirs,

        -- Misc utilities
        traceCmd, getProcessID
    ) where

#include "HsVersions.h"

import DynFlags
import DriverPhases
import ErrUtils
import Exception
import Util
import Outputable
import Panic

import Control.Monad

import Data.List
import Data.IORef

import System.FilePath
import System.IO.Error
import System.Directory

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
\subsection{Managing temporary files
*                                                                      *
************************************************************************
-}

cleanTempDirs :: DynFlags -> IO ()
cleanTempDirs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = dirsToClean dflags
        ds <- atomicModifyIORef' ref $ \ds -> (Map.empty, ds)
        removeTmpDirs dflags (Map.elems ds)

cleanTempFiles :: DynFlags -> IO ()
cleanTempFiles dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        fs <- atomicModifyIORef' ref $ \fs -> ([],fs)
        removeTmpFiles dflags fs

cleanTempFilesExcept :: DynFlags -> [FilePath] -> IO ()
cleanTempFilesExcept dflags dont_delete
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        to_delete <- atomicModifyIORef' ref $ \files ->
            let res@(_to_keep, _to_delete) =
                    partition (`Set.member` dont_delete_set) files
            in  res
        removeTmpFiles dflags to_delete
  where dont_delete_set = Set.fromList dont_delete


-- Return a unique numeric temp file suffix
newTempSuffix :: DynFlags -> IO Int
newTempSuffix dflags = atomicModifyIORef' (nextTempSuffix dflags) $ \n -> (n+1,n)

-- Find a temporary name that doesn't already exist.
newTempName :: DynFlags -> Suffix -> IO FilePath
newTempName dflags extn
  = do d <- getTempDir dflags
       findTempName (d </> "ghc_") -- See Note [Deterministic base name]
  where
    findTempName :: FilePath -> IO FilePath
    findTempName prefix
      = do n <- newTempSuffix dflags
           let filename = prefix ++ show n <.> extn
           b <- doesFileExist filename
           if b then findTempName prefix
                else do -- clean it up later
                        consIORef (filesToClean dflags) filename
                        return filename

newTempLibName :: DynFlags -> Suffix -> IO (FilePath, FilePath, String)
newTempLibName dflags extn
  = do d <- getTempDir dflags
       findTempName d ("ghc_")
  where
    findTempName :: FilePath -> String -> IO (FilePath, FilePath, String)
    findTempName dir prefix
      = do n <- newTempSuffix dflags -- See Note [Deterministic base name]
           let libname = prefix ++ show n
               filename = dir </> "lib" ++ libname <.> extn
           b <- doesFileExist filename
           if b then findTempName dir prefix
                else do -- clean it up later
                        consIORef (filesToClean dflags) filename
                        return (filename, dir, libname)


-- Return our temporary directory within tmp_dir, creating one if we
-- don't have one yet.
getTempDir :: DynFlags -> IO FilePath
getTempDir dflags = do
    mapping <- readIORef dir_ref
    case Map.lookup tmp_dir mapping of
        Nothing -> do
            pid <- getProcessID
            let prefix = tmp_dir </> "ghc" ++ show pid ++ "_"
            mask_ $ mkTempDir prefix
        Just dir -> return dir
  where
    tmp_dir = tmpDir dflags
    dir_ref = dirsToClean dflags

    mkTempDir :: FilePath -> IO FilePath
    mkTempDir prefix = do
        n <- newTempSuffix dflags
        let our_dir = prefix ++ show n

        -- 1. Speculatively create our new directory.
        createDirectory our_dir

        -- 2. Update the dirsToClean mapping unless an entry already exists
        -- (i.e. unless another thread beat us to it).
        their_dir <- atomicModifyIORef' dir_ref $ \mapping ->
            case Map.lookup tmp_dir mapping of
                Just dir -> (mapping, Just dir)
                Nothing  -> (Map.insert tmp_dir our_dir mapping, Nothing)

        -- 3. If there was an existing entry, return it and delete the
        -- directory we created.  Otherwise return the directory we created.
        case their_dir of
            Nothing  -> do
                debugTraceMsg dflags 2 $
                    text "Created temporary directory:" <+> text our_dir
                return our_dir
            Just dir -> do
                removeDirectory our_dir
                return dir
      `catchIO` \e -> if isAlreadyExistsError e
                      then mkTempDir prefix else ioError e

-- Note [Deterministic base name]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The filename of temporary files, especially the basename of C files, can end
-- up in the output in some form, e.g. as part of linker debug information. In the
-- interest of bit-wise exactly reproducible compilation (#4012), the basename of
-- the temporary file no longer contains random information (it used to contain
-- the process id).
--
-- This is ok, as the temporary directory used contains the pid (see getTempDir).

addFilesToClean :: DynFlags -> [FilePath] -> IO ()
-- May include wildcards [used by DriverPipeline.run_phase SplitMangle]
addFilesToClean dflags new_files
    = atomicModifyIORef' (filesToClean dflags) $ \files -> (new_files++files, ())

removeTmpDirs :: DynFlags -> [FilePath] -> IO ()
removeTmpDirs dflags ds
  = traceCmd dflags "Deleting temp dirs"
             ("Deleting: " ++ unwords ds)
             (mapM_ (removeWith dflags removeDirectory) ds)

removeTmpFiles :: DynFlags -> [FilePath] -> IO ()
removeTmpFiles dflags fs
  = warnNon $
    traceCmd dflags "Deleting temp files"
             ("Deleting: " ++ unwords deletees)
             (mapM_ (removeWith dflags removeFile) deletees)
  where
     -- Flat out refuse to delete files that are likely to be source input
     -- files (is there a worse bug than having a compiler delete your source
     -- files?)
     --
     -- Deleting source files is a sign of a bug elsewhere, so prominently flag
     -- the condition.
    warnNon act
     | null non_deletees = act
     | otherwise         = do
        putMsg dflags (text "WARNING - NOT deleting source files:" <+> hsep (map text non_deletees))
        act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

removeWith :: DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith dflags remover f = remover f `catchIO`
  (\e ->
   let msg = if isDoesNotExistError e
             then text "Warning: deleting non-existent" <+> text f
             else text "Warning: exception raised when deleting"
                                            <+> text f <> colon
               $$ text (show e)
   in debugTraceMsg dflags 2 msg
  )

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#endif

traceCmd :: DynFlags -> String -> String -> IO a -> IO a
-- trace the command (at two levels of verbosity)
traceCmd dflags phase_name cmd_line action
 = do   { let verb = verbosity dflags
        ; showPass dflags phase_name
        ; debugTraceMsg dflags 3 (text cmd_line)
        ; case flushErr dflags of
              FlushErr io -> io

           -- And run it!
        ; action `catchIO` handle_exn verb
        }
  where
    handle_exn _verb exn = do { debugTraceMsg dflags 2 (char '\n')
                              ; debugTraceMsg dflags 2 (text "Failed:" <+> text cmd_line <+> text (show exn))
                              ; throwGhcExceptionIO (ProgramError (show exn))}