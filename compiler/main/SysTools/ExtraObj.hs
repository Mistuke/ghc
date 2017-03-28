-----------------------------------------------------------------------------
--
-- GHC Extra object linking code
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------

module SysTools.ExtraObj (
   mkExtraObjToLinkIntoBinary, mkNoteObjsToLinkIntoBinary,
   getLinkInfo, getCompilerInfo,
   ghcLinkInfoSectionName, ghcLinkInfoNoteName, platformSupportsSavingLinkOpts,
   haveRtsOptsFlags
) where

import ErrUtils
import Exception
import DynFlags
import Packages
import Platform
import Outputable
import SrcLoc           ( noSrcSpan )
import Module
import Elf
import Util

import Control.Monad
import Data.Maybe
import Data.List
import Data.IORef

import Control.Monad.IO.Class

import SysTools.TempFiles
import SysTools.Process
import SysTools.Tasks

-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
--
-- On Windows, when making a shared library we also may need a DllMain.
--
mkExtraObjToLinkIntoBinary :: DynFlags -> IO FilePath
mkExtraObjToLinkIntoBinary dflags = do
   when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
      putLogMsg dflags NoReason SevInfo noSrcSpan
          (defaultUserStyle dflags)
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

   mkExtraObj dflags "c" (showSDoc dflags main)

 where
  main
   | gopt Opt_NoHsMain dflags = Outputable.empty
   | otherwise
       = case ghcLink dflags of
                LinkDynLib -> if platformOS (targetPlatform dflags) == OSMinGW32
                                 then dllMain
                                 else Outputable.empty
                _                      -> exeMain

  exeMain = vcat [
      text "#include \"Rts.h\"",
      text "extern StgClosure ZCMain_main_closure;",
      text "int main(int argc, char *argv[])",
      char '{',
      text " RtsConfig __conf = defaultRtsConfig;",
      text " __conf.rts_opts_enabled = "
          <> text (show (rtsOptsEnabled dflags)) <> semi,
      text " __conf.rts_opts_suggestions = "
          <> text (if rtsOptsSuggestions dflags
                      then "true"
                      else "false") <> semi,
      case rtsOpts dflags of
         Nothing   -> Outputable.empty
         Just opts -> text "    __conf.rts_opts= " <>
                        text (show opts) <> semi,
      text " __conf.rts_hs_main = true;",
      text " return hs_main(argc,argv,&ZCMain_main_closure,__conf);",
      char '}',
      char '\n' -- final newline, to keep gcc happy
     ]

  dllMain = vcat [
      text "#include \"Rts.h\"",
      text "#include <windows.h>",
      text "#include <stdbool.h>",
      char '\n',
      text "bool",
      text "WINAPI",
      text "DllMain ( HINSTANCE hInstance STG_UNUSED",
      text "        , DWORD reason STG_UNUSED",
      text "        , LPVOID reserved STG_UNUSED",
      text "        )",
      text "{",
      text "  return true;",
      text "}",
      char '\n' -- final newline, to keep gcc happy
     ]

-- Write out the link info section into a new assembly file. Previously
-- this was included as inline assembly in the main.c file but this
-- is pretty fragile. gas gets upset trying to calculate relative offsets
-- that span the .note section (notably .text) when debug info is present
mkNoteObjsToLinkIntoBinary :: DynFlags -> [InstalledUnitId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary dflags dep_packages = do
   link_info <- getLinkInfo dflags dep_packages

   if (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
     then fmap (:[]) $ mkExtraObj dflags "s" (showSDoc dflags (link_opts link_info))
     else return []

  where
    link_opts info = hcat [
      -- "link info" section (see Note [LinkInfo section])
      makeElfNote dflags ghcLinkInfoSectionName ghcLinkInfoNoteName 0 info,

      -- ALL generated assembly must have this section to disable
      -- executable stacks.  See also
      -- compiler/nativeGen/AsmCodeGen.hs for another instance
      -- where we need to do this.
      if platformHasGnuNonexecStack (targetPlatform dflags)
        then text ".section .note.GNU-stack,\"\",@progbits\n"
        else Outputable.empty
      ]

-- | Return the "link info" string
--
-- See Note [LinkInfo section]
getLinkInfo :: DynFlags -> [InstalledUnitId] -> IO String
getLinkInfo dflags dep_packages = do
   package_link_opts <- getPackageLinkOpts dflags dep_packages
   pkg_frameworks <- if platformUsesFrameworks (targetPlatform dflags)
                     then getPackageFrameworks dflags dep_packages
                     else return []
   let extra_ld_inputs = ldInputs dflags
   let
      link_info = (package_link_opts,
                   pkg_frameworks,
                   rtsOpts dflags,
                   rtsOptsEnabled dflags,
                   gopt Opt_NoHsMain dflags,
                   map showOpt extra_ld_inputs,
                   getOpts dflags opt_l)
   --
   return (show link_info)

platformSupportsSavingLinkOpts :: OS -> Bool
platformSupportsSavingLinkOpts os
  | os == OSSolaris2 = False -- see #5382
  | otherwise        = osElfTarget os

-- See Note [LinkInfo section]
ghcLinkInfoSectionName :: String
ghcLinkInfoSectionName = ".debug-ghc-link-info"
   -- if we use the ".debug" prefix, then strip will strip it by default

-- Identifier for the note (see Note [LinkInfo section])
ghcLinkInfoNoteName :: String
ghcLinkInfoNoteName = "GHC link info"

{- Note [LinkInfo section]
   ~~~~~~~~~~~~~~~~~~~~~~~

The "link info" is a string representing the parameters of the link. We save
this information in the binary, and the next time we link, if nothing else has
changed, we use the link info stored in the existing binary to decide whether
to re-link or not.

The "link info" string is stored in a ELF section called ".debug-ghc-link-info"
(see ghcLinkInfoSectionName) with the SHT_NOTE type.  For some time, it used to
not follow the specified record-based format (see #11022).

-}

mkExtraObj :: DynFlags -> Suffix -> String -> IO FilePath
mkExtraObj dflags extn xs
 = do cFile <- newTempName dflags extn
      oFile <- newTempName dflags "o"
      writeFile cFile xs
      ccInfo <- liftIO $ getCompilerInfo dflags
      runCc dflags
            ([Option        "-c",
              FileOption "" cFile,
              Option        "-o",
              FileOption "" oFile]
             ++ if extn /= "s"
                    then cOpts
                    else asmOpts ccInfo)
      return oFile
    where
      -- Pass a different set of options to the C compiler depending one whether
      -- we're compiling C or assembler. When compiling C, we pass the usual
      -- set of include directories and PIC flags.
      cOpts = map Option (picCCOpts dflags)
                    ++ map (FileOption "-I")
                            (includeDirs $ getPackageDetails dflags rtsUnitId)

      -- When compiling assembler code, we drop the usual C options, and if the
      -- compiler is Clang, we add an extra argument to tell Clang to ignore
      -- unused command line options. See trac #11684.
      asmOpts ccInfo =
            if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                then [Option "-Qunused-arguments"]
                else []

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True

-- Grab compiler info and cache it in DynFlags.
getCompilerInfo :: DynFlags -> IO CompilerInfo
getCompilerInfo dflags = do
  info <- readIORef (rtccInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getCompilerInfo' dflags
      writeIORef (rtccInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getCompilerInfo' :: DynFlags -> IO CompilerInfo
getCompilerInfo' dflags = do
  let (pgm,_) = pgm_c dflags
      -- Try to grab the info from the process output.
      parseCompilerInfo _stdo stde _exitc
        -- Regular GCC
        | any ("gcc version" `isInfixOf`) stde =
          return GCC
        -- Regular clang
        | any ("clang version" `isInfixOf`) stde =
          return Clang
        -- XCode 5.1 clang
        | any ("Apple LLVM version 5.1" `isPrefixOf`) stde =
          return AppleClang51
        -- XCode 5 clang
        | any ("Apple LLVM version" `isPrefixOf`) stde =
          return AppleClang
        -- XCode 4.1 clang
        | any ("Apple clang version" `isPrefixOf`) stde =
          return AppleClang
         -- Unknown linker.
        | otherwise = fail "invalid -v output, or compiler is unsupported"

  -- Process the executable call
  info <- catchIO (do
                (exitc, stdo, stde) <-
                    readProcessEnvWithExitCode pgm ["-v"] c_locale_env
                -- Split the output by lines to make certain kinds
                -- of processing easier.
                parseCompilerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out C compiler information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out C compiler information!" $$
                  text "Make sure you're using GNU gcc, or clang"
                return UnknownCC)
  return info