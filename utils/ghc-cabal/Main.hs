{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main (main) where

import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Package
import Distribution.System
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.GHC
import Distribution.Simple.Program
import Distribution.Simple.Program.HcPkg
import Distribution.Simple.Setup (ConfigFlags(configStripLibs), fromFlag, toFlag)
import Distribution.Simple.Utils (defaultPackageDesc, writeFileAtomic, toUTF8)
import Distribution.Simple.Build (writeAutogenFiles)
import Distribution.Simple.Register
import Distribution.Text
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex

import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import System.IO
import System.Directory (setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit      (exitWith, ExitCode(..))
import System.FilePath

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          args <- getArgs
          case args of
              "hscolour" : dir : distDir : args' ->
                  runHsColour dir distDir args'
              "check" : dir : [] ->
                  doCheck dir
              "copy" : dir : distDir
                     : strip : myDestDir : myPrefix : myLibdir : myDocdir
                     : ghcLibWays : args' ->
                  doCopy dir distDir
                         strip myDestDir myPrefix myLibdir myDocdir
                         ("dyn" `elem` words ghcLibWays)
                         args'
              "register" : dir : distDir : ghc : ghcpkg : topdir
                         : myDestDir : myPrefix : myLibdir : myDocdir
                         : relocatableBuild : args' ->
                  doRegister dir distDir ghc ghcpkg topdir
                             myDestDir myPrefix myLibdir myDocdir
                             relocatableBuild args'
              "configure" : dir : distDir : config_args ->
                  generate dir distDir config_args
              "sdist" : dir : distDir : [] ->
                  doSdist dir distDir
              ["--version"] ->
                  defaultMainArgs ["--version"]
              _ -> die syntax_error

syntax_error :: [String]
syntax_error =
    ["syntax: ghc-cabal configure <configure-args> -- <distdir> <directory>...",
     "        ghc-cabal install <ghc-pkg> <directory> <distdir> <destdir> <prefix> <args>...",
     "        ghc-cabal hscolour <distdir> <directory> <args>..."]

die :: [String] -> IO a
die errs = do mapM_ (hPutStrLn stderr) errs
              exitWith (ExitFailure 1)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory io
 = bracket (getCurrentDirectory) (setCurrentDirectory)
           (const (setCurrentDirectory directory >> io))

-- We need to use the autoconfUserHooks, as the packages that use
-- configure can create a .buildinfo file, and we need any info that
-- ends up in it.
userHooks :: UserHooks
userHooks = autoconfUserHooks

runDefaultMain :: IO ()
runDefaultMain
 = do let verbosity = normal
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readPackageDescription verbosity gpdFile
      case buildType (flattenPackageDescription gpd) of
          Just Configure -> defaultMainWithHooks autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          Just Custom ->
              do configureExists <- doesFileExist "configure"
                 if configureExists
                     then defaultMainWithHooks autoconfUserHooks
                     else defaultMain
          -- not quite right, but good enough for us:
          _ -> defaultMain

doSdist :: FilePath -> FilePath -> IO ()
doSdist directory distDir
 = withCurrentDirectory directory
 $ withArgs (["sdist", "--builddir", distDir])
            runDefaultMain

doCheck :: FilePath -> IO ()
doCheck directory
 = withCurrentDirectory directory
 $ do let verbosity = normal
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readPackageDescription verbosity gpdFile
      case filter isFailure $ checkPackage gpd Nothing of
          []   -> return ()
          errs -> mapM_ print errs >> exitWith (ExitFailure 1)
    where isFailure (PackageDistSuspicious {}) = False
          isFailure (PackageDistSuspiciousWarn {}) = False
          isFailure _ = True

runHsColour :: FilePath -> FilePath -> [String] -> IO ()
runHsColour directory distdir args
 = withCurrentDirectory directory
 $ defaultMainArgs ("hscolour" : "--builddir" : distdir : args)

doCopy :: FilePath -> FilePath
       -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Bool
       -> [String]
       -> IO ()
doCopy directory distDir
       strip myDestDir myPrefix myLibdir myDocdir withSharedLibs
       args
 = withCurrentDirectory directory $ do
     let copyArgs = ["copy", "--builddir", distDir]
                 ++ (if null myDestDir
                     then []
                     else ["--destdir", myDestDir])
                 ++ args
         copyHooks = userHooks {
                         copyHook = noGhcPrimHook
                                  $ modHook False
                                  $ copyHook userHooks
                     }

     defaultMainWithHooksArgs copyHooks copyArgs
    where
      noGhcPrimHook f pd lbi us flags
              = let pd'
                     | packageName pd == mkPackageName "ghc-prim" =
                        case library pd of
                        Just lib ->
                            let ghcPrim = fromJust (simpleParse "GHC.Prim")
                                ems = filter (ghcPrim /=) (exposedModules lib)
                                lib' = lib { exposedModules = ems }
                            in pd { library = Just lib' }
                        Nothing ->
                            error "Expected a library, but none found"
                     | otherwise = pd
                in f pd' lbi us flags
      modHook relocatableBuild f pd lbi us flags
       = do let verbosity = normal
                idts = updateInstallDirTemplates relocatableBuild
                                                 myPrefix myLibdir myDocdir
                                                 (installDirTemplates lbi)
                progs = withPrograms lbi
                stripProgram' = stripProgram {
                    programFindLocation = \_ _ -> return (Just (strip,[])) }

            progs' <- configureProgram verbosity stripProgram' progs
            let lbi' = lbi {
                               withPrograms = progs',
                               installDirTemplates = idts,
                               configFlags = cfg,
                               stripLibs = fromFlag (configStripLibs cfg),
                               withSharedLib = withSharedLibs
                           }

                -- This hack allows to interpret the "strip"
                -- command-line argument being set to ':' to signify
                -- disabled library stripping
                cfg | strip == ":" = (configFlags lbi) { configStripLibs = toFlag False }
                    | otherwise    = configFlags lbi

            f pd lbi' us flags

doRegister :: FilePath -> FilePath -> FilePath -> FilePath
           -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath
           -> String -> [String]
           -> IO ()
doRegister directory distDir ghc ghcpkg topdir
           myDestDir myPrefix myLibdir myDocdir
           relocatableBuildStr args
 = withCurrentDirectory directory $ do
     relocatableBuild <- case relocatableBuildStr of
                         "YES" -> return True
                         "NO"  -> return False
                         _ -> die ["Bad relocatableBuildStr: " ++
                                   show relocatableBuildStr]
     let regArgs = "register" : "--builddir" : distDir : args
         regHooks = userHooks {
                        regHook = modHook relocatableBuild
                                $ regHook userHooks
                    }

     defaultMainWithHooksArgs regHooks  regArgs
    where
      modHook relocatableBuild f pd lbi us flags
       = do let verbosity = normal
                idts = updateInstallDirTemplates relocatableBuild
                                                 myPrefix myLibdir myDocdir
                                                 (installDirTemplates lbi)
                progs = withPrograms lbi
                ghcpkgconf = topdir </> "package.conf.d"
                ghcProgram' = ghcProgram {
                    programPostConf = \_ cp -> return cp { programDefaultArgs = ["-B" ++ topdir] },
                    programFindLocation = \_ _ -> return (Just (ghc,[])) }
                ghcPkgProgram' = ghcPkgProgram {
                    programPostConf = \_ cp -> return cp { programDefaultArgs =
                                                                ["--global-package-db", ghcpkgconf]
                                                                ++ ["--force" | not (null myDestDir) ] },
                    programFindLocation = \_ _ -> return (Just (ghcpkg,[])) }
                configurePrograms ps conf = foldM (flip (configureProgram verbosity)) conf ps

            progs' <- configurePrograms [ghcProgram', ghcPkgProgram'] progs
            instInfos <- dump (hcPkgInfo progs') verbosity GlobalPackageDB
            let installedPkgs' = PackageIndex.fromList instInfos
            let lbi' = lbi {
                               installedPkgs = installedPkgs',
                               installDirTemplates = idts,
                               withPrograms = progs'
                           }
            f pd lbi' us flags

updateInstallDirTemplates :: Bool -> FilePath -> FilePath -> FilePath
                          -> InstallDirTemplates
                          -> InstallDirTemplates
updateInstallDirTemplates relocatableBuild myPrefix myLibdir myDocdir idts
    = idts {
          prefix    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir"
                          else myPrefix,
          libdir    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir"
                          else myLibdir,
          libsubdir = toPathTemplate "$libname",
          docdir    = toPathTemplate $
                          if relocatableBuild
                          then "$topdir/../doc/html/libraries/$pkgid"
                          else (myDocdir </> "$pkgid"),
          htmldir   = toPathTemplate "$docdir"
      }

-- On Windows we need to split the ghc package into 2 pieces, or the
-- DLL that it makes contains too many symbols (#5987). There are
-- therefore 2 libraries, not just the 1 that Cabal assumes.
mangleIPI :: FilePath -> FilePath -> LocalBuildInfo
          -> Installed.InstalledPackageInfo -> Installed.InstalledPackageInfo
mangleIPI "compiler" "stage2" lbi ipi
 | isWindows =
    -- Cabal currently only ever installs ONE Haskell library, c.f.
    -- the code in Cabal.Distribution.Simple.Register.  If it
    -- ever starts installing more we'll have to find the
    -- library that's too big and split that.
    let [old_hslib] = Installed.hsLibraries ipi
    in ipi {
        Installed.hsLibraries = [old_hslib, old_hslib ++ "-0"]
    }
    where isWindows = case hostPlatform lbi of
                      Platform _ Windows -> True
                      _                  -> False
mangleIPI _ _ _ ipi = ipi

generate :: FilePath -> FilePath -> [String] -> IO ()
generate directory distdir config_args
 = withCurrentDirectory directory
 $ do let verbosity = normal
      -- XXX We shouldn't just configure with the default flags
      -- XXX And this, and thus the "getPersistBuildConfig distdir" below,
      -- aren't going to work when the deps aren't built yet
      withArgs (["configure", "--distdir", distdir, "--ipid", "$pkg-$version"] ++ config_args)
               runDefaultMain

      lbi <- getPersistBuildConfig distdir
      let pd0 = localPkgDescr lbi

      writePersistBuildConfig distdir lbi

      hooked_bi <-
           if (buildType pd0 == Just Configure) || (buildType pd0 == Just Custom)
           then do
              maybe_infoFile <- defaultHookedPackageDesc
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> readHookedBuildInfo verbosity infoFile
           else
              return emptyHookedBuildInfo

      let pd = updatePackageDescription hooked_bi pd0

      -- generate Paths_<pkg>.hs and cabal-macros.h
      withAllComponentsInBuildOrder pd lbi $ \_ clbi ->
        writeAutogenFiles verbosity pd lbi clbi

      -- generate inplace-pkg-config
      withLibLBI pd lbi $ \lib clbi ->
          do cwd <- getCurrentDirectory
             let ipid = mkUnitId (display (packageId pd))
             let installedPkgInfo = inplaceInstalledPackageInfo cwd distdir
                                        pd (mkAbiHash "") lib lbi clbi
                 final_ipi = mangleIPI directory distdir lbi $ installedPkgInfo {
                                 Installed.installedUnitId = ipid,
                                 Installed.compatPackageKey = display (packageId pd),
                                 Installed.haddockHTMLs = []
                             }
                 content = Installed.showInstalledPackageInfo final_ipi ++ "\n"
             writeFileAtomic (distdir </> "inplace-pkg-config") (BS.pack $ toUTF8 content)

      let
          comp = compiler lbi
          libBiModules lib = (libBuildInfo lib, libModules lib)
          exeBiModules exe = (buildInfo exe, ModuleName.main : exeModules exe)
          biModuless = (map libBiModules . maybeToList $ library pd)
                    ++ (map exeBiModules $ executables pd)
          buildableBiModuless = filter isBuildable biModuless
              where isBuildable (bi', _) = buildable bi'
          (bi, modules) = case buildableBiModuless of
                          [] -> error "No buildable component found"
                          [biModules] -> biModules
                          _ -> error ("XXX ghc-cabal can't handle " ++
                                      "more than one buildinfo yet")
          -- XXX Another Just...
          Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)

          dep_pkgs = PackageIndex.topologicalOrder (packageHacks (installedPkgs lbi))
          forDeps f = concatMap f dep_pkgs

          -- copied from Distribution.Simple.PreProcess.ppHsc2Hs
          packageHacks = case compilerFlavor (compiler lbi) of
            GHC -> hackRtsPackage
            _   -> id
          -- We don't link in the actual Haskell libraries of our
          -- dependencies, so the -u flags in the ldOptions of the rts
          -- package mean linking fails on OS X (it's ld is a tad
          -- stricter than gnu ld). Thus we remove the ldOptions for
          -- GHC's rts package:
          hackRtsPackage index =
            case PackageIndex.lookupPackageName index (mkPackageName "rts") of
              [(_,[rts])] ->
                 PackageIndex.insert rts{
                     Installed.ldOptions = [],
                     Installed.libraryDirs = filter (not . ("gcc-lib" `isSuffixOf`)) (Installed.libraryDirs rts)} index
                        -- GHC <= 6.12 had $topdir/gcc-lib in their
                        -- library-dirs for the rts package, which causes
                        -- problems when we try to use the in-tree mingw,
                        -- due to accidentally picking up the incompatible
                        -- libraries there.  So we filter out gcc-lib from
                        -- the RTS's library-dirs here.
              _ -> error "No (or multiple) ghc rts package is registered!!"

          dep_ids  = map snd (externalPackageDeps lbi)
          deps     = map display dep_ids
          dep_direct = map (fromMaybe (error "ghc-cabal: dep_keys failed")
                           . PackageIndex.lookupUnitId
                                            (installedPkgs lbi)
                           . fst)
                       . externalPackageDeps
                       $ lbi
          dep_ipids = map (display . Installed.installedUnitId) dep_direct
          depLibNames
            | packageKeySupported comp = dep_ipids
            | otherwise = deps
          depNames = map (display . packageName) dep_ids

          transitive_dep_ids = map Installed.sourcePackageId dep_pkgs
          transitiveDeps = map display transitive_dep_ids
          transitiveDepLibNames
            | packageKeySupported comp = map fixupRtsLibName transitiveDeps
            | otherwise = transitiveDeps
          fixupRtsLibName "rts-1.0" = "rts"
          fixupRtsLibName x = x
          transitiveDepNames = map (display . packageName) transitive_dep_ids

          libraryDirs = forDeps Installed.libraryDirs
          -- The mkLibraryRelDir function is a bit of a hack.
          -- Ideally it should be handled in the makefiles instead.
          mkLibraryRelDir "rts"   = "rts/dist/build"
          mkLibraryRelDir "ghc"   = "compiler/stage2/build"
          mkLibraryRelDir "Cabal" = "libraries/Cabal/Cabal/dist-install/build"
          mkLibraryRelDir l       = "libraries/" ++ l ++ "/dist-install/build"
          libraryRelDirs = map mkLibraryRelDir transitiveDepNames
      wrappedIncludeDirs <- wrap $ forDeps Installed.includeDirs
      wrappedLibraryDirs <- wrap libraryDirs

      let variablePrefix = directory ++ '_':distdir
          mods      = map display modules
          otherMods = map display (otherModules bi)
          allMods = mods ++ otherMods
      let xs = [variablePrefix ++ "_VERSION = " ++ display (pkgVersion (package pd)),
                -- TODO: move inside withLibLBI
                variablePrefix ++ "_COMPONENT_ID = " ++ localCompatPackageKey lbi,
                variablePrefix ++ "_MODULES = " ++ unwords mods,
                variablePrefix ++ "_HIDDEN_MODULES = " ++ unwords otherMods,
                variablePrefix ++ "_SYNOPSIS =" ++ (unwords $ lines $ synopsis pd),
                variablePrefix ++ "_HS_SRC_DIRS = " ++ unwords (hsSourceDirs bi),
                variablePrefix ++ "_DEPS = " ++ unwords deps,
                variablePrefix ++ "_DEP_IPIDS = " ++ unwords dep_ipids,
                variablePrefix ++ "_DEP_NAMES = " ++ unwords depNames,
                variablePrefix ++ "_DEP_COMPONENT_IDS = " ++ unwords depLibNames,
                variablePrefix ++ "_TRANSITIVE_DEP_NAMES = " ++ unwords transitiveDepNames,
                variablePrefix ++ "_TRANSITIVE_DEP_COMPONENT_IDS = " ++ unwords transitiveDepLibNames,
                variablePrefix ++ "_INCLUDE_DIRS = " ++ unwords (includeDirs bi),
                variablePrefix ++ "_INCLUDES = " ++ unwords (includes bi),
                variablePrefix ++ "_INSTALL_INCLUDES = " ++ unwords (installIncludes bi),
                variablePrefix ++ "_EXTRA_LIBRARIES = " ++ unwords (extraLibs bi),
                variablePrefix ++ "_EXTRA_LIBDIRS = " ++ unwords (extraLibDirs bi),
                variablePrefix ++ "_C_SRCS  = " ++ unwords (cSources bi),
                variablePrefix ++ "_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard " ++ directory ++ "/cbits/*.cmm)))",
                variablePrefix ++ "_DATA_FILES = "    ++ unwords (dataFiles pd),
                -- XXX This includes things it shouldn't, like:
                -- -odir dist-bootstrapping/build
                variablePrefix ++ "_HC_OPTS = " ++ escape (unwords
                       (   programDefaultArgs ghcProg
                        ++ hcOptions GHC bi
                        ++ languageToFlags (compiler lbi) (defaultLanguage bi)
                        ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
                        ++ programOverrideArgs ghcProg)),
                variablePrefix ++ "_CC_OPTS = "                        ++ unwords (ccOptions bi),
                variablePrefix ++ "_CPP_OPTS = "                       ++ unwords (cppOptions bi),
                variablePrefix ++ "_LD_OPTS = "                        ++ unwords (ldOptions bi),
                variablePrefix ++ "_DEP_INCLUDE_DIRS_SINGLE_QUOTED = " ++ unwords wrappedIncludeDirs,
                variablePrefix ++ "_DEP_CC_OPTS = "                    ++ unwords (forDeps Installed.ccOptions),
                variablePrefix ++ "_DEP_LIB_DIRS_SINGLE_QUOTED = "     ++ unwords wrappedLibraryDirs,
                variablePrefix ++ "_DEP_LIB_DIRS_SEARCHPATH = "        ++ mkSearchPath libraryDirs,
                variablePrefix ++ "_DEP_LIB_REL_DIRS = "               ++ unwords libraryRelDirs,
                variablePrefix ++ "_DEP_LIB_REL_DIRS_SEARCHPATH = "    ++ mkSearchPath libraryRelDirs,
                variablePrefix ++ "_DEP_EXTRA_LIBS = "                 ++ unwords (forDeps Installed.extraLibraries),
                variablePrefix ++ "_DEP_LD_OPTS = "                    ++ unwords (forDeps Installed.ldOptions),
                variablePrefix ++ "_BUILD_GHCI_LIB = "                 ++ boolToYesNo (withGHCiLib lbi),
                "",
                -- Sometimes we need to modify the automatically-generated package-data.mk
                -- bindings in a special way for the GHC build system, so allow that here:
                "$(eval $(" ++ directory ++ "_PACKAGE_MAGIC))"
                ]
      writeFile (distdir ++ "/package-data.mk") $ unlines xs

      writeFileUtf8 (distdir ++ "/haddock-prologue.txt") $
          if null (description pd) then synopsis pd
                                   else description pd
  where
     escape = foldr (\c xs -> if c == '#' then '\\':'#':xs else c:xs) []
     wrap = mapM wrap1
     wrap1 s
      | null s        = die ["Wrapping empty value"]
      | '\'' `elem` s = die ["Single quote in value to be wrapped:", s]
      -- We want to be able to assume things like <space><quote> is the
      -- start of a value, so check there are no spaces in confusing
      -- positions
      | head s == ' ' = die ["Leading space in value to be wrapped:", s]
      | last s == ' ' = die ["Trailing space in value to be wrapped:", s]
      | otherwise     = return ("\'" ++ s ++ "\'")
     mkSearchPath = intercalate [searchPathSeparator]
     boolToYesNo True = "YES"
     boolToYesNo False = "NO"

     -- | Version of 'writeFile' that always uses UTF8 encoding
     writeFileUtf8 f txt = withFile f WriteMode $ \hdl -> do
         hSetEncoding hdl utf8
         hPutStr hdl txt
