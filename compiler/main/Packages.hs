-- (c) The University of Glasgow, 2006

{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}

-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * Reading the package config, and processing cmdline args
        PackageState(preloadPackages, explicitPackages, requirementContext),
        PackageConfigMap,
        emptyPackageState,
        initPackages,
        readPackageConfigs,
        getPackageConfRefs,
        resolvePackageConfig,
        readPackageConfig,
        listPackageConfigMap,

        -- * Querying the package config
        lookupPackage,
        lookupPackage',
        lookupInstalledPackage,
        lookupPackageName,
        improveUnitId,
        searchPackageId,
        getPackageDetails,
        getInstalledPackageDetails,
        componentIdString,
        displayInstalledUnitId,
        listVisibleModuleNames,
        lookupModuleInAllPackages,
        lookupModuleWithSuggestions,
        lookupPluginModuleWithSuggestions,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),

        -- * Inspecting the set of packages in scope
        getPackageIncludePath,
        getPackageLibraryPath,
        getPackageLinkOpts,
        getPackageExtraCcOpts,
        getPackageFrameworkPath,
        getPackageFrameworks,
        getPackageConfigMap,
        getPreloadPackagesAnd,

        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs,

        -- * Utils
        unwireUnitId,
        pprFlag,
        pprPackages,
        pprPackagesSimple,
        pprModuleMap,
        isDllName,
        filterRtsWays
    )
where

#include "HsVersions.h"

import GHC.PackageDb
import PackageConfig
import DynFlags
import Name             ( Name, nameModule_maybe )
import UniqFM
import UniqDFM
import UniqSet
import Module
import Util
import Panic
import Platform
import Outputable
import Maybes

import System.Environment ( getEnv )
import FastString
import ErrUtils         ( debugTraceMsg, MsgDoc, printInfoForUser )
import Exception

import System.Directory
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import Control.Monad
import Data.Char ( toUpper )
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Data.Monoid (First(..))
#if __GLASGOW_HASKELL__ > 710
import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup
#endif
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import qualified FiniteMap as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- The Package state

-- | Package state is all stored in 'DynFlags', including the details of
-- all packages, which packages are exposed, and which modules they
-- provide.
--
-- The package state is computed by 'initPackages', and kept in DynFlags.
-- It is influenced by various package flags:
--
--   * @-package <pkg>@ and @-package-id <pkg>@ cause @<pkg>@ to become exposed.
--     If @-hide-all-packages@ was not specified, these commands also cause
--      all other packages with the same name to become hidden.
--
--   * @-hide-package <pkg>@ causes @<pkg>@ to become hidden.
--
--   * (there are a few more flags, check below for their semantics)
--
-- The package state has the following properties.
--
--   * Let @exposedPackages@ be the set of packages thus exposed.
--     Let @depExposedPackages@ be the transitive closure from @exposedPackages@ of
--     their dependencies.
--
--   * When searching for a module from an preload import declaration,
--     only the exposed modules in @exposedPackages@ are valid.
--
--   * When searching for a module from an implicit import, all modules
--     from @depExposedPackages@ are valid.
--
--   * When linking in a compilation manager mode, we link in packages the
--     program depends on (the compiler knows this list by the
--     time it gets to the link step).  Also, we link in all packages
--     which were mentioned with preload @-package@ flags on the command-line,
--     or are a transitive dependency of same, or are \"base\"\/\"rts\".
--     The reason for this is that we might need packages which don't
--     contain any Haskell modules, and therefore won't be discovered
--     by the normal mechanism of dependency tracking.

-- Notes on DLLs
-- ~~~~~~~~~~~~~
-- When compiling module A, which imports module B, we need to
-- know whether B will be in the same DLL as A.
--      If it's in the same DLL, we refer to B_f_closure
--      If it isn't, we refer to _imp__B_f_closure
-- When compiling A, we record in B's Module value whether it's
-- in a different DLL, by setting the DLL flag.

-- | Given a module name, there may be multiple ways it came into scope,
-- possibly simultaneously.  This data type tracks all the possible ways
-- it could have come into scope.  Warning: don't use the record functions,
-- they're partial!
data ModuleOrigin =
    -- | Module is hidden, and thus never will be available for import.
    -- (But maybe the user didn't realize), so we'll still keep track
    -- of these modules.)
    ModHidden
    -- | Module is public, and could have come from some places.
  | ModOrigin {
        -- | @Just False@ means that this module is in
        -- someone's @exported-modules@ list, but that package is hidden;
        -- @Just True@ means that it is available; @Nothing@ means neither
        -- applies.
        fromOrigPackage :: Maybe Bool
        -- | Is the module available from a reexport of an exposed package?
        -- There could be multiple.
      , fromExposedReexport :: [PackageConfig]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [PackageConfig]
        -- | Did the module export come from a package flag? (ToDo: track
        -- more information.
      , fromPackageFlag :: Bool
      }

instance Outputable ModuleOrigin where
    ppr ModHidden = text "hidden module"
    ppr (ModOrigin e res rhs f) = sep (punctuate comma (
        (case e of
            Nothing -> []
            Just False -> [text "hidden package"]
            Just True -> [text "exposed package"]) ++
        (if null res
            then []
            else [text "reexport by" <+>
                    sep (map (ppr . packageConfigId) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . packageConfigId) res)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is expsed, and
-- also its 'PackageConfig'.
fromReexportedModules :: Bool -> PackageConfig -> ModuleOrigin
fromReexportedModules True pkg = ModOrigin Nothing [pkg] [] False
fromReexportedModules False pkg = ModOrigin Nothing [] [pkg] False

-- | Smart constructor for a module which was bound by a package flag.
fromFlag :: ModuleOrigin
fromFlag = ModOrigin Nothing [] [] True

#if __GLASGOW_HASKELL__ > 710
instance Semigroup ModuleOrigin where
    ModOrigin e res rhs f <> ModOrigin e' res' rhs' f' =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    _x <> _y = panic "ModOrigin: hidden module redefined"
#endif

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend (ModOrigin e res rhs f) (ModOrigin e' res' rhs' f') =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    mappend _ _ = panic "ModOrigin: hidden module redefined"

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False

-- | 'UniqFM' map from 'InstalledUnitId'
type InstalledUnitIdMap = UniqDFM

-- | 'UniqFM' map from 'UnitId' to 'PackageConfig', plus
-- the transitive closure of preload packages.
data PackageConfigMap = PackageConfigMap {
        unPackageConfigMap :: InstalledUnitIdMap PackageConfig,
        -- | The set of transitively reachable packages according
        -- to the explicitly provided command line arguments.
        -- See Note [UnitId to InstalledUnitId improvement]
        preloadClosure :: UniqSet InstalledUnitId
    }

-- | 'UniqFM' map from 'UnitId' to a 'UnitVisibility'.
type VisibilityMap = Map UnitId UnitVisibility

-- | 'UnitVisibility' records the various aspects of visibility of a particular
-- 'UnitId'.
data UnitVisibility = UnitVisibility
    { uv_expose_all :: Bool
      --  ^ Should all modules in exposed-modules should be dumped into scope?
    , uv_renamings :: [(ModuleName, ModuleName)]
      -- ^ Any custom renamings that should bring extra 'ModuleName's into
      -- scope.
    , uv_package_name :: First FastString
      -- ^ The package name is associated with the 'UnitId'.  This is used
      -- to implement legacy behavior where @-package foo-0.1@ implicitly
      -- hides any packages named @foo@
    , uv_requirements :: Map ModuleName (Set IndefModule)
      -- ^ The signatures which are contributed to the requirements context
      -- from this unit ID.
    , uv_explicit :: Bool
      -- ^ Whether or not this unit was explicitly brought into scope,
      -- as opposed to implicitly via the 'exposed' fields in the
      -- package database (when @-hide-all-packages@ is not passed.)
    }

instance Outputable UnitVisibility where
    ppr (UnitVisibility {
        uv_expose_all = b,
        uv_renamings = rns,
        uv_package_name = First mb_pn,
        uv_requirements = reqs,
        uv_explicit = explicit
    }) = ppr (b, rns, mb_pn, reqs, explicit)
instance Monoid UnitVisibility where
    mempty = UnitVisibility
             { uv_expose_all = False
             , uv_renamings = []
             , uv_package_name = First Nothing
             , uv_requirements = Map.empty
             , uv_explicit = False
             }
    mappend uv1 uv2
        = UnitVisibility
          { uv_expose_all = uv_expose_all uv1 || uv_expose_all uv2
          , uv_renamings = uv_renamings uv1 ++ uv_renamings uv2
          , uv_package_name = mappend (uv_package_name uv1) (uv_package_name uv2)
          , uv_requirements = Map.unionWith Set.union (uv_requirements uv1) (uv_requirements uv2)
          , uv_explicit = uv_explicit uv1 || uv_explicit uv2
          }

type WiredUnitId = DefUnitId
type PreloadUnitId = InstalledUnitId

-- | Map from 'ModuleName' to 'Module' to all the origins of the bindings
-- in scope.  The 'PackageConf' is not cached, mostly for convenience reasons
-- (since this is the slow path, we'll just look it up again).
type ModuleToPkgConfAll =
    Map ModuleName (Map Module ModuleOrigin)

data PackageState = PackageState {
  -- | A mapping of 'UnitId' to 'PackageConfig'.  This list is adjusted
  -- so that only valid packages are here.  'PackageConfig' reflects
  -- what was stored *on disk*, except for the 'trusted' flag, which
  -- is adjusted at runtime.  (In particular, some packages in this map
  -- may have the 'exposed' flag be 'False'.)
  pkgIdMap              :: PackageConfigMap,

  -- | A mapping of 'PackageName' to 'ComponentId'.  This is used when
  -- users refer to packages in Backpack includes.
  packageNameMap            :: Map PackageName ComponentId,

  -- | A mapping from wired in names to the original names from the
  -- package database.
  unwireMap :: Map WiredUnitId WiredUnitId,

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [PreloadUnitId],

  -- | Packages which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros.
  explicitPackages      :: [UnitId],

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleToPkgConfAll    :: !ModuleToPkgConfAll,

  -- | A map, like 'moduleToPkgConfAll', but controlling plugin visibility.
  pluginModuleToPkgConfAll    :: !ModuleToPkgConfAll,

  -- | A map saying, for each requirement, what interfaces must be merged
  -- together when we use them.  For example, if our dependencies
  -- are @p[A=<A>]@ and @q[A=<A>,B=r[C=<A>]:B]@, then the interfaces
  -- to merge for A are @p[A=<A>]:A@, @q[A=<A>,B=r[C=<A>]:B]:A@
  -- and @r[C=<A>]:C@.
  --
  -- There's an entry in this map for each hole in our home library.
  requirementContext :: Map ModuleName [IndefModule]
  }

emptyPackageState :: PackageState
emptyPackageState = PackageState {
    pkgIdMap = emptyPackageConfigMap,
    packageNameMap = Map.empty,
    unwireMap = Map.empty,
    preloadPackages = [],
    explicitPackages = [],
    moduleToPkgConfAll = Map.empty,
    pluginModuleToPkgConfAll = Map.empty,
    requirementContext = Map.empty
    }

type InstalledPackageIndex = Map InstalledUnitId PackageConfig

-- | Empty package configuration map
emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = PackageConfigMap emptyUDFM emptyUniqSet

-- | Find the package we know about with the given unit id, if any
lookupPackage :: DynFlags -> UnitId -> Maybe PackageConfig
lookupPackage dflags = lookupPackage' (isIndefinite dflags) (pkgIdMap (pkgState dflags))

-- | A more specialized interface, which takes a boolean specifying
-- whether or not to look for on-the-fly renamed interfaces, and
-- just a 'PackageConfigMap' rather than a 'DynFlags' (so it can
-- be used while we're initializing 'DynFlags'
lookupPackage' :: Bool -> PackageConfigMap -> UnitId -> Maybe PackageConfig
lookupPackage' False (PackageConfigMap pkg_map _) uid = lookupUDFM pkg_map uid
lookupPackage' True m@(PackageConfigMap pkg_map _) uid =
    case splitUnitIdInsts uid of
        (iuid, Just indef) ->
            fmap (renamePackage m (indefUnitIdInsts indef))
                 (lookupUDFM pkg_map iuid)
        (_, Nothing) -> lookupUDFM pkg_map uid

{-
-- | Find the indefinite package for a given 'ComponentId'.
-- The way this works is just by fiat'ing that every indefinite package's
-- unit key is precisely its component ID; and that they share uniques.
lookupComponentId :: DynFlags -> ComponentId -> Maybe PackageConfig
lookupComponentId dflags (ComponentId cid_fs) = lookupUDFM pkg_map cid_fs
  where
    PackageConfigMap pkg_map = pkgIdMap (pkgState dflags)
-}

-- | Find the package we know about with the given package name (e.g. @foo@), if any
-- (NB: there might be a locally defined unit name which overrides this)
lookupPackageName :: DynFlags -> PackageName -> Maybe ComponentId
lookupPackageName dflags n = Map.lookup n (packageNameMap (pkgState dflags))

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: DynFlags -> SourcePackageId -> [PackageConfig]
searchPackageId dflags pid = filter ((pid ==) . sourcePackageId)
                               (listPackageConfigMap dflags)

-- | Extends the package configuration map with a list of package configs.
extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap (PackageConfigMap pkg_map closure) new_pkgs
  = PackageConfigMap (foldl add pkg_map new_pkgs) closure
    -- We also add the expanded version of the packageConfigId, so that
    -- 'improveUnitId' can find it.
  where add pkg_map p = addToUDFM (addToUDFM pkg_map (expandedPackageConfigId p) p)
                                  (installedPackageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: DynFlags -> UnitId -> PackageConfig
getPackageDetails dflags pid =
    expectJust "getPackageDetails" (lookupPackage dflags pid)

lookupInstalledPackage :: DynFlags -> InstalledUnitId -> Maybe PackageConfig
lookupInstalledPackage dflags uid = lookupInstalledPackage' (pkgIdMap (pkgState dflags)) uid

lookupInstalledPackage' :: PackageConfigMap -> InstalledUnitId -> Maybe PackageConfig
lookupInstalledPackage' (PackageConfigMap db _) uid = lookupUDFM db uid

getInstalledPackageDetails :: DynFlags -> InstalledUnitId -> PackageConfig
getInstalledPackageDetails dflags uid =
    expectJust "getInstalledPackageDetails" $
        lookupInstalledPackage dflags uid

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, although all packages in this map are "visible", this
-- does not imply that the exposed-modules of the package are available
-- (they may have been thinned or renamed).
listPackageConfigMap :: DynFlags -> [PackageConfig]
listPackageConfigMap dflags = eltsUDFM pkg_map
  where
    PackageConfigMap pkg_map _ = pkgIdMap (pkgState dflags)

-- ----------------------------------------------------------------------------
-- Loading the package db files and building up the package state

-- | Call this after 'DynFlags.parseDynFlags'.  It reads the package
-- database files, and sets up various internal tables of package
-- information, according to the package-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
--
-- Returns a list of packages to link in if we're doing dynamic linking.
-- This list contains the packages that the user explicitly mentioned with
-- @-package@ flags.
--
-- 'initPackages' can be called again subsequently after updating the
-- 'packageFlags' field of the 'DynFlags', and it will update the
-- 'pkgState' in 'DynFlags' and return a list of packages to
-- link in.
initPackages :: DynFlags -> IO (DynFlags, [PreloadUnitId])
initPackages dflags0 = do
  dflags <- interpretPackageEnv dflags0
  pkg_db <-
    case pkgDatabase dflags of
        Nothing -> readPackageConfigs dflags
        Just db -> return $ map (\(p, pkgs)
                                    -> (p, setBatchPackageFlags dflags pkgs)) db
  (pkg_state, preload)
        <- mkPackageState dflags pkg_db []
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state },
          preload)

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO [(FilePath, [PackageConfig])]
readPackageConfigs dflags = do
  conf_refs <- getPackageConfRefs dflags
  confs     <- liftM catMaybes $ mapM (resolvePackageConfig dflags) conf_refs
  mapM (readPackageConfig dflags) confs


getPackageConfRefs :: DynFlags -> IO [PkgConfRef]
getPackageConfRefs dflags = do
  let system_conf_refs = [UserPkgConf, GlobalPkgConf]

  e_pkg_path <- tryIO (getEnv $ map toUpper (programName dflags) ++ "_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | not (null path) && isSearchPathSeparator (last path)
         -> map PkgConfFile (splitSearchPath (init path)) ++ system_conf_refs
         | otherwise
         -> map PkgConfFile (splitSearchPath path)

  return $ reverse (extraPkgConfs dflags base_conf_refs)
  -- later packages shadow earlier ones.  extraPkgConfs
  -- is in the opposite order to the flags on the
  -- command line.

resolvePackageConfig :: DynFlags -> PkgConfRef -> IO (Maybe FilePath)
resolvePackageConfig dflags GlobalPkgConf = return $ Just (systemPackageConfig dflags)
-- NB: This logic is reimplemented in Cabal, so if you change it,
-- make sure you update Cabal.  (Or, better yet, dump it in the
-- compiler info so Cabal can use the info.)
resolvePackageConfig dflags UserPkgConf = runMaybeT $ do
  dir <- versionedAppDir dflags
  let pkgconf = dir </> "package.conf.d"
  exist <- tryMaybeT $ doesDirectoryExist pkgconf
  if exist then return pkgconf else mzero
resolvePackageConfig _ (PkgConfFile name) = return $ Just name

readPackageConfig :: DynFlags -> FilePath -> IO (FilePath, [PackageConfig])
readPackageConfig dflags conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then readDirStylePackageConfig conf_file
       else do
            isfile <- doesFileExist conf_file
            if isfile
               then do
                 mpkgs <- tryReadOldFileStylePackageConfig
                 case mpkgs of
                   Just pkgs -> return pkgs
                   Nothing   -> throwGhcExceptionIO $ InstallationError $
                      "ghc no longer supports single-file style package " ++
                      "databases (" ++ conf_file ++
                      ") use 'ghc-pkg init' to create the database with " ++
                      "the correct format."
               else throwGhcExceptionIO $ InstallationError $
                      "can't find a package database at " ++ conf_file

  let
      top_dir = topDir dflags
      pkgroot = takeDirectory conf_file
      pkg_configs1 = map (mungePackagePaths top_dir pkgroot) proto_pkg_configs
      pkg_configs2 = setBatchPackageFlags dflags pkg_configs1
  --
  return (conf_file, pkg_configs2)
  where
    readDirStylePackageConfig conf_dir = do
      let filename = conf_dir </> "package.cache"
      debugTraceMsg dflags 2 (text "Using binary package database:" <+> text filename)
      readPackageDbForGhc filename

    -- Single-file style package dbs have been deprecated for some time, but
    -- it turns out that Cabal was using them in one place. So this is a
    -- workaround to allow older Cabal versions to use this newer ghc.
    -- We check if the file db contains just "[]" and if so, we look for a new
    -- dir-style db in conf_file.d/, ie in a dir next to the given file.
    -- We cannot just replace the file with a new dir style since Cabal still
    -- assumes it's a file and tries to overwrite with 'writeFile'.
    -- ghc-pkg also cooperates with this workaround.
    tryReadOldFileStylePackageConfig = do
      content <- readFile conf_file `catchIO` \_ -> return ""
      if take 2 content == "[]"
        then do
          let conf_dir = conf_file <.> "d"
          direxists <- doesDirectoryExist conf_dir
          if direxists
             then do debugTraceMsg dflags 2 (text "Ignoring old file-style db and trying:" <+> text conf_dir)
                     liftM Just (readDirStylePackageConfig conf_dir)
             else return (Just []) -- ghc-pkg will create it when it's updated
        else return Nothing

setBatchPackageFlags :: DynFlags -> [PackageConfig] -> [PackageConfig]
setBatchPackageFlags dflags pkgs = maybeDistrustAll pkgs
  where
    maybeDistrustAll pkgs'
      | gopt Opt_DistrustAllPackages dflags = map distrust pkgs'
      | otherwise                           = pkgs'

    distrust pkg = pkg{ trusted = False }

-- TODO: This code is duplicated in utils/ghc-pkg/Main.hs
mungePackagePaths :: FilePath -> FilePath -> PackageConfig -> PackageConfig
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths top_dir pkgroot pkg =
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      haddockHTMLs = munge_urls (haddockHTMLs pkg)
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url

    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing


-- -----------------------------------------------------------------------------
-- Modify our copy of the package database based on trust flags,
-- -trust and -distrust.

applyTrustFlag
   :: DynFlags
   -> UnusablePackages
   -> [PackageConfig]
   -> TrustFlag
   -> IO [PackageConfig]
applyTrustFlag dflags unusable pkgs flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {trusted=True}

    DistrustPackage str ->
       case selectPackages (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map distrust ps ++ qs)
          where distrust p = p {trusted=False}

-- | A little utility to tell if the 'thisPackage' is indefinite
-- (if it is not, we should never use on-the-fly renaming.)
isIndefinite :: DynFlags -> Bool
isIndefinite dflags = not (unitIdIsDefinite (thisPackage dflags))

applyPackageFlag
   :: DynFlags
   -> PackageConfigMap
   -> UnusablePackages
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [PackageConfig]
   -> VisibilityMap           -- Initially exposed
   -> PackageFlag               -- flag to apply
   -> IO VisibilityMap        -- Now exposed

applyPackageFlag dflags pkg_db unusable no_hide_others pkgs vm flag =
  case flag of
    ExposePackage _ arg (ModRenaming b rns) ->
       case findPackages pkg_db arg pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:_) -> return vm'
          where
           n = fsPackageName p

           -- If a user says @-unit-id p[A=<A>]@, this imposes
           -- a requirement on us: whatever our signature A is,
           -- it must fulfill all of p[A=<A>]:A's requirements.
           -- This method is responsible for computing what our
           -- inherited requirements are.
           reqs | UnitIdArg orig_uid <- arg = collectHoles orig_uid
                | otherwise                 = Map.empty

           collectHoles uid = case splitUnitIdInsts uid of
                (_, Just indef) ->
                  let local = [ Map.singleton
                                  (moduleName mod)
                                  (Set.singleton $ IndefModule indef mod_name)
                              | (mod_name, mod) <- indefUnitIdInsts indef
                              , isHoleModule mod ]
                      recurse = [ collectHoles (moduleUnitId mod)
                                | (_, mod) <- indefUnitIdInsts indef ]
                  in Map.unionsWith Set.union $ local ++ recurse
                -- Other types of unit identities don't have holes
                (_, Nothing) -> Map.empty


           uv = UnitVisibility
                { uv_expose_all = b
                , uv_renamings = rns
                , uv_package_name = First (Just n)
                , uv_requirements = reqs
                , uv_explicit = True
                }
           vm' = Map.insertWith mappend (packageConfigId p) uv vm_cleared
           -- In the old days, if you said `ghc -package p-0.1 -package p-0.2`
           -- (or if p-0.1 was registered in the pkgdb as exposed: True),
           -- the second package flag would override the first one and you
           -- would only see p-0.2 in exposed modules.  This is good for
           -- usability.
           --
           -- However, with thinning and renaming (or Backpack), there might be
           -- situations where you legitimately want to see two versions of a
           -- package at the same time, and this behavior would make it
           -- impossible to do so.  So we decided that if you pass
           -- -hide-all-packages, this should turn OFF the overriding behavior
           -- where an exposed package hides all other packages with the same
           -- name.  This should not affect Cabal at all, which only ever
           -- exposes one package at a time.
           --
           -- NB: Why a variable no_hide_others?  We have to apply this logic to
           -- -plugin-package too, and it's more consistent if the switch in
           -- behavior is based off of
           -- -hide-all-packages/-hide-all-plugin-packages depending on what
           -- flag is in question.
           vm_cleared | no_hide_others = vm
                      -- NB: renamings never clear
                      | (_:_) <- rns = vm
                      | otherwise = Map.filterWithKey
                            (\k uv -> k == packageConfigId p
                                   || First (Just n) /= uv_package_name uv) vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case findPackages pkg_db (PackageArg str) pkgs unusable of
         Left ps  -> packageFlagErr dflags flag ps
         Right ps -> return vm'
          where vm' = foldl' (flip Map.delete) vm (map packageConfigId ps)

-- | Like 'selectPackages', but doesn't return a list of unmatched
-- packages.  Furthermore, any packages it returns are *renamed*
-- if the 'UnitArg' has a renaming associated with it.
findPackages :: PackageConfigMap -> PackageArg -> [PackageConfig]
             -> UnusablePackages
             -> Either [(PackageConfig, UnusablePackageReason)]
                [PackageConfig]
findPackages pkg_db arg pkgs unusable
  = let ps = mapMaybe (finder arg) pkgs
    in if null ps
        then Left (mapMaybe (\(x,y) -> finder arg x >>= \x' -> return (x',y))
                            (Map.elems unusable))
        else Right (sortByVersion (reverse ps))
  where
    finder (PackageArg str) p
      = if str == sourcePackageIdString p || str == packageNameString p
          then Just p
          else Nothing
    finder (UnitIdArg uid) p
      = let (iuid, mb_indef) = splitUnitIdInsts uid
        in if iuid == installedPackageConfigId p
              then Just (case mb_indef of
                            Nothing    -> p
                            Just indef -> renamePackage pkg_db (indefUnitIdInsts indef) p)
              else Nothing

selectPackages :: PackageArg -> [PackageConfig]
               -> UnusablePackages
               -> Either [(PackageConfig, UnusablePackageReason)]
                  ([PackageConfig], [PackageConfig])
selectPackages arg pkgs unusable
  = let matches = matching arg
        (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        -- NB: packages from later package databases are LATER
        -- in the list.  We want to prefer the latest package.
        else Right (sortByVersion (reverse ps), rest)

-- | Rename a 'PackageConfig' according to some module instantiation.
renamePackage :: PackageConfigMap -> [(ModuleName, Module)]
              -> PackageConfig -> PackageConfig
renamePackage pkg_map insts conf =
    let hsubst = listToUFM insts
        smod  = renameHoleModule' pkg_map hsubst
        new_insts = map (\(k,v) -> (k,smod v)) (instantiatedWith conf)
    in conf {
        instantiatedWith = new_insts,
        exposedModules = map (\(mod_name, mb_mod) -> (mod_name, fmap smod mb_mod))
                             (exposedModules conf)
    }


-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> PackageConfig -> Bool
matchingStr str p
        =  str == sourcePackageIdString p
        || str == packageNameString p

matchingId :: InstalledUnitId -> PackageConfig -> Bool
matchingId uid p = uid == installedPackageConfigId p

matching :: PackageArg -> PackageConfig -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg (DefiniteUnitId (DefUnitId uid)))  = matchingId uid
matching (UnitIdArg _)  = \_ -> False -- TODO: warn in this case

sortByVersion :: [PackageConfig] -> [PackageConfig]
sortByVersion = sortBy (flip (comparing packageVersion))

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a

-- for missing DPH package we emit a more helpful error message, because
-- this may be the result of using -fdph-par or -fdph-seq.
packageFlagErr dflags (ExposePackage _ (PackageArg pkg) _) []
  | is_dph_package pkg
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ dph_err))
  where dph_err = text "the " <> text pkg <> text " package is not installed."
                  $$ text "To install it: \"cabal install dph\"."
        is_dph_package pkg = "dph" `isPrefixOf` pkg
packageFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprFlag flag) reasons

trustFlagErr :: DynFlags
             -> TrustFlag
             -> [(PackageConfig, UnusablePackageReason)]
             -> IO a
trustFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprTrustFlag flag) reasons

packageFlagErr' :: DynFlags
               -> SDoc
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a
packageFlagErr' dflags flag_doc reasons
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ err))
  where err = text "cannot satisfy " <> flag_doc <>
                (if null reasons then Outputable.empty else text ": ") $$
              nest 4 (ppr_reasons $$
                      text "(use -v for more information)")
        ppr_reasons = vcat (map ppr_reason reasons)
        ppr_reason (p, reason) =
            pprReason (ppr (unitId p) <+> text "is") reason

pprFlag :: PackageFlag -> SDoc
pprFlag flag = case flag of
    HidePackage p   -> text "-hide-package " <> text p
    ExposePackage doc _ _ -> text doc

pprTrustFlag :: TrustFlag -> SDoc
pprTrustFlag flag = case flag of
    TrustPackage p    -> text "-trust " <> text p
    DistrustPackage p -> text "-distrust " <> text p

-- -----------------------------------------------------------------------------
-- Wired-in packages

wired_in_pkgids :: [String]
wired_in_pkgids = map unitIdString wiredInUnitIds

type WiredPackagesMap = Map WiredUnitId WiredUnitId

findWiredInPackages
   :: DynFlags
   -> [PackageConfig]           -- database
   -> VisibilityMap             -- info on what packages are visible
                                -- for wired in selection
   -> IO ([PackageConfig],  -- package database updated for wired in
          WiredPackagesMap) -- map from unit id to wired identity

findWiredInPackages dflags pkgs vis_map = do
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
        matches :: PackageConfig -> String -> Bool
        pc `matches` pid = packageNameString pc == pid

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we try to pick the latest version of exposed packages.
        -- However, if there are no exposed wired in packages available
        -- (e.g. -hide-all-packages was used), we can't bail: we *have*
        -- to assign a package for the wired-in package: so we try again
        -- with hidden packages included to (and pick the latest
        -- version).
        --
        -- You can also override the default choice by using -ignore-package:
        -- this works even when there is no exposed wired in package
        -- available.
        --
        findWiredInPackage :: [PackageConfig] -> String
                           -> IO (Maybe PackageConfig)
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
               all_exposed_ps =
                    [ p | p <- all_ps
                        , Map.member (packageConfigId p) vis_map ] in
           case all_exposed_ps of
            [] -> case all_ps of
                       []   -> notfound
                       many -> pick (head (sortByVersion many))
            many -> pick (head (sortByVersion many))
          where
                notfound = do
                          debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " not found."
                          return Nothing
                pick :: PackageConfig
                     -> IO (Maybe PackageConfig)
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (Just pkg)


  mb_wired_in_pkgs <- mapM (findWiredInPackage pkgs) wired_in_pkgids
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs
        wired_in_ids = mapMaybe definitePackageConfigId wired_in_pkgs

        -- this is old: we used to assume that if there were
        -- multiple versions of wired-in packages installed that
        -- they were mutually exclusive.  Now we're assuming that
        -- you have one "main" version of each wired-in package
        -- (the latest version), and the others are backward-compat
        -- wrappers that depend on this one.  e.g. base-4.0 is the
        -- latest, base-3.0 is a compat wrapper depending on base-4.0.
        {-
        deleteOtherWiredInPackages pkgs = filterOut bad pkgs
          where bad p = any (p `matches`) wired_in_pkgids
                      && package p `notElem` map fst wired_in_ids
        -}

        wiredInMap :: Map WiredUnitId WiredUnitId
        wiredInMap = foldl' add_mapping Map.empty pkgs
          where add_mapping m pkg
                  | Just key <- definitePackageConfigId pkg
                  , key `elem` wired_in_ids
                  = Map.insert key (DefUnitId (stringToInstalledUnitId (packageNameString pkg))) m
                  | otherwise = m

        updateWiredInDependencies pkgs = map (upd_deps . upd_pkg) pkgs
          where upd_pkg pkg
                  | Just def_uid <- definitePackageConfigId pkg
                  , def_uid `elem` wired_in_ids
                  = let PackageName fs = packageName pkg
                    in pkg {
                      unitId = fsToInstalledUnitId fs,
                      componentId = ComponentId fs
                    }
                  | otherwise
                  = pkg
                upd_deps pkg = pkg {
                      -- temporary harmless DefUnitId invariant violation
                      depends = map (unDefUnitId . upd_wired_in . DefUnitId) (depends pkg),
                      exposedModules
                        = map (\(k,v) -> (k, fmap upd_wired_in_mod v))
                              (exposedModules pkg)
                    }
                upd_wired_in_mod (Module uid m) = Module (upd_wired_in_uid uid) m
                upd_wired_in_uid (DefiniteUnitId def_uid) =
                    DefiniteUnitId (upd_wired_in def_uid)
                upd_wired_in_uid (IndefiniteUnitId indef_uid) =
                    IndefiniteUnitId $ newIndefUnitId
                        (indefUnitIdComponentId indef_uid)
                        (map (\(x,y) -> (x,upd_wired_in_mod y)) (indefUnitIdInsts indef_uid))
                upd_wired_in key
                    | Just key' <- Map.lookup key wiredInMap = key'
                    | otherwise = key


  return (updateWiredInDependencies pkgs, wiredInMap)

updateVisibilityMap :: WiredPackagesMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (Map.toList wiredInMap)
  where f vm (from, to) = case Map.lookup (DefiniteUnitId from) vis_map of
                    Nothing -> vm
                    Just r -> Map.insert (DefiniteUnitId to) r
                                (Map.delete (DefiniteUnitId from) vm)


-- ----------------------------------------------------------------------------

type IsShadowed = Bool
data UnusablePackageReason
  = IgnoredWithFlag
  | MissingDependencies IsShadowed [InstalledUnitId]
instance Outputable UnusablePackageReason where
    ppr IgnoredWithFlag = text "[ignored with flag]"
    ppr (MissingDependencies b uids) =
        brackets (if b then text "shadowed" else empty <+> ppr uids)

type UnusablePackages = Map InstalledUnitId
                            (PackageConfig, UnusablePackageReason)

pprReason :: SDoc -> UnusablePackageReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> text "ignored due to an -ignore-package flag"
  MissingDependencies is_shadowed deps ->
      pref <+> text "unusable due to"
           <+> (if is_shadowed then text "shadowed"
                               else text "missing or recursive")
           <+> text "dependencies:" $$
        nest 2 (hsep (map ppr deps))

reportUnusable :: DynFlags -> UnusablePackages -> IO ()
reportUnusable dflags pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, (_, reason)) =
       debugTraceMsg dflags 2 $
         pprReason
           (text "package" <+> ppr ipid <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Detect any packages that have missing dependencies, and also any
-- mutually-recursive groups of packages (loops in the package graph
-- are not allowed).  We do this by taking the least fixpoint of the
-- dependency graph, repeatedly adding packages whose dependencies are
-- satisfied until no more can be added.
--
findBroken :: IsShadowed
           -> [PackageConfig]
           -> Map InstalledUnitId PackageConfig
           -> UnusablePackages
findBroken is_shadowed pkgs pkg_map0 = go [] pkg_map0 pkgs
 where
   go avail pkg_map not_avail =
     case partitionWith (depsAvailable pkg_map) not_avail of
        ([], not_avail) ->
            Map.fromList [ (unitId p, (p, MissingDependencies is_shadowed deps))
                         | (p,deps) <- not_avail ]
        (new_avail, not_avail) ->
            go (new_avail ++ avail) pkg_map' (map fst not_avail)
            where pkg_map' = Map.insertList
                             [ (unitId p, p) | p <- new_avail ]
                             pkg_map

   depsAvailable :: InstalledPackageIndex
                 -> PackageConfig
                 -> Either PackageConfig (PackageConfig, [InstalledUnitId])
   depsAvailable pkg_map pkg
        | null dangling = Left pkg
        | otherwise     = Right (pkg, dangling)
        where dangling = filter (not . (`Map.member` pkg_map)) (depends pkg)

-- -----------------------------------------------------------------------------
-- Ignore packages

ignorePackages :: [IgnorePackageFlag] -> [PackageConfig] -> UnusablePackages
ignorePackages flags pkgs = Map.fromList (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (unitId p, (p, IgnoredWithFlag))
                    | p <- ps ]
        -- missing package is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -> [(FilePath, [PackageConfig])]     -- initial databases
    -> [PreloadUnitId]              -- preloaded packages
    -> IO (PackageState,
           [PreloadUnitId])         -- new packages to preload

mkPackageState dflags dbs preload0 = do
  -- Compute the unit id
  let this_package = thisPackage dflags

{-
   Plan.

   There are two main steps for making the package state:

    1. We want to build a single, unified package database based
       on all of the input databases, which upholds the invariant that
       there is only one package per any UnitId, and that there are no
       dangling dependencies. We'll do this by successively merging each
       input database into this unified database:

       a) if an input database defines unit ID that is already in
          the unified database, that package SHADOWS the existing
          package in the current unified database
            * for every such shadowed package, we remove it and any
              packages which transitively depend on it from the
              unified datbase

       b) remove packages selected by -ignore-package from input database

       c) remove any packages with missing dependencies or mutually recursive
          dependencies from the input database

       d) report (with -v) any packages that were removed by steps 1-3

       e) merge the input database into the unified database

    2. We want to look at the flags controlling package visibility,
       and build a mapping of what module names are in scope and
       where they live.

       a) on the final, unified database, we apply -trust/-distrust
          flags directly, modifying the database so that the 'trusted'
          field has the correct value.

       b) we use the -package/-hide-package flags to compute a
          visibility map, stating what packages are "exposed" for
          the purposes of computing the module map.
          * if any flag refers to a package which was removed by 1-5, then
            we can give an error message explaining why
          * if -hide-all-packages what not specified, this step also
            hides packages which are superseded by later exposed packages
          * this step is done TWICE if -plugin-package/-hide-all-plugin-packages
            are used

       c) based on the visibility map, we pick wired packages and rewrite
          them to have the expected unitId.

       d) finally, using the visibility map and the package database,
          we build a mapping saying what every in scope module name points to.
-}

  let other_flags = reverse (packageFlags dflags)
      ignore_flags = reverse (ignorePackageFlags dflags)
  debugTraceMsg dflags 2 $
      text "package flags" <+> ppr other_flags

  let merge (pkg_map, prev_unusable) (db_path, db) = do
            debugTraceMsg dflags 2 $
                text "loading package database" <+> text db_path
            forM_ (Set.toList shadow_set) $ \pkg ->
                debugTraceMsg dflags 2 $
                    text "package" <+> ppr pkg <+>
                    text "shadows a previously defined package"
            reportUnusable dflags unusable
            -- NB: an unusable unit ID can become usable again
            -- if it's validly specified in a later package stack.
            -- Keep unusable up-to-date!
            return (pkg_map', (prev_unusable `Map.difference` pkg_map')
                                    `Map.union` unusable)
        where -- The set of UnitIds which appear in both
              -- db and pkgs (to be shadowed from pkgs)
              shadow_set :: Set InstalledUnitId
              shadow_set = foldr ins Set.empty db
                where ins pkg s
                        -- If the package from the upper database is
                        -- in the lower database, and the ABIs don't
                        -- match...
                        | Just old_pkg <- Map.lookup (unitId pkg) pkg_map
                        , abiHash old_pkg /= abiHash pkg
                        -- ...add this unit ID to the set of unit IDs
                        -- which (transitively) should be shadowed from
                        -- the lower database.
                        = Set.insert (unitId pkg) s
                        | otherwise
                        = s
              -- Remove shadow_set from pkg_map...
              shadowed_pkgs0 :: [PackageConfig]
              shadowed_pkgs0 = filter (not . (`Set.member` shadow_set) . unitId)
                                      (Map.elems pkg_map)
              -- ...and then remove anything transitively broken
              -- this way.
              shadowed = findBroken True shadowed_pkgs0 Map.empty
              shadowed_pkgs :: [PackageConfig]
              shadowed_pkgs = filter (not . (`Map.member` shadowed) . unitId)
                                     shadowed_pkgs0

              -- Apply ignore flags to db (TODO: could extend command line
              -- flag format to support per-database ignore now!  More useful
              -- than what we have now.)
              ignored = ignorePackages ignore_flags db
              db2 = filter (not . (`Map.member` ignored) . unitId) db

              -- Look for broken packages (either from ignore, or possibly
              -- because the db was broken to begin with)
              mk_pkg_map = Map.fromList . map (\p -> (unitId p, p))
              broken = findBroken False db2 (mk_pkg_map shadowed_pkgs)
              db3 = filter (not . (`Map.member` broken) . unitId) db2

              unusable = shadowed `Map.union` ignored
                                  `Map.union` broken

              -- Now merge the sets together (NB: later overrides
              -- earlier!)
              pkg_map' :: Map InstalledUnitId PackageConfig
              pkg_map' = mk_pkg_map (shadowed_pkgs ++ db3)

  (pkg_map1, unusable) <- foldM merge (Map.empty, Map.empty) dbs
  -- Apply trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  pkgs1 <- foldM (applyTrustFlag dflags unusable)
                 (Map.elems pkg_map1) (reverse (trustFlags dflags))
  let prelim_pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs1

  --
  -- Calculate the initial set of packages, prior to any package flags.
  -- This set contains the latest version of all valid (not unusable) packages,
  -- or is empty if we have -hide-all-packages
  --
  let preferLater pkg pkg' =
        case comparing packageVersion pkg pkg' of
            GT -> pkg
            _  -> pkg'
      calcInitial m pkg = addToUDFM_C preferLater m (fsPackageName pkg) pkg
      initial = if gopt Opt_HideAllPackages dflags
                    then emptyUDFM
                    else foldl' calcInitial emptyUDFM pkgs1
      vis_map1 = foldUDFM (\p vm ->
                            -- Note: we NEVER expose indefinite packages by
                            -- default, because it's almost assuredly not
                            -- what you want (no mix-in linking has occurred).
                            if exposed p && unitIdIsDefinite (packageConfigId p)
                               then Map.insert (packageConfigId p)
                                               UnitVisibility {
                                                 uv_expose_all = True,
                                                 uv_renamings = [],
                                                 uv_package_name = First (Just (fsPackageName p)),
                                                 uv_requirements = Map.empty,
                                                 uv_explicit = False
                                               }
                                               vm
                               else vm)
                         Map.empty initial

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- foldM (applyPackageFlag dflags prelim_pkg_db unusable
                        (gopt Opt_HideAllPackages dflags) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  (pkgs2, wired_map) <- findWiredInPackages dflags pkgs1 vis_map2
  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = gopt Opt_HideAllPluginPackages dflags
  plugin_vis_map <-
    case pluginPackageFlags dflags of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return Map.empty
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = Map.empty
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- foldM (applyPackageFlag dflags prelim_pkg_db unusable
                                (gopt Opt_HideAllPluginPackages dflags) pkgs1)
                             plugin_vis_map1
                             (reverse (pluginPackageFlags dflags))
                -- Updating based on wired in packages is mostly
                -- good hygiene, because it won't matter: no wired in
                -- package has a compiler plugin.
                -- TODO: If a wired in package had a compiler plugin,
                -- and you tried to pick different wired in packages
                -- with the plugin flags and the normal flags... what
                -- would happen?  I don't know!  But this doesn't seem
                -- likely to actually happen.
                return (updateVisibilityMap wired_map plugin_vis_map2)

  --
  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  -- NB: preload IS important even for type-checking, because we
  -- need the correct include path to be set.
  --
  let preload1 = Map.keys (Map.filter uv_explicit vis_map)

  let pkgname_map = foldl add Map.empty pkgs2
        where add pn_map p
                = Map.insert (packageName p) (componentId p) pn_map

  -- The explicitPackages accurately reflects the set of packages we have turned
  -- on; as such, it also is the only way one can come up with requirements.
  -- The requirement context is directly based off of this: we simply
  -- look for nested unit IDs that are directly fed holes: the requirements
  -- of those units are precisely the ones we need to track
  let explicit_pkgs = Map.keys vis_map
      req_ctx = Map.map (Set.toList)
              $ Map.unionsWith Set.union (map uv_requirements (Map.elems vis_map))


  let preload2 = preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUDFM (unPackageConfigMap pkg_db))
                [baseUnitId, rtsUnitId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload3 = nub $ filter (/= this_package)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db (zip (map toInstalledUnitId preload3) (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let mod_map = mkModuleToPkgConfAll dflags pkg_db vis_map
  when (dopt Opt_D_dump_mod_map dflags) $
      printInfoForUser (dflags { pprCols = 200 })
                       alwaysQualify (pprModuleMap mod_map)

  -- Force pstate to avoid leaking the dflags0 passed to mkPackageState
  let !pstate = PackageState{
    preloadPackages     = dep_preload,
    explicitPackages    = explicit_pkgs,
    pkgIdMap            = pkg_db,
    moduleToPkgConfAll  = mod_map,
    pluginModuleToPkgConfAll = mkModuleToPkgConfAll dflags pkg_db plugin_vis_map,
    packageNameMap          = pkgname_map,
    unwireMap = Map.fromList [ (v,k) | (k,v) <- Map.toList wired_map ],
    requirementContext = req_ctx
    }
  return (pstate, new_dep_preload)

-- | Given a wired-in 'UnitId', "unwire" it into the 'UnitId'
-- that it was recorded as in the package database.
unwireUnitId :: DynFlags -> UnitId -> UnitId
unwireUnitId dflags uid@(DefiniteUnitId def_uid) =
    maybe uid DefiniteUnitId (Map.lookup def_uid (unwireMap (pkgState dflags)))
unwireUnitId _ uid = uid

-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

-- Slight irritation: we proceed by leafing through everything
-- in the installed package database, which makes handling indefinite
-- packages a bit bothersome.

mkModuleToPkgConfAll
  :: DynFlags
  -> PackageConfigMap
  -> VisibilityMap
  -> ModuleToPkgConfAll
mkModuleToPkgConfAll dflags pkg_db vis_map =
    Map.foldlWithKey extend_modmap emptyMap vis_map
 where
  emptyMap = Map.empty
  sing pk m _ = Map.singleton (mkModule pk m)
  addListTo = foldl' merge
  merge m (k, v) = MapStrict.insertWith (Map.unionWith mappend) k v m
  setOrigins m os = fmap (const os) m
  extend_modmap modmap uid
    UnitVisibility { uv_expose_all = b, uv_renamings = rns }
    = addListTo modmap theBindings
   where
    pkg = pkg_lookup uid

    theBindings :: [(ModuleName, Map Module ModuleOrigin)]
    theBindings = newBindings b rns

    newBindings :: Bool
                -> [(ModuleName, ModuleName)]
                -> [(ModuleName, Map Module ModuleOrigin)]
    newBindings e rns  = es e ++ hiddens ++ map rnBinding rns

    rnBinding :: (ModuleName, ModuleName)
              -> (ModuleName, Map Module ModuleOrigin)
    rnBinding (orig, new) = (new, setOrigins origEntry fromFlag)
     where origEntry = case lookupUFM esmap orig of
            Just r -> r
            Nothing -> throwGhcException (CmdLineError (showSDoc dflags
                        (text "package flag: could not find module name" <+>
                            ppr orig <+> text "in package" <+> ppr pk)))

    es :: Bool -> [(ModuleName, Map Module ModuleOrigin)]
    es e = do
     (m, exposedReexport) <- exposed_mods
     let (pk', m', pkg', origin') =
          case exposedReexport of
           Nothing -> (pk, m, pkg, fromExposedModules e)
           Just (Module pk' m') ->
            let pkg' = pkg_lookup pk'
            in (pk', m', pkg', fromReexportedModules e pkg')
     return (m, sing pk' m' pkg' origin')

    esmap :: UniqFM (Map Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, sing pk m pkg ModHidden) | m <- hidden_mods]

    pk = packageConfigId pkg
    pkg_lookup uid = lookupPackage' (isIndefinite dflags) pkg_db uid
                        `orElse` pprPanic "pkg_lookup" (ppr uid)

    exposed_mods = exposedModules pkg
    hidden_mods = hiddenModules pkg

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Find all the include directories in these and the preload packages
getPackageIncludePath :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageLibraryPath dflags pkgs = do
  pkg_configs <- getPreloadPackagesAnd dflags pkgs
  let pkg_paths = collectLibraryPaths pkg_configs

      -- For the rts package we have to now correct and point the build
      -- system to the proper location of the libraries
      ways0 = ways dflags

      ways1 = filter (/= WayDyn) ways0
      -- the name of a shared library is libHSfoo-ghc<version>.so
      -- except for the RTS libs, which follow the naming scheme
      -- rts/ghc<version>/<way>/libHSrts.so)
      -- we leave out the _dyn, because it is superfluous

      -- debug RTS includes support for -eventlog
      ways2 | WayDebug `elem` ways1
            = filter (/= WayEventLog) ways1
            | otherwise
            = ways1

      rts_tag = mkBuildTag ways2

      rts_paths = concat $ (collectRtsLibraryPaths dflags rts_tag) `mapMaybe` pkg_configs
  return $ pkg_paths ++ rts_paths

collectLibraryPaths :: [PackageConfig] -> [FilePath]
collectLibraryPaths ps = nub (filter notNull (concatMap libraryDirs ps))

collectRtsLibraryPaths :: DynFlags -> String -> PackageConfig -> Maybe [FilePath]
collectRtsLibraryPaths dflags rts_tag pkgConfig
  = if packageConfigId pkgConfig == rtsUnitId
       then map mkPath $ libraryDirs pkgConfig -- This produces lots of directories that don't exist.
       else []
  where mkPath :: FilePath -> FilePath
        mkPath base = base FilePath.</> "rts"
                           FilePath.</> ("ghc" ++ projectVersion dflags)
                           FilePath.</> rts_tag

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [PreloadUnitId] -> IO ([String], [String], [String])
getPackageLinkOpts dflags pkgs = do
  pkg_configs <- getPreloadPackagesAnd dflags pkgs
  let (hs, extra, other) = collectLinkOpts dflags pkg_configs
  -- The Dynamic and the Static RTS files are in the same folder and have the
  -- same names. So we want to help ld pick the right one depending on if we're
  -- linking statically or dynamically.
  -- This is especially important on Windows since otherwise we would always
  -- use the dynamically linked variant.
  let rtsLdOptions = nub $ concat $ (collectRtsLinkOpts dflags) `map` pkg_configs
  return (hs, extra, other ++ rtsLdOptions)

collectLinkOpts :: DynFlags -> [PackageConfig] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . extraLibraries) ps,
        concatMap ldOptions ps
    )

collectRtsLinkOpts :: DynFlags -> PackageConfig -> [String]
collectRtsLinkOpts dflags pkgConfig
  = if    packageConfigId pkgConfig == rtsUnitId
       && WayDyn `notElem` ways dflags
#if defined(mingw32_HOST_OS)
      -- If we're statically linking packages then
      -- tell the linker so we also statically link the RTS
      -- This may be a bit of a big hammer as it affects
      -- user programs as well. Instead should look into
      -- specifying the full lib name to -l to disambiguate. 
      then ["-Wl,-static"]
#else
      then []
#endif
      else []

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (hsLibraries p)
  where
        ways0   = filterRtsWays $ ways dflags
        tag     = mkBuildTag (filter (not . wayRTSOnly) ways0)
        rts_tag = mkBuildTag ways0

        mkDynName x
         | WayDyn `notElem` ways dflags = x
         | "rts" `isSuffixOf` x         = x
         | "HS"  `isPrefixOf` x         =
              x ++ '-':programName dflags ++ projectVersion dflags
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ x)

        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap frameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllPackages :: DynFlags
                          -> ModuleName
                          -> [(Module, PackageConfig)]
lookupModuleInAllPackages dflags m
  = case lookupModuleWithSuggestions dflags m Nothing of
      LookupFound a b -> [(a,b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust "lookupModule" (lookupPackage dflags
                                                         (moduleUnitId m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module PackageConfig
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, ModuleOrigin)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [(Module, ModuleOrigin)] [(Module, ModuleOrigin)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [ModuleSuggestion] -- suggestions

data ModuleSuggestion = SuggestVisible ModuleName Module ModuleOrigin
                      | SuggestHidden ModuleName Module ModuleOrigin

lookupModuleWithSuggestions :: DynFlags
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions dflags
  = lookupModuleWithSuggestions' dflags
        (moduleToPkgConfAll (pkgState dflags))

lookupPluginModuleWithSuggestions :: DynFlags
                                  -> ModuleName
                                  -> Maybe FastString
                                  -> LookupResult
lookupPluginModuleWithSuggestions dflags
  = lookupModuleWithSuggestions' dflags
        (pluginModuleToPkgConfAll (pkgState dflags))

lookupModuleWithSuggestions' :: DynFlags
                            -> ModuleToPkgConfAll
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions' dflags mod_map m mb_pn
  = case Map.lookup m mod_map of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[]) (Map.toList xs) of
            ([], [], []) -> LookupNotFound suggestions
            (_, _, [(m, _)])             -> LookupFound m (mod_pkg m)
            (_, _, exposed@(_:_))        -> LookupMultiple exposed
            (hidden_pkg, hidden_mod, []) -> LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_pkg m) origin0
          x = (m, origin)
      in case origin of
          ModHidden                  -> (hidden_pkg,   x:hidden_mod, exposed)
          _ | originEmpty origin     -> (hidden_pkg,   hidden_mod,   exposed)
            | originVisible origin   -> (hidden_pkg,   hidden_mod,   x:exposed)
            | otherwise              -> (x:hidden_pkg, hidden_mod,   exposed)

    pkg_lookup p = lookupPackage dflags p `orElse` pprPanic "lookupModuleWithSuggestions" (ppr p <+> ppr m)
    mod_pkg = pkg_lookup . moduleUnitId

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: Maybe FastString
                 -> PackageConfig
                 -> ModuleOrigin
                 -> ModuleOrigin
    filterOrigin Nothing _ o = o
    filterOrigin (Just pn) pkg o =
      case o of
          ModHidden -> if go pkg then ModHidden else mempty
          ModOrigin { fromOrigPackage = e, fromExposedReexport = res,
                      fromHiddenReexport = rhs }
            -> ModOrigin {
                  fromOrigPackage = if go pkg then e else Nothing
                , fromExposedReexport = filter go res
                , fromHiddenReexport = filter go rhs
                , fromPackageFlag = False -- always excluded
                }
      where go pkg = pn == fsPackageName pkg

    suggestions
      | gopt Opt_HelpfulErrors dflags =
           fuzzyLookup (moduleNameString m) all_mods
      | otherwise = []

    all_mods :: [(String, ModuleSuggestion)]     -- All modules
    all_mods = sortBy (comparing fst) $
        [ (moduleNameString m, suggestion)
        | (m, e) <- Map.toList (moduleToPkgConfAll (pkgState dflags))
        , suggestion <- map (getSuggestion m) (Map.toList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames dflags =
    map fst (filter visible (Map.toList (moduleToPkgConfAll (pkgState dflags))))
  where visible (_, ms) = any originVisible (Map.elems ms)

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PreloadUnitId] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package dflags pkg_map) preload pairs)
  return (map (getInstalledPackageDetails dflags) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> [(InstalledUnitId, Maybe InstalledUnitId)]
          -> IO [InstalledUnitId]
closeDeps dflags pkg_map ps
    = throwErr dflags (closeDepsErr dflags pkg_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: DynFlags
             -> PackageConfigMap
             -> [(InstalledUnitId,Maybe InstalledUnitId)]
             -> MaybeErr MsgDoc [InstalledUnitId]
closeDepsErr dflags pkg_map ps = foldM (add_package dflags pkg_map) [] ps

-- internal helper
add_package :: DynFlags
            -> PackageConfigMap
            -> [PreloadUnitId]
            -> (PreloadUnitId,Maybe PreloadUnitId)
            -> MaybeErr MsgDoc [PreloadUnitId]
add_package dflags pkg_db ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupInstalledPackage' pkg_db p of
        Nothing -> Failed (missingPackageMsg p <>
                           missingDependencyMsg mb_parent)
        Just pkg -> do
           -- Add the package's dependents also
           ps' <- foldM add_unit_key ps (depends pkg)
           return (p : ps')
          where
            add_unit_key ps key
              = add_package dflags pkg_db ps (key, Just p)

missingPackageMsg :: Outputable pkgid => pkgid -> SDoc
missingPackageMsg p = text "unknown package:" <+> ppr p

missingDependencyMsg :: Maybe InstalledUnitId -> SDoc
missingDependencyMsg Nothing = Outputable.empty
missingDependencyMsg (Just parent)
  = space <> parens (text "dependency of" <+> ftext (installedUnitIdFS parent))

-- -----------------------------------------------------------------------------

componentIdString :: DynFlags -> ComponentId -> Maybe String
componentIdString dflags cid =
    fmap sourcePackageIdString (lookupInstalledPackage dflags
        (componentIdToInstalledUnitId cid))

displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
displayInstalledUnitId dflags uid =
    fmap sourcePackageIdString (lookupInstalledPackage dflags uid)

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> UnitId {- not used -} -> Module -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the symbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags this_pkg this_mod name
  | WayDyn `notElem` ways dflags = False
  | Just mod <- nameModule_maybe name
    -- Issue #8696 - when GHC is dynamically linked, it will attempt
    -- to load the dynamic dependencies of object files at compile
    -- time for things like QuasiQuotes or
    -- TemplateHaskell. Unfortunately, this interacts badly with
    -- intra-package linking, because we don't generate indirect
    -- (dynamic) symbols for intra-package calls. This means that if a
    -- module with an intra-package call is loaded without its
    -- dependencies, then GHC fails to link. This is the cause of #
    --
    -- In the mean time, always force dynamic indirections to be
    -- generated: when the module name isn't the module being
    -- compiled, references are dynamic.
    = case platformOS $ targetPlatform dflags of
        -- On Windows the hack for #8696 makes it unlinkable.
        -- As the entire setup of the code from Cmm down to the RTS expects
        -- the use of trampolines for the imported functions only when
        -- doing intra-package linking, e.g. refering to a symbol defined in the same
        -- package should not use a trampoline.
        -- I much rather have dynamic TH not supported than the entire Dynamic linking
        -- not due to a hack.
        -- Also not sure this would break on Windows anyway.
        OSMinGW32 -> moduleUnitId mod /= this_pkg

        -- For the other platforms, still perform the hack
        _         -> mod /= this_mod

  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show (very verbose) package info
pprPackages :: DynFlags -> SDoc
pprPackages = pprPackagesWith pprPackageConfig

pprPackagesWith :: (PackageConfig -> SDoc) -> DynFlags -> SDoc
pprPackagesWith pprIPI dflags =
    vcat (intersperse (text "---") (map pprIPI (listPackageConfigMap dflags)))

-- | Show simplified package info.
--
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
pprPackagesSimple :: DynFlags -> SDoc
pprPackagesSimple = pprPackagesWith pprIPI
    where pprIPI ipi = let i = installedUnitIdFS (unitId ipi)
                           e = if exposed ipi then text "E" else text " "
                           t = if trusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: ModuleToPkgConfAll -> SDoc
pprModuleMap mod_map =
  vcat (map pprLine (Map.toList mod_map))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry :: Outputable a => ModuleName -> (Module, a) -> SDoc
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnitId m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: PackageConfig -> FastString
fsPackageName = mkFastString . packageNameString

-- | Given a fully instantiated 'UnitId', improve it into a
-- 'InstalledUnitId' if we can find it in the package database.
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
improveUnitId _ uid@(DefiniteUnitId _) = uid -- short circuit
improveUnitId pkg_map uid =
    -- Do NOT lookup indefinite ones, they won't be useful!
    case lookupPackage' False pkg_map uid of
        Nothing  -> uid
        Just pkg ->
            -- Do NOT improve if the indefinite unit id is not
            -- part of the closure unique set.  See
            -- Note [UnitId to InstalledUnitId improvement]
            if installedPackageConfigId pkg `elementOfUniqSet` preloadClosure pkg_map
                then packageConfigId pkg
                else uid

-- | Retrieve the 'PackageConfigMap' from 'DynFlags'; used
-- in the @hs-boot@ loop-breaker.
getPackageConfigMap :: DynFlags -> PackageConfigMap
getPackageConfigMap = pkgIdMap . pkgState
