{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, environment info function definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module Session where

import Data

import System.IO ( Handle() )
import Control.Monad.Trans.State ( StateT(..) )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( when )

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

-- | This header file will contain definition for all the C constants used in this file:
--   TABLES_NEXT_TO_CODE, TargetPlatform_NAME, TARGET_ARCH, TARGET_OS and TARGET_VENDOR
#include "HsVersions.h"

tablesNextToCode :: Bool
tablesNextToCode = TABLES_NEXT_TO_CODE == (1 :: Int)

targetPlatformStr :: String
targetPlatformStr = TargetPlatform_NAME

targetPlatform :: (TargetArch, TargetVendor, TargetOS)
targetPlatform = (targetArchitecture, targetVendor, targetOS)

targetArchitecture :: TargetArch
targetArchitecture = case (TARGET_ARCH :: String) of
                       "i386"      -> X86
                       "i486"      -> X86
                       "x86_64"    -> X86_64
                       "amd64"     -> X86_64
                       "powerpc"   -> PowerPC
                       "powerpc64" -> PowerPC64
                       "sparc"     -> Sparc
                       "arm"       -> Arm
                       "aarch64"   -> Arm64
                       "alpha"     -> Alpha
                       "mips"      -> Mips
                       "mipseb"    -> Mips
                       "mipsel"    -> Mipsel
                       "hppa"      -> Hppa
                       _           -> UnknownArch
                        
targetOS :: TargetOS
targetOS = case (TARGET_OS :: String) of
             "linux"         -> Linux
             "ios"           -> IOS
             "darwin"        -> Darwin
             "solaris2"      -> Solaris2
             "mingw32"       -> MingW32
             "freebsd"       -> FreeBSD
             "dragonfly"     -> DragonFly
             "kfreebsdgnu"   -> KFreeBSDGnu
             "openbsd"       -> OpenBSD
             "netbsd"        -> NetBSD
             "haiku"         -> Haiku
             "osf3"          -> OSF3
             "nto-gnx"       -> NtoGNX
             "linux-android" -> Android
             _               -> UnknownOS

             
targetVendor :: TargetVendor
targetVendor = case (TARGET_VENDOR :: String) of
                 "dec"        -> Dec
                 "hp"         -> HP
                 "apple"      -> Apple
                 "next"       -> Next
                 "sun"        -> Sun
                 "sgi"        -> SGI
                 "ibm"        -> IBM
                 "montavista" -> MontaVista
                 "portbld"    -> PortBld
                 _            -> Unknown
                 
data Sections = Sections { fhwnd             :: Handle                 -- ^ Handle to the @_ifile@ that can be used to read data
                         , octr              :: Int                    -- ^ number of sections found
                         , local             :: LocalConstants         -- ^ local constants
                         , dyLdChunks        :: LocalExports           -- ^ dynamic linked chunks
                         , dyLdChunksDefined :: LocalExports           -- ^ dynamic linked chunks definitions
                         , pieces            :: M.Map Int B.ByteString -- ^ Collected sections numbered from 1
                         , buffer            :: B.ByteString           -- ^ temporary buffer in between reads
                         , header            :: B.ByteString           -- ^ collected ASM headers for each section
                         
                         -- File paths
                         , _ifile            :: FilePath               -- ^ Input file name
                         , _prefix           :: FilePath               -- ^ output file prefix to use
                         , _ofile            :: FilePath               -- ^ main output file 
                         }
                           
-- Split monad in which to perform calculation
type SplitM a = StateT Sections IO a

-- | Initialize a new session based on the three input arguments given to the application
--   and a handle to the asm_file.
newSession :: FilePath -> FilePath -> FilePath -> Handle -> Sections
newSession asm_file tmp_prefix out_file hwnd 
    = Sections { fhwnd             = hwnd
               , octr              = 0
               , local             = M.empty
               , dyLdChunksDefined = M.empty
               , dyLdChunks        = M.empty
               , pieces            = M.empty
               , buffer            = ""
               , header            = ""
               
               , _ifile            = asm_file
               , _prefix           = tmp_prefix
               , _ofile            = out_file
               }
      
-- | Indicates whether debug information should be printed      
dump_asm_splitting_info :: Bool
dump_asm_splitting_info = False

optimiseC :: Bool
optimiseC = False

debug :: String -> SplitM ()
debug = liftIO . when dump_asm_splitting_info . putStrLn