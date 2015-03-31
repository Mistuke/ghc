%************************************************************************
%*                                                                      *
\section[Driver-obj-splitting]{Splitting into many \tr{.o} files (for libraries)}
%*                                                                      *
%************************************************************************

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 

module Main where

#include "HsVersions.h"

import Control.Exception ( onException, bracket )

import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( openFile, IOMode(..), hClose )

import qualified Data.ByteString as BS

-- | Target OSes as defined in aclocal.m4 under checkOS()
data TargetOS
  = Linux       -- ^ linux
  | IOS         -- ^ ios
  | Darwin      -- ^ darwin
  | Solaris2    -- ^ solaris2
  | MingW32     -- ^ mingw32
  | FreeBSD     -- ^ freebsd
  | DragonFly   -- ^ dragonfly
  | KFreeBSDGnu -- ^ kfreebsdgnu
  | OpenBSD     -- ^ openbsd
  | NetBSD      -- ^ netbsd
  | Haiku       -- ^ haiku
  | OSF3        -- ^ osf3
  | NtoGNX      -- ^ nto-gnx
  | Android     -- ^ linux-android
  | UnknownOS   -- ^ The rest
    deriving (Show, Eq)
    
-- | Target Architectures as defined in aclocal.m4 under checkZrch()
data TargetArch
  = I386        -- ^ i386
  | X64_64      -- ^ x64_64
  | PowerPC     -- ^ powerpc
  | PowerPC64   -- ^ powerpc64
  | Sparc       -- ^ sparc
  | Arm         -- ^ arm
  | Arm64       -- ^ aarch64
  | Alpha       -- ^ alpha
  | Mips        -- ^ mips|mipseb
  | Mipsel      -- ^ mipsel
  | UnknownArch -- ^ The rest
    deriving (Show, Eq)

tablesNextToCode :: Bool
tablesNextToCode = TABLES_NEXT_TO_CODE == 1

targetOS :: TargetOS
targetOS = case TARGET_OS of
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
             

targetPlatform :: String
targetPlatform = TargetPlatform_NAME

targetArchitecture :: String
targetArchitecture = TARGET_ARCH

main :: IO ()   
main = do args     <- getArgs
          progName <- getProgName
          let ifile      = args !! 0
              tmp_prefix = args !! 1
              output     = args !! 2
              
          print tablesNextToCode
          print targetOS
          print targetPlatform
          print targetArchitecture
          split_asm_file ifile
              
split_asm_file :: FilePath -> IO ()
split_asm_file asm_file
   = onException (do content <- BS.readFile asm_file
                     
                     return ()
                  )
                 (die $ "failed to open " ++ asm_file ++ " (to write)\n")             
           
die :: String -> IO ()
die msg = do name <- getProgName
             putStrLn $ name ++ ": " ++ msg
             exitFailure 
\end{code}