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
import System.IO ( openFile, IOMode(..), hClose, openFile, Handle(..) )
import Text.Regex.Posix.ByteString ( Regex(..), compile, execute )
import Text.Regex.Posix ( makeRegex, matchTest )
import Data.Monoid ( Monoid(..) )
import Control.Monad ( when, forM_ )
import Data.Word ( Word8 )

import qualified Data.ByteString.Char8  as B

import Control.Monad.State ( StateT(..), runStateT )

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
    
-- | Target Architectures as defined in aclocal.m4 under checkArch()
data TargetArch
  = X86         -- ^ i386
  | X86_64      -- ^ x64_64
  | PowerPC     -- ^ powerpc
  | PowerPC64   -- ^ powerpc64
  | Sparc       -- ^ sparc
  | Arm         -- ^ arm
  | Arm64       -- ^ aarch64
  | Alpha       -- ^ alpha
  | Mips        -- ^ mips|mipseb
  | Mipsel      -- ^ mipsel
  | Hppa        -- ^ hppa
  | UnknownArch -- ^ The rest
    deriving (Show, Eq)
    
-- | Target vendor as defined in aclocal.m4 under checkVendor()
data TargetVendor
  = Dec   -- ^ dec
  | Unknown -- ^ unknown
  | HP      -- ^ hp
  | Apple   -- ^ apple
  | Next    -- ^ next
  | Sun     -- ^ sun
  | SGI     -- ^ sgi
  | IBM     -- ^ ibm
  | MontaVista -- ^ montavista
  | PortBld    -- ^ portbld
    deriving (Show, Eq)

tablesNextToCode :: Bool
tablesNextToCode = TABLES_NEXT_TO_CODE == 1

targetPlatformStr :: String
targetPlatformStr = TargetPlatform_NAME

targetPlatform :: (TargetArch, TargetVendor, TargetOS)
targetPlatform = (targetArchitecture, targetVendor, targetOS)

targetArchitecture :: TargetArch
targetArchitecture = case TARGET_ARCH of
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

             
targetVendor :: TargetVendor
targetVendor = case TARGET_VENDOR of
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
                 
data Sections = Sections { input  :: Handle
                         , output :: [B.ByteString]
                         }
                 
dump_asm_splitting_info :: Bool
dump_asm_splitting_info = True

debug :: Show a => a -> IO ()
debug = when dump_asm_splitting_info . print

main :: IO ()   
main = do args     <- getArgs
          progName <- getProgName
          let ifile      = args !! 0
              tmp_prefix = args !! 1
              output     = args !! 2
              
          print tablesNextToCode
          print targetPlatform
          
          split_asm_file ifile
              
split_asm_file :: FilePath -> IO ()
split_asm_file asm_file
   = onException (bracket
                     (openFile asm_file ReadMode)
                     (hClose)
                     (pipeline)
                  )
                 (die $ "failed to open " ++ asm_file ++ " (to write)\n")             
           
die :: String -> IO a
die msg = do name <- getProgName
             putStrLn $ name ++ ": " ++ msg
             exitFailure 
             
pipeline :: Handle -> IO ()
pipeline hwnd = do exports <- collectExports hwnd
                   s_stuff <- readTMPIUpToAMarker "" 0 hwnd
                   -- that first stuff is a prologue for all .s outputs
                   prologue_stuff <- process_asm_block s_stuff
                   -- $_ already has some of the next stuff in it...
                   return ()
             
collectExports :: Handle -> IO [B.ByteString]
collectExports = case targetArchitecture of
                        Hppa -> const $ return []
                        Mips -> const $ return []
                        _    -> const $ return []
                        
-- * Regular expressions
stg_split_marker :: Regex
stg_split_marker = makeRegex ("_?__stg_split_marker" :: B.ByteString)

read_tmpi_up_to_marker :: [Regex]
read_tmpi_up_to_marker = let m_regs = [ "_?__stg_split_marker"
                                      , "^L[^C].*:$"
                                      , "^\\.stab"
                                      , "\\t\\.proc"
                                      , "\\t\\.stabd"
                                      , "\\t\\.even"
                                      , "\\tunlk a6"
                                      , "^\\t!#PROLOGUE"
                                      , "\t\\.prologue"
                                      , "\\t\\.frame"
                                      , "^\\s+(save|retl?|restore|nop)"
                                      ] :: [B.ByteString]
                         in map makeRegex m_regs
-- * End
          
readTMPIUpToAMarker :: B.ByteString -> Int -> Handle -> IO B.ByteString
readTMPIUpToAMarker str count input 
  = do start <- seek str input
  
       -- if not EOF, then creep forward until next "real" line
       -- (throwing everything away).
       -- that first "real" line will be returned by function.
       --
       -- This loop is intended to pick up the body of the split_marker function
       -- Note that the assembler mangler will already have eliminated this code
       -- if it's been invoked (which it probably has).
       buff <- chomp input
       debug $ "### BLOCK:" ++ show count ++ "\n" ++ show buff
       
       -- in case Perl doesn't convert line endings
       case targetOS of
          MingW32 -> return $ B.filter (not . (/= '\r')) buff
          _       -> return buff
         
    where seek :: B.ByteString -> Handle -> IO B.ByteString
          seek str tmpi = do line <- B.hGetLine tmpi
                             if line /= "" && not (stg_split_marker `matchTest` line)
                                then seek (str `mappend` line) tmpi
                                else return str
                                
          chomp :: Handle -> IO B.ByteString
          chomp tmpi = do line <- B.hGetLine tmpi
                          if line /= "" && undefined
                             then chomp tmpi
                             else return line

process_asm_block :: B.ByteString -> IO B.ByteString
process_asm_block str 
  = case targetPlatform of
      (_      , Apple, Darwin) -> undefined
      (Sparc  , _    ,      _) -> undefined
      (X86    , _    ,      _) -> process_asm_block_iX86 str
      (X86_64 , _    ,      _) -> undefined
      (PowerPC, _    ,  Linux) -> undefined
      _                        -> die $ "no process_asm_block for " ++ targetPlatformStr
      
process_asm_block_iX86 :: B.ByteString -> IO B.ByteString
process_asm_block_iX86 str 
  = do -- strip the marker
       return str
       
    where str_marker_1 :: Regex
          str_marker_1 = makeRegex ("s/(\\.text\\n\\t\\.align .(,0x90)?\\n)\\.globl\\s+.*_?__stg_split_marker.*\\n/$1/m" :: B.ByteString)
          
          str_marker_2 :: Regex
          str_marker_2 = makeRegex ("s/(\\t\\.align .(,0x90)?\\n)\\.globl\\s+.*_?__stg_split_marker.*\\n/$1/m" :: B.ByteString)
\end{code}