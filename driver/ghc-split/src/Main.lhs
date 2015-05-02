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
import System.IO ( openFile, IOMode(..), hClose, openFile, hIsEOF, Handle(..) )
import Data.Monoid ( Monoid(..) )
import Control.Monad ( when, forM_, unless )
import Data.Word ( Word8 )
import qualified Data.Map as M
import qualified Data.Array as A
import Foreign.C.String ( newCString )
import qualified Data.ByteString.Char8  as B
import Control.Monad.Trans.State
import Control.Monad.IO.Class ( liftIO )
import Data.Function ( fix )

import Text.Regex.PCRE.ByteString ( Regex(), compile, execute, compExtended, compCaseless, compMultiline, execBlank)
import Text.Regex.Base.RegexLike( RegexLike(..), makeRegexOpts, matchTest, matchAllText, MatchText )

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
  = Dec        -- ^ dec
  | Unknown    -- ^ unknown
  | HP         -- ^ hp
  | Apple      -- ^ apple
  | Next       -- ^ next
  | Sun        -- ^ sun
  | SGI        -- ^ sgi
  | IBM        -- ^ ibm
  | MontaVista -- ^ montavista
  | PortBld    -- ^ portbld
    deriving (Show, Eq)
    
type LocalConstants = M.Map B.ByteString B.ByteString

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
                 
data Sections = Sections { fhwnd  :: Handle
                         , output :: [B.ByteString]
                         , octr   :: Int
                         , local  :: LocalConstants
                         , pieces :: M.Map Int B.ByteString
                         , buffer :: B.ByteString
                         -- File paths
                         , _ifile  :: FilePath
                         , _prefix :: FilePath
                         , _ofile  :: FilePath
                         }
                           
type SplitM a = StateT Sections IO a

newSession :: FilePath -> FilePath -> FilePath -> Handle -> Sections
newSession asm_file tmp_prefix out_file hwnd 
    = Sections { fhwnd   = hwnd
               , output  = []
               , octr    = 0
               , local   = M.empty
               , pieces  = M.empty
               , buffer  = ""
               
               , _ifile  = asm_file
               , _prefix = tmp_prefix
               , _ofile  = out_file
               }
                 
dump_asm_splitting_info :: Bool
dump_asm_splitting_info = True

debug :: Show a => a -> SplitM ()
debug = liftIO . when dump_asm_splitting_info . print

main :: IO ()   
main = do args     <- getArgs
          progName <- getProgName
          let ifile      = args !! 0
              tmp_prefix = args !! 1
              output     = args !! 2
              
          print tablesNextToCode
          print targetPlatform
          
          split_asm_file ifile tmp_prefix output
              
split_asm_file :: FilePath -> FilePath -> FilePath -> IO ()
split_asm_file asm_file tmp_prefix out_file
   = onException (bracket
                     (openFile asm_file ReadMode)
                     (hClose)
                     (evalStateT pipeline . newSession asm_file tmp_prefix out_file)
                  )
                 (die $ "failed to open " ++ asm_file ++ " (to read)\n")             
           
die :: String -> IO a
die msg = do name <- getProgName
             putStrLn $ name ++ ": " ++ msg
             exitFailure 
             
pipeline :: SplitM ()
pipeline = do exports <- collectExports
              s_stuff <- readTMPIUpToAMarker
              liftIO $ print s_stuff
              -- that first stuff is a prologue for all .s outputs
              prologue_stuff <- process_asm_block s_stuff
              -- $_ already has some of the next stuff in it...
              
              -- lie about where this stuff came from
              -- Note that \Q: ignore regex meta-chars in Tmp_prefix
              input_prefix <- fmap ((`B.append` ".c"      ) . B.pack . _prefix) get
              input_source <- fmap ((`B.append` "_root.hc") . B.pack . _ifile ) get
              let prologue_stuff' = replaceAll input_prefix input_source prologue_stuff
              
              -- process all the remaining assembly blocks
              process_asm_blocks
              
              return ()
  where incr_octr = modify (\s -> s { octr = octr s + 1 })
        process_asm_blocks :: SplitM ()
        process_asm_blocks = do handle <- fmap fhwnd get
                                isEOF  <- liftIO $ hIsEOF handle
                                if isEOF 
                                   then return ()
                                   else do incr_octr
                                           _ <- process_asm_block =<< readTMPIUpToAMarker
                                           return ()
                                           
              
replaceAll :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceAll key val str = replace str
  where val_len = B.length val
  
        replace :: B.ByteString -> B.ByteString
        replace input = case B.breakSubstring key input of
                          ("" ,    _) -> input
                          (pre, body) -> pre `B.append` val `B.append` (replace $ B.drop val_len body)
             
collectExports :: SplitM [B.ByteString]
collectExports = case targetArchitecture of
                        Hppa -> return []
                        Mips -> return []
                        _    -> return []
                        
-- * Regular expressions
mkRegex :: B.ByteString -> Regex
mkRegex bs = makeRegexOpts (compExtended + compCaseless) execBlank bs

mkRegex' :: B.ByteString -> Regex
mkRegex' bs = makeRegexOpts (compExtended + compCaseless + compMultiline) execBlank bs

stg_split_marker :: Regex
stg_split_marker = mkRegex "_?__stg_split_marker"

read_tmpi_up_to_marker :: [Regex]
read_tmpi_up_to_marker = let m_regs = [ "_?__stg_split_marker"
                                      , "^L[^C].*:$"
                                      , "^\\.stab"
                                      , "\t\\.proc"
                                      , "\t\\.stabd"
                                      , "\t\\.even"
                                      , "\tunlk a6"
                                      , "^\t!#PROLOGUE"
                                      , "\t\\.prologue"
                                      , "\t\\.frame"
                                      , "^\\s+(save|retl?|restore|nop)"
                                      ]
                         in map mkRegex m_regs
-- * End
          
readTMPIUpToAMarker :: SplitM B.ByteString
readTMPIUpToAMarker
  = do session <- get
       let input = fhwnd session
           str   = buffer session
           count = octr session
       str <- liftIO $ seek str input
  
       -- if not EOF, then creep forward until next "real" line
       -- (throwing everything away).
       -- that first "real" line will be returned by function.
       --
       -- This loop is intended to pick up the body of the split_marker function
       -- Note that the assembler mangler will already have eliminated this code
       -- if it's been invoked (which it probably has).
       buff <- liftIO $ chomp input
       modify (\s -> s { buffer = buff })
       
       debug $ "### BLOCK:" ++ show count ++ "\n" ++ show str
       
       -- in case Perl doesn't convert line endings
       case targetOS of
          MingW32 -> return $ B.filter (/= '\r') str
          _       -> return str
         
    where seek :: B.ByteString -> Handle -> IO B.ByteString
          seek str tmpi = do line <- B.hGetLine tmpi
                             if line /= "" && not (stg_split_marker `matchTest` line)
                                then seek (str `mappend` line) tmpi
                                else return str
                                
          chomp :: Handle -> IO B.ByteString
          chomp tmpi = do line <- B.hGetLine tmpi -- check EOF instead
                          if line /= "" && (any (`matchTest` line) read_tmpi_up_to_marker)
                             then chomp tmpi
                             else return line

process_asm_block :: B.ByteString -> SplitM B.ByteString
process_asm_block str 
  = case targetPlatform of
      (_      , Apple, Darwin) -> undefined
      (Sparc  , _    ,      _) -> undefined
      (X86    , _    ,      _) -> process_asm_block_iX86 str
      (X86_64 , _    ,      _) -> process_asm_block_x86_64 str
      (PowerPC, _    ,  Linux) -> undefined
      _                        -> liftIO $ die $ "no process_asm_block for " ++ targetPlatformStr
      
process_asm_block_iX86 :: B.ByteString -> SplitM B.ByteString
process_asm_block_iX86 str 
  = do -- strip the marker
       return str
       
    where str_marker_1 :: Regex
          str_marker_1 = mkRegex "s/(\\.text\n\t\\.align .(,0x90)?\n)\\.globl\\s+.*_?__stg_split_marker.*\n/$1/m"
          
          str_marker_2 :: Regex
          str_marker_2 = mkRegex "s/(\t\\.align .(,0x90)?\n)\\.globl\\s+.*_?__stg_split_marker.*\n/$1/m"
          
process_asm_block_x86_64 :: B.ByteString -> SplitM B.ByteString
process_asm_block_x86_64 str 
  = do str' <- while "((?:^|\\.)(LC\\d+):\n(\t\\.(ascii|string).*\n|\\s*\\.byte.*\n){1,100})" str process
       return str'
    where process :: [B.ByteString] -> SplitM B.ByteString
          process matches  
             = do session <- get
                  let label = matches !! 0
                      body  = matches !! 1
                      cache = local session
                      
                  when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                  
                  let cache' = M.insert label body cache
                  
                  liftIO $ print matches
                  put $ session { local = cache' }
                  return ""
                      
flatRegResult :: [MatchText a] -> [a]
flatRegResult = map fst . concatMap A.elems
  
while :: B.ByteString -> B.ByteString -> ([B.ByteString] -> SplitM B.ByteString) -> SplitM B.ByteString
while reg str fn = while' (mkRegex' reg) str fn
    where while' :: Regex -> B.ByteString -> ([B.ByteString] -> SplitM B.ByteString) -> SplitM B.ByteString
          while' reg str fn 
             = do session <- get
                  let mp  = local session
                  let res = flatRegResult $ reg `matchAllText` str
                  liftIO $ print res
                  if null res 
                     then return str
                     else do str' <- fn res
                             while' reg str' fn
  
\end{code}
