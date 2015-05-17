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
import System.IO ( openFile, IOMode(..), hClose, openFile, hIsEOF, hSeek, SeekMode(..), Handle() )
import Data.Monoid ( Monoid(..) )
import Data.Function ( fix )
import Data.Word ( Word8 )
import Data.Maybe ( isNothing, fromJust, maybe )
import Data.Foldable ( foldrM )
import Control.Monad ( when, forM_, unless )
import qualified Data.Map as M
import qualified Data.Array as A
import Foreign.C.String ( newCString )
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.State
import Control.Monad.IO.Class ( liftIO )

import Text.Regex.PCRE.ByteString ( Regex(), compile, execute, compExtended, compCaseless, compMultiline, execBlank )
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
type LocalExports   = M.Map B.ByteString B.ByteString

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
                 
data Sections = Sections { fhwnd             :: Handle
                         , output            :: [B.ByteString]
                         , octr              :: Int
                         , local             :: LocalConstants
                         , dyLdChunks        :: LocalExports
                         , dyLdChunksDefined :: LocalExports
                         , pieces            :: M.Map Int B.ByteString
                         , buffer            :: B.ByteString
                         , header            :: B.ByteString
                         -- File paths
                         , _ifile            :: FilePath
                         , _prefix           :: FilePath
                         , _ofile            :: FilePath
                         }
                           
type SplitM a = StateT Sections IO a

newSession :: FilePath -> FilePath -> FilePath -> Handle -> Sections
newSession asm_file tmp_prefix out_file hwnd 
    = Sections { fhwnd             = hwnd
               , output            = []
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
                 
dump_asm_splitting_info :: Bool
dump_asm_splitting_info = True

optimiseC :: Bool
optimiseC = False

debug :: String -> SplitM ()
debug = liftIO . when dump_asm_splitting_info . putStrLn

main :: IO ()   
main = do args     <- getArgs
          progName <- getProgName
          
          when (length args < 3) $ die $ "Syntax: " ++ progName ++ " {input-file} {file-prefix} {output-file}"
          
          let ifile      = args !! 0
              tmp_prefix = args !! 1
              output     = args !! 2
              
          print targetPlatform
          
          noOfSplitFiles <- split_asm_file ifile tmp_prefix output
          onException (bracket
                          (openFile output WriteMode)
                          (hClose)
                          (flip B.hPutStrLn (B.pack $ show noOfSplitFiles))
                      )
                      (die $ "failed to open `" ++ output ++ "' (to write)\n")  
              
split_asm_file :: FilePath -> FilePath -> FilePath -> IO Int
split_asm_file asm_file tmp_prefix out_file
   = onException (bracket
                     (openFile asm_file ReadMode)
                     (hClose)
                     (evalStateT pipeline . newSession asm_file tmp_prefix out_file)
                  )
                 (die $ "failed to open `" ++ asm_file ++ "' (to read)\n")             
           
die :: String -> IO a
die msg = do name <- getProgName
             putStrLn $ name ++ ": " ++ msg
             exitFailure 
             
pipeline :: SplitM Int
pipeline = do collectExports
              s_stuff <- readTMPIUpToAMarker
              
              -- that first stuff is a prologue for all .s outputs
              prologue_stuff <- process_asm_block s_stuff
              -- $_ already has some of the next stuff in it...
              
              -- lie about where this stuff came from
              -- Note that \Q: ignore regex meta-chars in Tmp_prefix
              input_prefix <- fmap ((`B.append` ".c"      ) . B.pack . _prefix) get
              input_source <- fmap ((`B.append` "_root.hc") . B.pack . _ifile ) get
              let prologue_stuff' = replaceAll input_prefix input_source prologue_stuff
              modify (\s -> s { header = prologue_stuff' })
              
              -- process all the remaining assembly blocks
              process_asm_blocks
              
              -- make sure that we still have some output when the input file is empty
              _octr <- fmap octr get
              when (_octr == 0) $ modify' (\s -> s { octr = 1, pieces = M.insert 1 "" (pieces s)})
              
              noOfSplitFiles <- fmap octr get
              
              -- Add the GNU note the end of every piece if found
              lastPiece <- fmap ((M.! noOfSplitFiles) . pieces) get
              let matchGnu   = mkRegex' "(\n[ \t]*\\.section[ \t]+\\.note\\.GNU-stack,[^\n]*\n)"
              let matchesGnu = flatAllResult $ matchGnu `matchAllText` lastPiece
              when (not $ null matchesGnu) $ addGnuNote noOfSplitFiles (matchesGnu !! 0)
              
              -- Write out the results
              forM_ [1..noOfSplitFiles] writeSplitResultFile
              
              -- return the number of files written
              return noOfSplitFiles
              
  where incr_octr :: SplitM ()
        incr_octr = modify' (\s -> s { octr = octr s + 1 })
  
        process_asm_blocks :: SplitM ()
        process_asm_blocks = do handle <- fmap fhwnd get
                                isEOF  <- liftIO $ hIsEOF handle
                                unless isEOF $ do incr_octr
                                                  asm <- process_asm_block =<< readTMPIUpToAMarker
                                                  modify' (\s -> s { pieces = M.insert (octr s) asm (pieces s) })
                                                  process_asm_blocks
                                           
        addGnuNote :: Int -> B.ByteString -> SplitM ()
        addGnuNote noFiles note = modify (\s -> s { pieces = foldr ($) (pieces s) $ map (M.adjust (`B.append` note)) [1..(noFiles - 1)] })
                             
        writeSplitResultFile :: Int -> SplitM ()
        writeSplitResultFile idx 
           = do -- output to a file of its own
                -- open a new output file...
                session <- get
                let ofname = (_prefix session) ++ "__" ++ show idx ++ ".s"
                debug $ "writing " ++ ofname
                liftIO $ onException 
                            (bracket
                                (openFile ofname WriteMode)
                                (hClose)
                                (\handle -> do B.hPut handle (header session)
                                               B.hPut handle ((pieces session) M.! idx)
                                )
                            ) 
                            (die $ "Failed writing `" ++ ofname ++ "'\n") 
              
replaceAll :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceAll key val str = replace str
  where val_len = B.length val
  
        replace :: B.ByteString -> B.ByteString
        replace input = case B.breakSubstring key input of
                          ("" ,    _) -> input
                          (pre, body) -> pre `B.append` val `B.append` (replace $ B.drop val_len body)
             
collectExports :: SplitM ()
collectExports = case targetPlatform of
                   (_   , Apple, Darwin) -> collectDyldStuff_darwin
                   (_   , _    , _     ) -> return ()
    
data DyLdStuff = DarwinDyLd 
               { drwn_cur_section   :: B.ByteString
               , drwn_section       :: B.ByteString
               , drwn_label         :: B.ByteString
               , drwn_chunk         :: B.ByteString
               , drwn_chunk_label   :: B.ByteString
               , drwn_alignment     :: B.ByteString
               , drwn_cur_alignment :: B.ByteString
               }
               
collectDyldStuff_darwin :: SplitM ()
collectDyldStuff_darwin 
  = do -- make sure the global tables are empty
       modify (\s -> s { dyLdChunksDefined = M.empty, dyLdChunks = M.empty })
       
       hwnd <- fmap fhwnd get
       isEOF <- liftIO $ hIsEOF hwnd
       let ld = DarwinDyLd { drwn_cur_section   = ""
                           , drwn_section       = ""
                           , drwn_label         = ""
                           , drwn_chunk         = ""
                           , drwn_chunk_label   = ""
                           , drwn_alignment     = ""
                           , drwn_cur_alignment = ""
                           }
                           
       _ <- collect isEOF ld
       
       liftIO $ hSeek hwnd AbsoluteSeek 0
       
    where collect :: Bool -> DyLdStuff -> SplitM DyLdStuff
          collect True  ld = return ld
          collect False ld = do handle <- fmap fhwnd get
                                line  <- liftIO $ hGetLine handle
                                
                                ld' <- if case1 `matchTest` line && case2 `matchTest` line
                                          then match1and2 line ld
                                          else if case3 `matchTest` line
                                                  then match3 line ld
                                                  else if case4 `matchTest` line
                                                          then match4 line ld
                                                          else if case5 `matchTest` line
                                                                  then match5 line ld
                                                                  else return $ ld { drwn_chunk = (drwn_chunk ld) `B.append` line }
                                   
                                isEOF <- liftIO $ hIsEOF   handle
                                collect isEOF ld'
                             
          case1 :: Regex
          case1 = mkRegex' "^L(_.+)\\$.+:"

          case2 :: Regex
          case2 = mkRegex' "^L(.*)\\$stub_binder:"
          
          match1and2 :: B.ByteString -> DyLdStuff -> SplitM DyLdStuff
          match1and2 line dy = do modify (\s -> s { dyLdChunksDefined = M.adjust (`B.append` (drwn_section dy `B.append` drwn_alignment dy `B.append` drwn_chunk_label dy `B.append` drwn_chunk dy)) (drwn_label dy) (dyLdChunksDefined s) })
                                  let section = replaceM line ".non_lazy_symbol_pointer" $ (mkRegex' "\\.data") `matchOnceText` line
                                  let dy' = maybe dy (\s -> dy { drwn_chunk = "\t.indirect_symbol $label\n\t.long 0\n", drwn_section = s }) section
                                  modify (\s -> s { dyLdChunks = M.adjust (`B.append` (drwn_section dy' `B.append` drwn_alignment dy' `B.append` drwn_chunk_label dy' `B.append` drwn_chunk dy')) (drwn_label dy') (dyLdChunks s) })
                                  debug $ "### dyld chunk: " ++ (show $ B.concat [drwn_label dy', "\n", drwn_section dy', drwn_alignment dy', drwn_chunk dy'])
                                
                                  let (_, match, _) = maybe ("", line, "") extract $ case2 `matchOnceText` line
                                  let newDy = dy' { drwn_chunk       = ""
                                                  , drwn_chunk_label = line
                                                  , drwn_label       = match
                                                  , drwn_section     = drwn_cur_section dy'
                                                  , drwn_alignment   = drwn_cur_alignment dy'
                                                  }
                                  debug $ "### label:" ++ show (drwn_label newDy)
                                  return newDy
          case3 :: Regex
          case3 = mkRegex' "^\\s*\\.(symbol_stub|picsymbol_stub|lazy_symbol_pointer|non_lazy_symbol_pointer|data|section __IMPORT,.*|section __DATA, __la_sym_ptr(2|3),lazy_symbol_pointers)"
          
          match3 :: B.ByteString -> DyLdStuff -> SplitM DyLdStuff
          match3 line dy = do let dy' = dy { drwn_cur_section = line, drwn_cur_alignment = "" }
                              debug $ "section: " ++ show (drwn_cur_section dy')
                              return dy'
          
          case4 :: Regex
          case4 = mkRegex' "^\\s*\\.section\\s+__TEXT,__symbol_stub1,symbol_stubs,pure_instructions,\\d+"
                    
          match4 :: B.ByteString -> DyLdStuff -> SplitM DyLdStuff
          match4 line dy = do -- always make sure we align things
                              let dy' = dy { drwn_cur_section = line, drwn_cur_alignment = "\t.align 2" }
                              debug $ "section: " ++ show (drwn_cur_section dy')
                              return dy'
                              
          case5 :: Regex
          case5 = mkRegex' "^\\s*\\.align.*"
          
          match5 :: B.ByteString -> DyLdStuff -> SplitM DyLdStuff
          match5 line dy = do let dy' = dy { drwn_cur_alignment = line }
                              debug $ "alignment: " ++ show (drwn_cur_alignment dy')
                              return dy'
                        
-- * Regular expressions
mkRegex :: B.ByteString -> Regex
mkRegex bs = makeRegexOpts (compExtended + compCaseless) execBlank bs

mkRegex' :: B.ByteString -> Regex
mkRegex' bs = makeRegexOpts (compExtended + compCaseless + compMultiline) execBlank bs

stg_split_marker :: Regex
stg_split_marker = mkRegex' "_?__stg_split_marker"

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
                         in map mkRegex' m_regs
-- * End
          
readTMPIUpToAMarker :: SplitM B.ByteString
readTMPIUpToAMarker
  = do session <- get
       let input = fhwnd session
           str   = buffer session
           count = octr session
       
       str' <- liftIO $ seek str input
  
       -- if not EOF, then creep forward until next "real" line
       -- (throwing everything away).
       -- that first "real" line will be returned by function.
       --
       -- This loop is intended to pick up the body of the split_marker function
       -- Note that the assembler mangler will already have eliminated this code
       -- if it's been invoked (which it probably has).
       buff <- liftIO $ chomp input
       modify (\s -> s { buffer = buff })
       
       debug $ "### BLOCK:" ++ show count ++ "\n" ++ show str'
       
       return str'
         
    where seek :: B.ByteString -> Handle -> IO B.ByteString
          seek str tmpi = do eof <- hIsEOF tmpi
                             if eof
                                then return str
                                else do line <- hGetLine tmpi
                                        if not $ stg_split_marker `matchTest` line
                                           then seek (str `mappend` line) tmpi
                                           else return str
                                
          chomp :: Handle -> IO B.ByteString
          chomp tmpi = do eof <- hIsEOF tmpi 
                          if eof
                             then return ""
                             else do line <- hGetLine tmpi
                                     if any (`matchTest` line) read_tmpi_up_to_marker
                                        then chomp tmpi
                                        else return line

extract :: (B.ByteString, MatchText B.ByteString, B.ByteString) -> (B.ByteString, B.ByteString, B.ByteString)
extract (l, n, r) = (l, fst $ n A.! 0, r)
          
combine str m = let (l, v, r) = maybe ("", str, "") extract m
                in B.concat [l,v,r]

replace str v m = let (l, _, r) = maybe ("", str, "") extract m
                  in B.concat [l,v,r]
                  
replaceM str v m = fmap ((\(l, _, r) -> B.concat [l,v,r]) . extract) m
          
process_asm_block :: B.ByteString -> SplitM B.ByteString
process_asm_block str 
  = case targetPlatform of
      (_      , Apple, Darwin) -> process_asm_block_darwin        str
      (Sparc  , _    ,  _    ) -> process_asm_block_sparc         str
      (X86    , _    ,  _    ) -> process_asm_block_iX86          str
      (X86_64 , _    ,  _    ) -> process_asm_block_x86_64        str
      (PowerPC, _    ,  Linux) -> process_asm_block_powerpc_linux str
      _                        -> liftIO $ die $ "no process_asm_block for " ++ targetPlatformStr
      
-- The logic for both Darwin/PowerPC and Darwin/x86 ends up being the same.
process_asm_block_darwin :: B.ByteString -> SplitM B.ByteString
process_asm_block_darwin str
  = do -- strip the marker
       let str' = replace str "" $ (mkRegex' "___stg_split_marker.*\n") `matchOnceText` str
       let str'' = replace str' "" $ (mkRegex' "L_.*\\$.*:\n(.|\n)*") `matchOnceText` str'
       
       -- remove/record any literal constants defined here
       val <- while "^(\\s+.section\\s+\\.rodata\n\\s+\\.align.*\n(\\.LC\\d+):\n(\\s\\.(byte|short|long|quad|2byte|4byte|8byte|fill|space|ascii|string).*\n)+)" str' process
              
       newStr <- process_asm_locals "\\b" "(\\b|\\[)" val
       
       chunks  <- fmap dyLdChunks get
       defined <- fmap dyLdChunksDefined get
       let keys     = M.keys chunks
       let adjLocal = \k val -> if (mkRegex' $ B.concat ["\\bL", k, "\\$"]) `matchTest` val 
                                   then if (mkRegex' $ B.concat ["^", k, ":$"]) `matchTest` val
                                           then (chunks  M.! k) `B.append` val
                                           else (defined M.! k) `B.append` val
                                    else val
                                    
       let dyld_stuff = foldr ($) newStr $ map adjLocal keys
       
       let newStr' = "\n" `B.append` dyld_stuff
       
       debug $ "### STRIPPED BLOCK (darwin):\n" ++ show newStr'
       return newStr'
       
    where process :: (B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString
          process (prefix, matches , postfix) _
            = do session <- get
                 let label = matches !! 1
                     body  = matches !! 0
                     cache = local session
                     
                 when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                                  
                 modify (\s -> s { local = M.insert label body cache })
                 
                 return (prefix `B.append` postfix)

process_asm_block_powerpc_linux :: B.ByteString -> SplitM B.ByteString
process_asm_block_powerpc_linux str
  = do -- strip the marker
       let str' = replace str "" $ (mkRegex' "__stg_split_marker.*\n") `matchOnceText` str
       
       -- remove/record any literal constants defined here
       val <- while "^(\\s+.section\\s+\\.rodata\n\\s+\\.align.*\n(\\.LC\\d+):\n(\\s\\.(byte|short|long|quad|2byte|4byte|8byte|fill|space|ascii|string).*\n)+)" str' process
        
       newStr <- process_asm_locals "[\\s,]" "\\b" val
        
       debug $ "### STRIPPED BLOCK (powerpc linux):\n" ++ show newStr
       return newStr
    
    where process :: (B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString
          process (prefix, matches , postfix) _
            = do session <- get
                 let label = matches !! 1
                     body  = matches !! 0
                     cache = local session
                     
                 when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                                  
                 modify (\s -> s { local = M.insert label body cache })
                 
                 return (prefix `B.append` postfix)
                        
process_asm_block_sparc :: B.ByteString -> SplitM B.ByteString
process_asm_block_sparc str
  = do -- strip the marker
       let str' = if optimiseC
                     then replace str "" $ (mkRegex' "_?__stg_split_marker.*:\n") `matchOnceText` str
                     else let res = combine str $ (mkRegex' "(\\.text\n\t\\.align .\n)\t\\.global\\s+.*_?__stg_split_marker.*\n\t\\.proc.*\n") `matchOnceText` str
                          in combine res $ (mkRegex' "(\t\\.align .\n)\t\\.global\\s+.*_?__stg_split_marker.*\n\t\\.proc.*\n") `matchOnceText` res
        
       session <- get
       -- make sure the .hc filename gets saved; not just ghc*.c (temp name)
       let ifile = B.pack $ ".stabs \"" ++ _ifile session ++ "_root.hc\""
           ren   = replace str' ifile $ (mkRegex' "^\\.stabs \"(ghc\\d+\\.c)\"") `matchOnceText` str' -- HACK HACK
            
       -- remove/record any literal constants defined here
       val <- while "(\t\\.align .\n\\.?(L?LC\\d+):\n(\t\\.asci[iz].*\n)+)" ren process
        
       newStr <- process_asm_locals "\\b" "\\b" val
        
       debug $ "### STRIPPED BLOCK (sparc):\n" ++ show newStr
       return newStr
    
    where process :: (B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString
          process (prefix, matches , postfix) strInput
            = do session <- get
                 let label = matches !! 1
                     body  = matches !! 0
                     cache = local session
                     
                 when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                                  
                 modify (\s -> s { local = M.insert label body cache })
                 
                 return $ replace strInput "" $ (mkRegex' "\t\\.align .\n\\.?LL?C\\d+:\n(\t\\.asci[iz].*\n)+") `matchOnceText` strInput
                  
      
process_asm_block_iX86 :: B.ByteString -> SplitM B.ByteString
process_asm_block_iX86 str 
  = do -- strip the marker
       let (l , res , r ) = maybe ("", str, "") extract $ str_marker_1 `matchOnceText` str
       let (l', res', r') = maybe ("", res, "") extract $ str_marker_2 `matchOnceText` (B.concat [l, res, r])
       
       -- it seems prudent to stick on one of these:
       let str' = ".text\n\t.align 4\n" `B.append` (B.concat [l', res', r'])
       newStr <- process_asm_block_x86_XX str'
       
       debug $ "### STRIPPED BLOCK (x86_64):\n" ++ show newStr
       
       return newStr
       
    where str_marker_1 :: Regex
          str_marker_1 = mkRegex "(\\.text\n\t\\.align .(,0x90)?\n)\\.globl\\s+.*_?__stg_split_marker.*\n"
          
          str_marker_2 :: Regex
          str_marker_2 = mkRegex "(\t\\.align .(,0x90)?\n)\\.globl\\s+.*_?__stg_split_marker.*\n"
          
process_asm_block_x86_64 :: B.ByteString -> SplitM B.ByteString
process_asm_block_x86_64 str 
  = do newStr <- process_asm_block_x86_XX str
       
       debug $ "### STRIPPED BLOCK (x86_64):\n" ++ show newStr
       
       return newStr
                                    
process_asm_block_x86_XX :: B.ByteString -> SplitM B.ByteString
process_asm_block_x86_XX str
  = do str' <- while "((?:^|\\.)(LC\\d+):\n(\t\\.(ascii|string).*\n|\\s*\\.byte.*\n){1,100})" str process
       process_asm_locals "\\b" "\\b" str'
       
    where process :: (B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString
          process (prefix, matches , postfix) _
             = do session <- get
                  let label = matches !! 1
                      body  = matches !! 0
                      cache = local session
                      
                  when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                  
                  let (body', suffix') = expandBody (mkRegex "^((\t\\.(ascii|string).*\n|\\s*\\.byte.*\n){1,100})") body postfix 
                                   
                  let cache' = M.insert label body' cache
                                    
                  put $ session { local = cache' }
                  return (prefix `B.append` suffix')
                  
          expandBody :: Regex -> B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
          expandBody reg body str = let res = reg `matchOnceText` str
                                        fn = \res -> case flatRegResult res of
                                                        (prefix, matches, suffix) -> (body `B.append` (matches !! 0), suffix)
                                    in maybe (body, str) fn res

process_asm_locals :: B.ByteString -> B.ByteString -> B.ByteString -> SplitM B.ByteString
process_asm_locals l r str 
  = do -- inject definitions for any local constants now used herein
       locals <- fmap local get 
       let keys     = M.keys locals
       let adjLocal = \k val -> if (mkRegex' $ B.concat [l, k, r]) `matchTest` val
                                   then (locals M.! k) `B.append` val
                                   else val
       return $ foldr ($) str $ map adjLocal keys
                                    
flatRegResult :: (a, MatchText a, a) -> (a, [a], a)
flatRegResult (l, m, r) = (l, map fst $ A.elems m, r)

flatAllResult :: [MatchText a] -> [a]
flatAllResult = map fst . concatMap A.elems
  
while :: B.ByteString -> B.ByteString -> ((B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString) -> SplitM B.ByteString
while reg str fn = while' (mkRegex' reg) str fn
    where while' :: Regex -> B.ByteString -> ((B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString) -> SplitM B.ByteString
          while' reg str fn 
             = do session <- get
                  let res = reg `matchOnceText` str
                  if isNothing res 
                     then return str
                     else do str' <- fn ((flatRegResult . fromJust) res) str
                             while' reg str' fn
                            
hGetLine :: Handle -> IO B.ByteString
hGetLine = fmap (`B.append` "\n") . B.hGetLine
\end{code}
