{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, Darwin definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module Darwin where

import qualified Data.ByteString.Char8 as B
import Utils
import Session

import Control.Monad ( when )
import qualified Data.Map as M
import Control.Monad.Trans.State ( get, modify )
import Control.Monad.IO.Class ( liftIO )
import Text.Regex.PCRE.ByteString ( Regex() )
import Text.Regex.Base.RegexLike( RegexLike(..) )
import System.IO ( hIsEOF, hSeek, SeekMode(..) )

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
                              
-- The logic for both Darwin/PowerPC and Darwin/x86 ends up being the same.
process_asm_block_darwin :: B.ByteString -> SplitM B.ByteString
process_asm_block_darwin str
  = do -- strip the marker
       let str' = replace str "" $ (mkRegex' "___stg_split_marker.*\n") `matchOnceText` str
       let str'' = replace str' "" $ (mkRegex' "L_.*\\$.*:\n(.|\n)*") `matchOnceText` str'
       
       -- remove/record any literal constants defined here
       res <- while "^(\\s+.const.*\n\\s+\\.align.*\n(LC\\d+):\n(\\s\\.(byte|short|long|fill|space|ascii).*\n)+)" str'' process
              
       newStr <- process_asm_locals "\\b" "(\\b|\\[)" res
       
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