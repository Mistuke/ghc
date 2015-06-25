{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, x86 and x86_64 definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module X86_X86_64 where

import qualified Data.ByteString.Char8 as B
import Utils
import Session

import Control.Monad ( when )
import qualified Data.Map as M
import Control.Monad.Trans.State ( get, put )
import Control.Monad.IO.Class ( liftIO )
import Text.Regex.PCRE.ByteString ( Regex() )
import Text.Regex.Base.RegexLike( RegexLike(..) )

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
process_asm_block_x86_XX string
  = do str' <- while "((?:^|\\.)(LC\\d+):\n(\t\\.(ascii|string).*\n|\\s*\\.byte.*\n){1,100})" string process
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
                                        fn = \val -> case flatRegResult val of
                                                        (_prefix, matches, suffix) -> (body `B.append` (matches !! 0), suffix)
                                    in maybe (body, str) fn res