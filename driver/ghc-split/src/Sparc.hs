{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, Sparc definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module Sparc where

import qualified Data.ByteString.Char8 as B
import Utils
import Session

import Control.Monad ( when )
import qualified Data.Map as M
import Control.Monad.Trans.State ( get, modify )
import Control.Monad.IO.Class ( liftIO )
import Text.Regex.Base.RegexLike( RegexLike(..) )

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
            
       debug "---"
       debug $ show ren
       debug "---"
            
       -- remove/record any literal constants defined here
       val <- while "(\t\\.align .\n\\.?(L?LC\\d+):\n(\t\\.asci[iz].*\n)+)" ren process
        
       newStr <- process_asm_locals "\\b" "\\b" val
        
       debug $ "### STRIPPED BLOCK (sparc):\n" ++ show newStr
       return newStr
    
    where process :: (B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString
          process (_prefix, matches , _postfix) strInput
            = do session <- get
                 let label = matches !! 1
                     body  = matches !! 0
                     cache = local session
                     
                 when (label `M.member` cache) $ liftIO $ die $ "Local constant label " ++ show label ++ " already defined!\n"
                                  
                 modify (\s -> s { local = M.insert label body cache })
                 
                 return $ replace strInput "" $ (mkRegex' "\t\\.align .\n\\.?LL?C\\d+:\n(\t\\.asci[iz].*\n)+") `matchOnceText` strInput
                  