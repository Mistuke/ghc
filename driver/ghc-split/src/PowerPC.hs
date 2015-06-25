{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, PowerPC definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module PowerPC where

import qualified Data.ByteString.Char8 as B
import Utils
import Session

import Control.Monad ( when )
import qualified Data.Map as M
import Control.Monad.Trans.State ( get, modify )
import Control.Monad.IO.Class ( liftIO )
import Text.Regex.Base.RegexLike( RegexLike(..) )

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