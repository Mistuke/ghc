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
import System.IO ( openFile, IOMode(..), hClose, openFile, hIsEOF, Handle() )
import Data.Monoid ( Monoid(..) )
import Control.Monad ( when, forM_, unless )
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State ( get, modify, modify', evalStateT )

-- | Import local files
import Data
import Session
import Utils

-- | Import architecture files
import Darwin
import PowerPC
import Sparc
import X86_X86_64

import Text.Regex.PCRE.ByteString ( Regex() )
import Text.Regex.Base.RegexLike( RegexLike(..), matchTest, matchAllText )

main :: IO ()   
main = do args     <- getArgs
          progName <- getProgName
          
          when (length args < 3) $ die $ "Syntax: " ++ progName ++ " {input-file} {file-prefix} {output-file}"
          
          let ifile      = args !! 0
              tmp_prefix = args !! 1
              output     = args !! 2
          
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
           
-- | process the items in the pipeline and return the amount of sections found           
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
          
process_asm_block :: B.ByteString -> SplitM B.ByteString
process_asm_block str
   = case targetPlatform of
      (_      , Apple, Darwin) -> process_asm_block_darwin        str
      (Sparc  , _    ,  _    ) -> process_asm_block_sparc         str
      (X86    , _    ,  _    ) -> process_asm_block_iX86          str
      (X86_64 , _    ,  _    ) -> process_asm_block_x86_64        str
      (PowerPC, _    ,  Linux) -> process_asm_block_powerpc_linux str
      _                        -> liftIO $ die $ "no process_asm_block for " ++ targetPlatformStr
             
collectExports :: SplitM ()
collectExports = case targetPlatform of
                   (_   , Apple, Darwin) -> collectDyldStuff_darwin
                   (_   , _    , _     ) -> return ()
                        
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
\end{code}
