{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, utility function definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module Utils where

import Session

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Array as A
import Text.Regex.PCRE.ByteString ( Regex(), compExtended, compCaseless, compMultiline, execBlank )
import Text.Regex.Base.RegexLike( RegexLike(..), makeRegexOpts, matchTest, MatchText )

import Control.Monad.Trans.State ( get )
import System.IO ( Handle() )
import Data.Maybe ( isNothing, fromJust )
import System.Exit ( exitFailure )
import System.Environment ( getProgName )

replaceAll :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceAll key val str = replace' str
  where val_len = B.length val
  
        replace' :: B.ByteString -> B.ByteString
        replace' input = case B.breakSubstring key input of
                           ("" ,    _) -> input
                           (pre, body) -> pre `B.append` val `B.append` (replace' $ B.drop val_len body)
                          
-- * Regular expressions
mkRegex :: B.ByteString -> Regex
mkRegex bs = makeRegexOpts (compExtended + compCaseless) execBlank bs

mkRegex' :: B.ByteString -> Regex
mkRegex' bs = makeRegexOpts (compExtended + compCaseless + compMultiline) execBlank bs

extract :: (B.ByteString, MatchText B.ByteString, B.ByteString) -> (B.ByteString, B.ByteString, B.ByteString)
extract (l, n, r) = (l, fst $ n A.! 0, r)
    
combine :: B.ByteString
        -> Maybe (B.ByteString, MatchText B.ByteString, B.ByteString)
        -> B.ByteString    
combine str m = let (l, v, r) = maybe ("", str, "") extract m
                in B.concat [l,v,r]

replace :: B.ByteString
        -> B.ByteString
        -> Maybe (B.ByteString, MatchText B.ByteString, B.ByteString)
        -> B.ByteString
replace str v m = let (l, a, r) = maybe ("", str, "") (sub . extract) m
                  in B.concat  [l,a,r]
    where sub (a, _, b) = (a, v, b)    

replaceM :: B.ByteString
         -> B.ByteString
         -> Maybe (B.ByteString, MatchText B.ByteString, B.ByteString)
         -> Maybe B.ByteString
replaceM str v m = let (l, a, r) = maybe ("", str, "") (sub . extract) m
                  in return $ B.concat  [l,a,r]
    where sub (a, _, b) = (a, v, b)

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
while reg string func = while' (mkRegex' reg) string func
    where while' :: Regex -> B.ByteString -> ((B.ByteString, [B.ByteString], B.ByteString) -> B.ByteString -> SplitM B.ByteString) -> SplitM B.ByteString
          while' regex str fn 
             = do let res = regex `matchOnceText` str
                  if isNothing res 
                     then return str
                     else do str' <- fn ((flatRegResult . fromJust) res) str
                             while' regex str' fn
                            
hGetLine :: Handle -> IO B.ByteString
hGetLine = fmap (`B.append` "\n") . B.hGetLine
          
-- | Display an error and immediately exit with a failure          
die :: String -> IO a
die msg = do name <- getProgName
             putStrLn $ name ++ ": " ++ msg
             exitFailure 