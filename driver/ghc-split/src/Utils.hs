module Utils (
    matchTest, 
    makeRegex, 
    matchTestIO, 
    matchTestAny, 
    matchTestAnyIO,
    matches,
    matchesIO
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B (splitAt, drop)
import Data.Maybe ( isJust )
import Regex ( Regex(..), regcomp, regIgnoreCase,  regExtended )
import ByteString ( regexec )

makeRegex :: String -> IO Regex
makeRegex = flip regcomp (regExtended + regIgnoreCase)

matchTest :: Regex -> ByteString -> IO Bool
matchTest reg str = fmap isJust $ regexec reg str 0

matchTestIO :: IO Regex -> ByteString -> IO Bool
matchTestIO reg str 
  = do reg' <- reg
       matchTest reg' str
       
matchTestAny :: [Regex] -> ByteString -> IO Bool
matchTestAny regs str = fmap (any isJust) $ mapM (\reg -> regexec reg str 0) regs

matchTestAnyIO :: IO [Regex] -> ByteString -> IO Bool
matchTestAnyIO regs str 
  = do regs' <- regs
       matchTestAny regs' str
       
matches :: Regex -> ByteString -> IO [ByteString]
matches reg str
  = do result <- regexec reg str 0
       case result of
         Just (_, idx) -> return $ breakUp 0 idx str
         _             -> return []
  where -- function only works under the assumption that the list is sorted
    breakUp :: Int -> [(Int, Int)] -> ByteString -> [ByteString]
    breakUp _       []      _   = []
    breakUp idx ((l, r):xs) str = let str1   = B.drop l str
                                      (s, v) = B.splitAt (r - l - idx) str1
                                  in s : breakUp r xs v
         

matchesIO :: IO Regex -> ByteString -> IO [ByteString]
matchesIO reg str = reg >>= flip matches str