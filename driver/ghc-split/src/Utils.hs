module Utils (matchTest, makeRegex, matchTestIO) where

import Data.ByteString (ByteString)
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
       fmap isJust $ regexec reg' str 0