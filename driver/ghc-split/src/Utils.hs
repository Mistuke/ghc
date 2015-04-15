module Utils (matchTest, makeRegex, matchTestIO, matchTestAny, matchTestAnyIO) where

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
       matchTest reg' str
       
matchTestAny :: [Regex] -> ByteString -> IO Bool
matchTestAny regs str = fmap (any isJust) $ mapM (\reg -> regexec reg str 0) regs

matchTestAnyIO :: IO [Regex] -> ByteString -> IO Bool
matchTestAnyIO regs str 
  = do regs' <- regs
       matchTestAny regs' str