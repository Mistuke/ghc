-- !!! test hIsEOF in various buffering situations

import System.IO

main = do
  h <- openFile "hIsEOF002.out" ReadWriteMode
  hSetBuffering h NoBuffering
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hPutChar h 'x'
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print 
