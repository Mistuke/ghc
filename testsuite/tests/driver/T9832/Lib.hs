module Lib
 ( summerize
 , single
 , MyPly()
 , munch
 , singleIO
 ) where

summerize :: [Int] -> [Int] -> Int
summerize x y = sum $ x ++ y

single :: Int -> [Int]
single x = [1..x]

data MyPly = MyPly { unPly :: Int -> [Int] -> [Int]
                   , ploo :: [Int]}

munch :: Int -> MyPly
munch i = MyPly { ploo  = [i]
                , unPly = (:)
                }

singleIO :: Int -> IO [Int]
singleIO = return . single