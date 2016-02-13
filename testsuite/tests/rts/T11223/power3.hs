module Main where

foreign import ccall "power3" power2 :: Int -> Int

main = print $ power3 4