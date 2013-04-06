{-|
Module      : $Header$
Stability   : experimental
Portability : portable

-}
module Foo where

foreign export ccall foo :: Int -> IO Int

foo :: Int -> IO Int
foo n = return (length (f n))

f :: Int -> [Int]
f 0 = []
f n = n : f (n-1)

foreign export ccall fib :: Int -> IO Int

fib :: Int -> IO Int
fib n = return $ fibs !! n where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)