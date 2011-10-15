{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Scratch written while reading:

* <http://blog.johantibell.com/2010/09/slides-from-my-high-performance-haskell.html>

-}

module Main where

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [
    bench "fib 10" (whnf fib 10)
  ]
