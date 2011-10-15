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
import Prelude hiding (foldl)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x in seq z' (foldl' f z' xs)

main :: IO ()
main = defaultMain
  [ bench "foldl" (whnf (foldl (+) 0) [1..10000])
  , bench "foldl'" (whnf (foldl' (+) 0) [1..10000])
  ]