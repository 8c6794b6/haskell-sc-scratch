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

main :: IO ()
main = defaultMain
  [ bench "whnf" (whnf (replicate n) 'a')
  , bench "nf" (nf (replicate n) 'a')
  ] where n = 10000