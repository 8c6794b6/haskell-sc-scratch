{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Parallel test, take 0, non-parallel.
-}

module Hal6.ParTest0 where

import Hal6.Collatz

main :: IO ()
main = print $ maxC 1 500000
