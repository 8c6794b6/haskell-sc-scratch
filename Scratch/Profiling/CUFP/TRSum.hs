{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Scratch written while reading:

* <http://blog.johantibell.com/2010/09/slides-from-my-high-performance-haskell.html>

-}
module TRSum where

import Prelude hiding (sum)

sum :: [Int] -> Int
sum = sum' 0 where
  sum' acc [] = acc
  sum' acc (x:xs) = sum' (acc+x) xs