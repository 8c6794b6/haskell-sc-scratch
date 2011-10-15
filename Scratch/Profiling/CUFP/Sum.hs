{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Scratch written while reading:

* <http://blog.johantibell.com/2010/09/slides-from-my-high-performance-haskell.html>

-}
module Sum where

import Prelude hiding (sum)

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
