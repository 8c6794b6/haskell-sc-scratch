{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Parallel test, take 2. Looking for max with summing up every elements.
-}
module Hal6.ParTest4 where

import Control.Parallel.Strategies
import Hal6.Collatz

main :: IO ()
main = print $ partest 500000

partest :: Int -> (Int, Int)
partest n =
  let cs = map (collatz . fromIntegral) [1..n] `using`
           parList rdeepseq
  in  (maximum cs, sum cs)
