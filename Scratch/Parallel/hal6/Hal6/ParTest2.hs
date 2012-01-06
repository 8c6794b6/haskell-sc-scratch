{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Parallel test, take 2. Looking for max with summing up every elements.
-}
module Hal6.ParTest2 where

import Control.Parallel.Strategies
import Hal6.Collatz

main :: IO ()
main = print $ partest 500000

partest :: Int -> (Int, Int)
partest n = runEval $ do
  let h = n `div` 2
  r1 <- rpar $ maxCP 1     h
  r2 <- rpar $ maxCP (h+1) n
  let c (m1, s1) (m2, s2) = (m1 `max` m2, s1 + s2)
  return $ c r1 r2
