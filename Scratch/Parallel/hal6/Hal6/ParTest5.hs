{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC specific)

Parallel test, take 2. Looking for max with summing up every elements.
Chunking the list before creating sparks.
-}
module Hal6.ParTest5 where

import Control.Parallel.Strategies
import GHC.Conc
import Hal6.Collatz

main :: IO ()
main = print $ partest 500000

partest :: Int -> (Int, Int)
partest n =
  let cs = map (collatz . fromIntegral) [1..n] `using`
           parListChunk (n `div` (10 * numCapabilities)) rdeepseq
  in  (maximum cs, sum cs)
