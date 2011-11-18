{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Benchmarks for data structures in RecSlowdown.
Amortimized time of head, tail, cons, snoc, last, init are /O(1)/.

-}
module RecSlowdown.BenchDQ where

import Prelude hiding (head, tail, last, init, (++))
import qualified Prelude as P
import RecSlowdown.ImplicitDeque
import Criterion.Main
import System.Random

mkQueue :: Int -> StdGen -> Queue Int
mkQueue n g
  | n == 0 = empty
  | even n = case random g of (!x,!g') -> cons x (mkQueue (n-1) g')
  | odd  n = case random g of (!x,!g') -> snoc x (mkQueue (n-1) g')

mkList :: Int -> StdGen -> [Int]
mkList n g
  | n == 0    = []
  | otherwise = case random g of (!x,!g') -> x : mkList (n-1) g'

main :: IO ()
main = do
  g <- newStdGen

  let mk f c n k = bench n (whnf f (c k g))
      qops k =
        [ mk head mkQueue "head" k
        , mk tail mkQueue "tail" k
        , mk init mkQueue "init" k
        , mk last mkQueue "last" k ]
      lops k =
        [ mk P.head mkList "head" k
        , mk P.tail mkList "tail" k
        , mk P.init mkList "init" k
        , mk P.last mkList "last" k ]

  defaultMain
    [ bgroup "n=100"
       [ bgroup "queue" (qops 100)
       , bgroup "list" (lops 100) ]
    , bgroup "n=1000"
       [ bgroup "queue" (qops 1000)
       , bgroup "list" (lops 1000) ]
    , bgroup "n=10000"
       [ bgroup "queue" (qops 10000)
       , bgroup "list" (lops 10000) ]
    ]
