{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading /purely functional data structure/,
by Chris Okasaki.

Implicit queue, from figure 8.1.

/Remarks:/

In practice, these queues are slower than the implementation in
Chapter 3, 4, and 7. However, these queues have the advantage of
supporting random access efficiently. In particular, we can lookup and
update the ith element in O(log i) time. These queues contain a
logarithmetic number of trees of logarithmic depth. Random access is a
two stage process of finding the right tree and then finding the right
element.

-}
module RecSlowdown.ImplicitQueue where

import Prelude hiding (head, tail)
import System.Random

import Control.DeepSeq (NFData(..))
import Criterion.Main

import Queue.Initial (emptyQueueException)

data ZeroOne a
  = Zero
  | One a
  deriving (Show)

data OneTwo a
  = One' a
  | Two' a a
  deriving (Show)

data Queue a
  = Shallow (ZeroOne a)
  | Deep (OneTwo a) (Queue (a,a)) (ZeroOne a)
  deriving (Show)

instance Functor ZeroOne where
  fmap f zo = case zo of
    Zero  -> Zero
    One a -> One (f a)

instance Functor OneTwo where
  fmap f ot = case ot of
    One' a -> One' (f a)
    Two' a b -> Two' (f a) (f b)

instance Functor Queue where
  fmap f q = case q of
    Shallow d -> Shallow (fmap f d)
    Deep front m r ->
      Deep (fmap f front) (fmap (\(x,y) -> (f x,f y)) m) (fmap f r)

instance NFData a => NFData (Queue a) where
  rnf q = case q of
    Shallow h  -> rnf h `seq` ()
    Deep f m r -> rnf f `seq` rnf m `seq` rnf r `seq` ()

instance NFData a => NFData (ZeroOne a) where
  rnf zo = case zo of
    Zero  -> ()
    One a -> rnf a `seq` ()

instance NFData a => NFData (OneTwo a) where
  rnf ot = case ot of
    One' x -> rnf x `seq` ()
    Two' x y -> rnf x `seq` rnf y `seq` ()

empty :: Queue a
empty = Shallow Zero

isEmpty :: Queue a -> Bool
isEmpty q = case q of Shallow Zero -> True; _ -> False

snoc :: a -> Queue a -> Queue a
snoc y q = case q of
  Shallow Zero     -> Shallow (One y)
  Shallow (One x)  -> Deep (Two' x y) empty Zero
  Deep f m Zero    -> Deep f m (One y)
  Deep f m (One x) -> Deep f (snoc (x,y) m) Zero

head :: Queue a -> a
head q = case q of
  Shallow Zero        -> emptyQueueException
  Shallow (One x)     -> x
  Deep (One' x) _ _   -> x
  Deep (Two' x _) _ _ -> x

tail :: Queue a -> Queue a
tail q = case q of
  Shallow Zero        -> emptyQueueException
  Shallow (One _)     -> Shallow Zero
  Deep (Two' x y) m r -> Deep (One' y) m r
  Deep (One' x) m r
    | isEmpty m -> Shallow r
    | otherwise -> case head m of (y,z) -> Deep (Two' y z) (tail m) r

--
-- Theorem 8.3:
-- /snoc/ and /tail/ run in /O(1)/ amortized time.
--

mkQueue :: Int -> StdGen -> Queue Int
mkQueue n g
  | n == 0    = empty
  | otherwise = case random g of (!x,!g') -> snoc x (mkQueue (n-1) g)

main :: IO ()
main = do
  g <- newStdGen
  let test_snoc k = bench ("k="++show k) (whnf (snoc 1) (mkQueue k g))
      test_tail k = bench ("k="++show k) (whnf tail (mkQueue k g))
  defaultMain
    [ bgroup "snoc"
      [ test_snoc (10^3), test_snoc (10^4), test_snoc (10^5) ]
    , bgroup "tail"
      [ test_snoc (10^3), test_snoc (10^4), test_snoc (10^5) ]
    ]

{-

$ ghc --make -fforce-recomp -o a.out -rtsopts -threaded -O2 -main-is ImplicitQueue
$ ./a.out
warming up
estimating clock resolution...
mean is 16.38576 us (40001 iterations)
found 1338 outliers among 39999 samples (3.3%)
  1124 (2.8%) high severe
estimating cost of a clock call...
mean is 107.4185 ns (60 iterations)
found 7 outliers among 60 samples (11.7%)
  1 (1.7%) high mild
  6 (10.0%) high severe

benchmarking snoc/k=1000
mean: 33.72553 ns, lb 33.54973 ns, ub 33.97303 ns, ci 0.950
std dev: 1.056906 ns, lb 819.8027 ps, ub 1.408205 ns, ci 0.950

benchmarking snoc/k=10000
mean: 257.3619 ns, lb 164.3787 ns, ub 359.8819 ns, ci 0.950
std dev: 499.7148 ns, lb 461.8883 ns, ub 574.4629 ns, ci 0.950

benchmarking snoc/k=100000
collecting 100 samples, 1 iterations each, in estimated 161.7383 s
mean: 283.5879 ns, lb 190.6047 ns, ub 386.1079 ns, ci 0.950
std dev: 502.0819 ns, lb 469.5568 ns, ub 579.1613 ns, ci 0.950

benchmarking tail/k=1000
mean: 35.45154 ns, lb 35.09540 ns, ub 36.02014 ns, ci 0.950
std dev: 2.269715 ns, lb 1.570137 ns, ub 3.455337 ns, ci 0.950

benchmarking tail/k=10000
mean: 295.5089 ns, lb 200.1414 ns, ub 395.6447 ns, ci 0.950
std dev: 506.8339 ns, lb 475.0033 ns, ub 581.5210 ns, ci 0.950

benchmarking tail/k=100000
collecting 100 samples, 1 iterations each, in estimated 161.7520 s
mean: 386.1079 ns, lb 283.5879 ns, ub 493.3963 ns, ci 0.950
std dev: 535.0070 ns, lb 501.1662 ns, ub 610.6260 ns, ci 0.950

-}
