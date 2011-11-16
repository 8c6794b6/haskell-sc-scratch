{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains bootstrapped heap, shown in figure 7.6.

-}
module Bootstrap.Heap where

import Tree.Exception
import qualified Tree.Skbh as H

import Criterion.Main
import Control.DeepSeq (NFData(..))
import System.Random
import Data.List (sort)

data Heap a = Empty | Heap a (H.SBHeap (Heap a))
   deriving (Show, Eq)

instance NFData a => NFData (Heap a) where
  rnf h = case h of
    Empty -> ()
    Heap x h' -> rnf x `seq` rnf h' `seq` ()

instance Ord a => Ord (Heap a) where
  compare h1 h2 = case (h1,h2) of
    (Empty, Empty)       -> EQ
    (Empty, Heap _ _)    -> LT
    (Heap _ _, Empty)    -> GT
    (Heap a _, Heap b _) -> compare a b

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty h = case h of Empty -> True; _ -> False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 h2 = case (h1,h2) of
  (Empty,_) -> h2
  (_,Empty) -> h1
  (Heap x p1, Heap y p2)
    | x <= y    -> Heap x (H.insert h2 p1)
    | otherwise -> Heap y (H.insert h1 p2)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (Heap x H.empty) h

findMin :: Heap a -> a
findMin h = case h of
  Empty -> emptyTreeException
  Heap x _ -> x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin h = case h of
  Empty -> emptyTreeException
  Heap x p
    | H.isEmpty p -> Empty
    | otherwise   -> case (H.findMin p, H.deleteMin p) of
      (Heap y p1, p2) -> Heap y (H.merge p1 p2)

toList :: Heap a -> [a]
toList h = case h of
  Empty -> []
  Heap x p -> x : concatMap toList (H.toList p)

toSortedList :: Ord a => Heap a -> [a]
toSortedList h = case h of
  Empty -> []
  _     -> findMin h : toSortedList (deleteMin h)

mkList :: Int -> StdGen -> [Int]
mkList n g = go n g [] where
  go k gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (x,gen') -> go (k-1) gen' (x:acc)

mkHeap :: Int -> StdGen -> Heap Int
mkHeap n g = go n g empty where
  go k gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (x,gen') -> go (k-1) gen' (insert x acc)

main :: IO ()
main = do
  g <- newStdGen
  let n = 10000
      k = 100
  defaultMain
    [ bgroup "bare sorting"
        [bench "list" (whnf sort (mkList n g))
        , bench "heap" (whnf toSortedList (mkHeap n g))
        ]
    , bgroup "bare insert"
        [ bench "list" (nfIO (mkList k `fmap` newStdGen))
        , bench "heap" (nfIO (mkHeap k `fmap` newStdGen))
        ]
    , bgroup "insert and sort"
        [ bench "list" (nfIO ((sort . mkList k) `fmap` newStdGen))
        , bench "heap" (nfIO ((toSortedList . mkHeap k) `fmap` newStdGen))
        ]
    ]