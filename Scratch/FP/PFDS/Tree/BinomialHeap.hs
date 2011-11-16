{-# LANGUAGE BangPatterns #-}
{-|
module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains binomial heap shown in figure 6.8, from chapter 6.
-}
module Tree.BinomialHeap where

import Tree.Exception

import Criterion.Main
import System.Random

------------------------------------------------------------------------------
-- Tree used in this implementation. Rose tree with holding rank of itself.

data Tree a = Node Int a [Tree a] deriving (Show)

newtype BinomialHeap a = BinomialHeap [Tree a] deriving (Show)

empty :: BinomialHeap a
empty = BinomialHeap []

isEmpty :: BinomialHeap a -> Bool
isEmpty (BinomialHeap hs) = null hs

------------------------------------------------------------------------------
-- Functions for Tree

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r+1) x1 (t2:c1)
  | otherwise = Node (r+1) x2 (t1:c2)

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t ts = case ts of
  [] -> [t]
  t':rest
    | rank t < rank t' -> t:ts
    | otherwise        -> insTree (link t t') rest

------------------------------------------------------------------------------
-- Heap functions

insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
insert a (BinomialHeap ts) = BinomialHeap (insTree (Node 0 a []) ts)

merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge (BinomialHeap ts1) (BinomialHeap ts2) = BinomialHeap (merge' ts1 ts2)

merge' :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
merge' ts1 ts2 =  case (ts1,ts2) of
  (_,[]) -> ts1
  ([],_) -> ts2
  (n1:ns1,n2:ns2)
    | rank n1 < rank n2 -> n1 : merge' ns1 ts2
    | rank n2 < rank n1 -> n2 : merge' ts1 ns2
    | otherwise         -> insTree (link n1 n2) (merge' ns1 ns2)

findMin :: Ord a => BinomialHeap a -> a
findMin (BinomialHeap ts) = findMin' ts where
  findMin' ts = case ts of
    [] -> emptyTreeException
    [t] -> root t
    (n:ns) ->
      let x = root n
          y = findMin' ns
      in  if x <= y then x else y

deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin (BinomialHeap ts) = BinomialHeap (deleteMin' ts) where
  deleteMin' us = case us of
    [] -> emptyTreeException
    (x:xs) ->
      let getMin ys = case ys of
            [y] -> (y,[])
            (z:zs) -> let (z',zs') = getMin zs
                      in  if root z <= root z' then (z,zs) else (z',z:zs')
          (Node _ x ts1,ts2) = getMin us
      in  merge' (reverse ts1) ts2

------------------------------------------------------------------------------
-- Test

toList :: BinomialHeap a -> [a]
toList = toList_plain

toList_manual :: BinomialHeap a -> [a]
toList_manual (BinomialHeap ts) = go ts [] where
  go us = case us of
      []     -> id
      (v:vs) -> go' v . go vs
  go' (Node _ x ns) = (x:) . go ns

toList_plain :: BinomialHeap a -> [a]
toList_plain (BinomialHeap ts) = concatMap go ts where
  go (Node _ x ns) = x : go' ns
  go' ns = case ns of
    []   -> []
    x:xs -> go x ++ go' xs

-- tolist_plain performed better than tolist_manual.
--
main :: IO ()
main = do
  g <- newStdGen
  let h' = foldr insert empty (take 100000 $ randomRs (1,maxBound::Int) g)
      !h = h' `seq` h'
  defaultMain
    [ bench "tolist_plain" (whnf toList_plain h)
    , bench "tolist_manual" (whnf toList_manual h) ]
