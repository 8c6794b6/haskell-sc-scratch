{-# LANGUAGE NoImplicitPrelude #-}
{-|
module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains random access list shown in figure 6.6, from chapter 6.2.
-}
module Tree.RandomAccessList where

import Prelude hiding (head, tail, lookup)
import Tree.Exception

------------------------------------------------------------------------------
-- Inner guts of random access list
--
-- Using binary tree and dense natural number to implement data structure.
--

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show)

data Digit a = Zero | One (Tree a) deriving (Show)

------------------------------------------------------------------------------
-- Random access list

type RList a = [Digit a]

empty :: RList a
empty = []

isEmpty :: RList a -> Bool
isEmpty = null

size :: Tree a -> Int
size t = case t of
  Leaf _     -> 1
  Node w _ _ -> w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

insTree :: Tree a -> RList a -> RList a
insTree t rs = case rs of
  []          -> [One t]
  Zero:ts     -> One t:ts
  One t' : ts -> Zero : insTree (link t t') ts

borrowTree :: RList a -> (Tree a, RList a)
borrowTree rs = case rs of
  []         -> emptyTreeException
  [One t]    -> (t, [])
  One t : ts -> (t, Zero:ts)
  Zero  : ts ->
    let (Node _ t1 t2, ts') = borrowTree ts
    in  (t1, One t2 : ts')

lookupTree :: Tree a -> Int -> a
lookupTree t i = case (t,i) of
  (Leaf x,0)  -> x
  (Leaf _, _) -> treeIndexOutOfRangeException
  (Node w t1 t2, _)
    | i < w `div` 2 -> lookupTree t1 i
    | otherwise     -> lookupTree t2 (i - w `div` 2)

updateTree :: Tree a -> Int -> a -> Tree a
updateTree t i y = case (t,i) of
  (Leaf x, 0) -> Leaf y
  (Leaf x, i) -> treeIndexOutOfRangeException
  (Node w t1 t2, i)
    | i < w `div` 2 -> Node w (updateTree t1 i y) t2
    | otherwise     -> Node w t1 (updateTree t2 (i - w `div` 2) y)

------------------------------------------------------------------------------
-- Functions for random access list

cons :: a -> RList a -> RList a
cons x = insTree (Leaf x)

head :: RList a -> a
head xs = case borrowTree xs of (Leaf x,_) -> x

tail :: RList a -> RList a
tail xs = case borrowTree xs of (_,xs') -> xs'

lookup :: Int -> RList a -> a
lookup i rs = case rs of
  []      -> treeIndexOutOfRangeException
  Zero:ts -> lookup i ts
  One t:ts
    | i < size t -> lookupTree t i
    | otherwise  -> lookup (i - size t) ts

update :: Int -> a -> RList a -> RList a
update i y rs = case rs of
  [] -> treeIndexOutOfRangeException
  Zero:ts -> Zero : update i y ts
  One t:ts | i < size t -> One (updateTree t i y) : ts
           | otherwise  -> One t : update (i-size t) y ts

------------------------------------------------------------------------------
-- test

r1 :: RList Int
r1 = foldr cons empty [1..30]

toList :: RList a -> [a]
toList = concatMap toListDigit

toListDigit :: Digit a -> [a]
toListDigit d = go d [] where
  go v = case v of
    Zero  -> id
    One t -> toListTree t

toListTree :: Tree a -> ([a] -> [a])
toListTree t = go t where
  go v = case v of
    Leaf x       -> (x:)
    Node _ v1 v2 -> toListTree v1 . toListTree v2

check_update :: Bool
check_update =
  let v = 999
      i = 17
  in  toList (update i v r1) !! i == v

main :: IO ()
main = putStrLn $ if check_update then "OK" else "NG"