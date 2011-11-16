{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains /skew binomial heap/, shown in figure 6.10.

-}
module Tree.Skbh where

import Tree.Exception

------------------------------------------------------------------------------
-- Binary tree for skew binomial heap.

data Tree a = Node Int a [a] [Tree a] deriving (Show)

rank :: Tree a -> Int
rank (Node r _ _ _) = r

root :: Tree a -> a
root (Node _ x _ _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node _ x2 xs2 c2)
  | x1 <= x2  = Node (r+1) x1 xs1 (t2:c1)
  | otherwise = Node (r+1) x2 xs2 (t1:c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = case link t1 t2 of
  Node r y ys c
    | x <= y    -> Node r x (y:ys) c
    | otherwise -> Node r y (x:ys) c

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t ts = case ts of
  [] -> [t]
  t':ts'
    | rank t < rank t' -> t:t':ts'
    | otherwise        -> insTree (link t t') ts'

mergeTrees :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mergeTrees ts1 ts2 = case (ts1,ts2) of
  (_,[]) -> ts1
  ([],_) -> ts2
  (t1:ts1', t2:ts2')
    | rank t1 < rank t2 -> t1 : mergeTrees ts1' ts2
    | rank t1 > rank t2 -> t2 : mergeTrees ts1 ts2'
    | otherwise         -> insTree (link t1 t2) (mergeTrees ts1' ts2')

normalize :: Ord a => [Tree a] -> [Tree a]
normalize ts = case ts of
  [] -> []
  (t:ts') -> insTree t ts'

------------------------------------------------------------------------------
-- Functions for heap

newtype SBHeap a = SBHeap [Tree a] deriving (Show)

empty :: SBHeap a
empty = SBHeap []

isEmpty :: SBHeap a -> Bool
isEmpty (SBHeap hs) = null hs

insert :: Ord a => a -> SBHeap a -> SBHeap a
insert x (SBHeap ts) = SBHeap $ case ts of
  (t1:t2:rest)
    | rank t1 == rank t2 -> skewLink x t1 t2 : rest
    | otherwise          -> Node 0 x [] [] : ts
  _ -> Node 0 x [] [] : ts

merge :: Ord a => SBHeap a -> SBHeap a -> SBHeap a
merge (SBHeap ts1) (SBHeap ts2) =
  SBHeap $ mergeTrees (normalize ts1) (normalize ts2)

findMin :: Ord a => SBHeap a -> a
findMin (SBHeap ts) = case ts of
  []      -> emptyTreeException
  [t]     -> root t
  (t:ts') -> case (root t, findMin (SBHeap ts')) of
    (x,y) | x <= y    -> x
          | otherwise -> y

deleteMin :: Ord a => SBHeap a -> SBHeap a
deleteMin (SBHeap ts) = case ts of
  [] -> emptyTreeException
  _  ->
    let getMin xs = case xs of
          [y] -> (y,[])
          (y:ys) -> case getMin ys of
            (z,zs) | root y <= root z -> (y,ys)
                   | otherwise        -> (z,y:zs)
        (Node _ x xs c, ts') = getMin ts
        insertAll as bs = case (as,bs) of
          ([],_) -> bs
          (a:as',_) -> case insert a (SBHeap bs) of
            SBHeap bs' -> insertAll as' bs'
    in  SBHeap $ insertAll xs (mergeTrees (reverse c) (normalize ts'))

------------------------------------------------------------------------------
-- Tests

toList :: SBHeap a -> [a]
toList (SBHeap ts) = concatMap go ts where
  go (Node _ r rs ns) = r : rs ++ concatMap go ns
