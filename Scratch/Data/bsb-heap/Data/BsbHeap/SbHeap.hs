{-# LANGUAGE BangPatterns #-}
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
module Data.BsbHeap.SbHeap where

import Control.DeepSeq (NFData(..))

import Data.BsbHeap.Exception

------------------------------------------------------------------------------
-- Binary tree for skew binomial heap.

data Tree a = Node {-# UNPACK #-} !Int !a [a] [Tree a] deriving (Show)

instance Eq a => Eq (Tree a) where
  Node r1 x1 xs1 ts1 == Node r2 x2 xs2 ts2 =
    r1 == r2 && x1 == x2 && xs1 == xs2 && ts1 == ts2

instance NFData a => NFData (Tree a) where
  -- {-# INLINE rnf #-}
  rnf (Node i x ns ts) = rnf i `seq` rnf x `seq` rnf ns `seq` rnf ts

rank :: Tree a -> Int
rank (Node !r _ _ _) = r
-- {-# INLINE rank #-}

root :: Tree a -> a
root (Node _ !x _ _) = x
-- {-# INLINE root #-}

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node !r !x1 xs1 c1) t2@(Node _ !x2 xs2 c2)
  | x1 <= x2  = Node (r+1) x1 xs1 (t2:c1)
  | otherwise = Node (r+1) x2 xs2 (t1:c2)
-- {-# INLINE link #-}

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = case link t1 t2 of
  Node r y ys c
    | x <= y    -> Node r x (y:ys) c
    | otherwise -> Node r y (x:ys) c
-- {-# INLINE skewLink #-}

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree !t ts = case ts of
  [] -> [t]
  (!t'):ts'
    | rank t < rank t' -> t:t':ts'
    | otherwise        -> insTree (link t t') ts'
-- {-# INLINE insTree #-}

mergeTrees :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mergeTrees ts1 ts2 = case (ts1,ts2) of
  (_,[]) -> ts1
  ([],_) -> ts2
  (!t1:ts1', !t2:ts2')
    | rank t1 < rank t2 -> t1 : mergeTrees ts1' ts2
    | rank t1 > rank t2 -> t2 : mergeTrees ts1 ts2'
    | otherwise         -> insTree (link t1 t2) (mergeTrees ts1' ts2')
-- {-# INLINE mergeTrees #-}

normalize :: Ord a => [Tree a] -> [Tree a]
normalize ts = case ts of
  []      -> []
  (t:ts') -> insTree t ts'
-- {-# INLINE normalize #-}

------------------------------------------------------------------------------
-- Functions for heap

newtype SbHeap a = SbHeap [Tree a] deriving (Eq,Show)

instance NFData a => NFData (SbHeap a) where
  -- {-# INLINE rnf #-}
  rnf (SbHeap xs) = rnf xs

empty :: SbHeap a
empty = SbHeap []
-- {-# INLINE empty #-}

isEmpty :: SbHeap a -> Bool
isEmpty (SbHeap hs) = null hs
-- {-# INLINE isEmpty #-}

insert :: Ord a => a -> SbHeap a -> SbHeap a
insert x (SbHeap ts) = SbHeap $ case ts of
  (t1:t2:rest)
    | rank t1 == rank t2 -> skewLink x t1 t2 : rest
    | otherwise          -> Node 0 x [] [] : ts
  _ -> Node 0 x [] [] : ts
-- {-# INLINE insert #-}

merge :: Ord a => SbHeap a -> SbHeap a -> SbHeap a
merge (SbHeap ts1) (SbHeap ts2) =
  SbHeap $ mergeTrees (normalize ts1) (normalize ts2)
-- {-# INLINE merge #-}

findMin :: Ord a => SbHeap a -> a
findMin (SbHeap ts) = case ts of
  []      -> emptyHeap
  [t]     -> root t
  (t:ts') -> case (root t, findMin (SbHeap ts')) of
    (x,y) | x <= y    -> x
          | otherwise -> y
-- {-# INLINE findMin #-}

deleteMin :: Ord a => SbHeap a -> SbHeap a
deleteMin (SbHeap ts) = case ts of
  [] -> emptyHeap
  _  ->
    let (Node _ !x xs c, ts') = getMin ts
    in  SbHeap $ insertAll xs (mergeTrees (reverse c) (normalize ts'))
-- {-# INLINE deleteMin #-}

getMin :: Ord a => [Tree a] -> (Tree a, [Tree a])
getMin xs = case xs of
  [y]    -> (y,[])
  (y:ys) -> case getMin ys of
    (z,zs) | root y <= root z -> (y,ys)
           | otherwise        -> (z,y:zs)
-- {-# INLINE getMin #-}

insertAll :: Ord a => [a] -> [Tree a] -> [Tree a]
insertAll as bs = case (as,bs) of
  ([],_)    -> bs
  (!a:as',_) -> case insert a (SbHeap bs) of
    SbHeap bs' -> insertAll as' bs'
-- {-# INLINE insertAll #-}

toList :: SbHeap a -> [a]
toList (SbHeap ts) = concatMap go ts where
  go (Node _ r rs ns) = r : rs ++ concatMap go ns
-- {-# INLINE toList #-}