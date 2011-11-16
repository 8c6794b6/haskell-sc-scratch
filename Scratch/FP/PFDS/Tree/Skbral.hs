{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains /skew binary random access list/, shown in figure 6.9.

-}
module Skbral where

import Prelude hiding (head, tail, lookup)

import Tree.Exception

------------------------------------------------------------------------------
-- Complete binary tree.

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

-- First element in inner list is weight of the tree.
newtype RList a = RList [(Int, Tree a)] deriving (Show)

empty :: RList a
empty = RList []

isEmpty :: RList a -> Bool
isEmpty (RList ts) = null ts

cons :: a -> RList a -> RList a
cons x (RList ts) = case ts of
  (w1,t1):(w2,t2):ts'
    | w1 == w2  -> RList $ (1+w1+w2, Node x t1 t2) : ts'
    | otherwise -> RList $ (1, Leaf x) : ts
  _ -> RList $ (1, Leaf x) : ts

head :: RList a -> a
head (RList ts) = case ts of
  [] -> emptyTreeException
  (1, Leaf x):_ -> x
  (_, Node x t1 t2):_ -> x

tail :: RList a -> RList a
tail (RList ts) = case ts of
  [] -> emptyTreeException
  (1, Leaf x):ts' -> RList ts'
  (w, Node x t1 t2):ts' -> RList $ case w `div` 2 of
    w' -> (w', t1) : (w', t2) : ts'

{-

Not sure how (<) and `div` in ML are defined, though I suppose found
an errata in pdf, Last lookup case was written as:

> i < w `div` 2

but index get out of range when i == 1. Modified this to:

> i <= w `div` 2

-}

lookupTree ::
  Int        -- ^ Tree size
  -> Tree a  -- ^ Tree
  -> Int     -- ^ Index to lookup
  -> a
lookupTree w t i = case (w,t,i) of
  (1, Leaf x, 0)     -> x
  (1, Leaf _, _)     -> treeIndexOutOfRangeException
  (_, Node x _ _, 0) -> x
  (_, Node x t1 t2, _)
    | i <= w `div` 2 -> lookupTree (w `div` 2) t1 (i-1)
    | otherwise      -> lookupTree (w `div` 2) t2 (i-1- (w `div` 2))

-- Same modification as in lookupTree done here

updateTree ::
  Int       -- ^ Tree size
  -> Tree a -- ^ Tree
  -> Int    -- ^ Index
  -> a      -- ^ New element
  -> Tree a
updateTree w t i y = case (w,t,i,y) of
  (1, Leaf x, 0, y) -> Leaf y
  (1, _, _, _) -> treeIndexOutOfRangeException
  (_, Node _ t1 t2, 0, y) -> Node y t1 t2
  (_, Node x t1 t2, _, _)
    | i <= w `div` 2 -> Node x (updateTree (w `div` 2) t1 (i-1) y) t2
    | otherwise      -> Node x t1 (updateTree (w `div` 2) t2 (i-1-w `div` 2) y)

lookup :: Int -> RList a -> a
lookup i (RList ts) = case ts of
  [] -> treeIndexOutOfRangeException
  (w,t):ts'
    | i < w     -> lookupTree w t i
    | otherwise -> lookup (i-w) (RList ts')

update :: Int -> a -> RList a -> RList a
update i y rlist@(RList ts) = case ts of
  [] -> treeIndexOutOfRangeException
  n@(w,t):ts'
    | i < w     -> RList $ (w, updateTree w t i y) : ts'
    | otherwise -> RList $ n :
                   case update (i-w) y (RList ts') of RList ts'' -> ts''

------------------------------------------------------------------------------
-- Test

toList :: RList a -> [a]
toList (RList ts) = go ts where
  go us = case us of
    []        -> []
    (_,t):ts' -> go' t ++ go ts'
  go' t = case t of
    Leaf x -> [x]
    Node x t1 t2 -> x : go' t1 ++ go' t2