{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Bootstrapped skew binomial heap based from /Purely Functional Data Structure/,
by Chris Okasaki.

This codes implements bootstrapped heap, shown in figure 7.6.

-}
module Data.BsbHeap
  ( -- * Type
    Heap

    -- * Heap Functions
  , empty
  , null
  , merge
  , insert
  , findMin
  , deleteMin

    -- * Converting functions
  , toList
  , toSortedList
  ) where

import Prelude hiding (null)

import Control.DeepSeq (NFData(..))

import Data.BsbHeap.Exception
import qualified Data.BsbHeap.SbHeap as H

-- | Bootstrapped heap.
--
-- Skewed binomial heap used as kernel.
--
data Heap a
   = Empty
   | Tip !a (H.SbHeap (Heap a))

------------------------------------------------------------------------------
--
-- * Instances
--

instance Show a => Show (Heap a) where
  show h = ("Heap " ++) . (showList $ toList h) $ ""

instance Eq a => Eq (Heap a) where
  {-# INLINE (==) #-}
  h1 == h2 = case (h1, h2) of
    (Empty, Empty) -> True
    (Empty, _    ) -> False
    (_    , Empty) -> False
    (Tip a1 p1, Tip a2 p2) -> a1 == a2 && p1 == p2

instance Ord a => Ord (Heap a) where
  {-# INLINE compare #-}
  compare h1 h2 = case (h1,h2) of
    (Empty, Empty)     -> EQ
    (Empty, _    )     -> LT
    (_    , Empty)     -> GT
    (Tip a _, Tip b _) -> compare a b

instance Functor Heap where
  {-# INLINE fmap #-}
  fmap f h = case h of
    Empty    -> Empty
    Tip a h' -> Tip (f a) (fmap (fmap f) h')

instance NFData a => NFData (Heap a) where
  {-# INLINE rnf #-}
  rnf h = case h of
    Empty    -> ()
    Tip x h' -> rnf x `seq` rnf h' `seq` ()

------------------------------------------------------------------------------
--
-- * Heap functions
--

empty :: Heap a
empty = Empty
{-# INLINE empty #-}

null :: Heap a -> Bool
null h = case h of Empty -> True; _ -> False
{-# INLINE null #-}

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 h2 = case (h1,h2) of
  (Empty,_) -> h2
  (_,Empty) -> h1
  (Tip !x p1, Tip !y p2)
    | x <= y    -> Tip x (H.insert h2 p1)
    | otherwise -> Tip y (H.insert h1 p2)
{-# INLINE merge #-}

insert :: Ord a => a -> Heap a -> Heap a
insert !x h = merge (Tip x H.empty) h
{-# INLINE insert #-}

findMin :: Heap a -> a
findMin h = case h of
  Empty    -> emptyHeap
  Tip !x _ -> x
{-# INLINE findMin #-}

deleteMin :: Ord a => Heap a -> Heap a
deleteMin h = case h of
  Empty -> emptyHeap
  Tip _ p
    | H.null p  -> Empty
    | otherwise -> case (H.findMin p, H.deleteMin p) of
      (Tip !y p1, p2) -> Tip y (H.merge p1 p2)
      _               -> emptyHeap
{-# INLINE deleteMin #-}

toList :: Heap a -> [a]
toList h = case h of
  Empty    -> []
  Tip !x p -> x : concatMap toList (H.toList p)
{-# INLINE toList #-}

toSortedList :: Ord a => Heap a -> [a]
toSortedList h = case h of
  Empty    -> []
  Tip !x _ -> x : toSortedList (deleteMin h)
{-# INLINE toSortedList #-}
