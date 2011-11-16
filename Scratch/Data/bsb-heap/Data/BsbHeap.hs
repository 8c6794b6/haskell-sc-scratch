{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Bootstrapped skew binomial heap based from /purely functional data structure/,
by Chris Okasaki.

This codes implements bootstrapped heap, shown in figure 7.6.

-}
module Data.BsbHeap where

import Control.DeepSeq (NFData(..))

import Data.BsbHeap.Exception
import qualified Data.BsbHeap.SbHeap as H

data Heap a = Empty | Heap !a (H.SbHeap (Heap a))
   deriving (Show)

instance Eq a => Eq (Heap a) where
  h1 == h2 = case (h1, h2) of
    (Empty, Empty) -> True
    (Empty, _    ) -> False
    (_    , Empty) -> False
    (Heap a1 h1, Heap a2 h2) -> a1 == a2 && h1 == h2

instance NFData a => NFData (Heap a) where
  -- {-# INLINE rnf #-}
  rnf h = case h of
    Empty     -> ()
    Heap x h' -> rnf x `seq` rnf h' `seq` ()

instance Ord a => Ord (Heap a) where
  -- {-# INLINE compare #-}
  compare h1 h2 = case (h1,h2) of
    (Empty, Empty)         -> EQ
    (Empty, _    )         -> LT
    (_    , Empty)         -> GT
    (Heap !a _, Heap !b _) -> compare a b

empty :: Heap a
empty = Empty
-- {-# INLINE empty #-}

isEmpty :: Heap a -> Bool
isEmpty h = case h of Empty -> True; _ -> False
-- {-# INLINE isEmpty #-}

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 h2 = case (h1,h2) of
  (Empty,_) -> h2
  (_,Empty) -> h1
  (Heap !x p1, Heap !y p2)
    | x <= y    -> Heap x (H.insert h2 p1)
    | otherwise -> Heap y (H.insert h1 p2)
-- {-# INLINE merge #-}

insert :: Ord a => a -> Heap a -> Heap a
insert !x h = merge (Heap x H.empty) h
-- {-# INLINE insert #-}

findMin :: Heap a -> a
findMin h = case h of
  Empty     -> emptyHeap
  Heap !x _ -> x
-- {-# INLINE findMin #-}

deleteMin :: Ord a => Heap a -> Heap a
deleteMin h = case h of
  Empty -> emptyHeap
  Heap _ p
    | H.isEmpty p -> Empty
    | otherwise   -> case (H.findMin p, H.deleteMin p) of
      (Heap !y p1, p2) -> Heap y (H.merge p1 p2)
-- {-# INLINE deleteMin #-}

toList :: Heap a -> [a]
toList h = case h of
  Empty     -> []
  Heap !x p -> x : concatMap toList (H.toList p)
-- {-# INLINE toList #-}

-- toSortedList :: Ord a => Heap a -> [a]
-- toSortedList h = case h of
--   Empty -> []
--   _     -> findMin h : toSortedList (deleteMin h)

toSortedList :: Ord a => Heap a -> [a]
toSortedList h = case h of
  Empty -> []
  Heap !x p -> x : toSortedList (deleteMin h)

-- {-# INLINE toSortedList #-}
