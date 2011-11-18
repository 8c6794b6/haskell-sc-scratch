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

SCD, Simple Catenable Deque.
-}
module RecSlowdown.SCD where

import Prelude hiding (head, tail, (++))

import RecSlowdown.ImplicitDeque (Queue)
import qualified RecSlowdown.ImplicitDeque as D
import Queue.Initial (emptyQueueException)

data Cat a
  = Shallow (Queue a)
  | Deep (Queue a) (Cat (Queue a)) (Queue a)
    deriving (Show)

empty :: Cat a
empty = Shallow D.empty

isEmpty :: Cat a -> Bool
isEmpty c = case c of Shallow d -> D.isEmpty d; _ -> False

cons :: a -> Cat a -> Cat a
cons x c = case c of
  Shallow d  -> Shallow (D.cons x d)
  Deep f m r -> Deep (D.cons x f) m r

head :: Cat a -> a
head c = case c of
  Shallow d
    | D.isEmpty d -> emptyQueueException
    | otherwise   -> D.head d
  Deep f _ _ -> D.head f

tail :: Cat a -> Cat a
tail c = case c of
  Shallow d
    | D.isEmpty d -> emptyQueueException
    | otherwise   -> Shallow (D.tail d)
  Deep f m r
    | D.isDeep f  -> Deep (D.tail f) m r
    | isEmpty m -> Shallow r
    | otherwise -> Deep (D.cons (D.last f) (head m)) (tail m) r

snoc :: a -> Cat a -> Cat a
snoc x c = case c of
  Shallow d -> Shallow (D.snoc x d)
  Deep f m r -> Deep f m (D.snoc x r)

shortAppendL :: Queue a -> Queue a -> Queue a
shortAppendL d1 d2
  | D.isEmpty d1 = d2
  | otherwise    = D.cons (D.head d1) d2

shortAppendR :: Queue a -> Queue a -> Queue a
shortAppendR d1 d2
  | D.isEmpty d2 = d1
  | otherwise    = D.snoc (D.last d2) d1

(++) :: Cat a -> Cat a -> Cat a
c1 ++ c2 = case (c1,c2) of
  (Shallow d1, Shallow d2)
    | D.lt2 d1  -> Shallow (shortAppendL d1 d2)
    | D.lt2 d2  -> Shallow (shortAppendR d1 d2)
    | otherwise -> Deep d1 empty d2
  (Shallow d, Deep f m r)
    | D.lt2 d   -> Deep (shortAppendL d f) m r
    | otherwise -> Deep d (cons f m) r
  (Deep f m r, Shallow d)
    | D.lt2 d   -> Deep f m (shortAppendR r d)
    | otherwise -> Deep f (snoc r m) d
  (Deep f1 m1 r1, Deep f2 m2 r2) -> Deep f1 (snoc r1 m1 ++ cons f2 m2) r2
