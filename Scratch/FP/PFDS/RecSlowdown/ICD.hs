{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Scratch written while reading /purely functional data structure/,
by Chris Okasaki.

Implicit catenable deque, from figure 8.5 and 8.6.

-}
module RecSlowdown.ICD
  ( -- * Concatenable Double Ended Queue
    Cat
  , empty
  , null
  , cons
  , head
  , tail
  , snoc
  , last
  , init
  , (++)
  , toList
  ) where

import Prelude hiding (null, head, tail, last, init, (++))
import qualified Prelude as P

import RecSlowdown.ImplicitDeque (Queue)
import qualified RecSlowdown.ImplicitDeque as D
import Queue.Initial (emptyQueueException)

import Control.DeepSeq (NFData(..))

------------------------------------------------------------------------------
-- Types

data Cat a
  = Shallow (Queue a)
  | Deep (Queue a) (Cat (CmpdElem a)) (Queue a) (Cat (CmpdElem a)) (Queue a)
  deriving (Show)

data CmpdElem a
  = Simple (Queue a)
  | CE (Queue a) (Cat (CmpdElem a)) (Queue a)
  deriving (Show)

empty :: Cat a
empty = Shallow D.empty
{-# INLINE empty #-}

null :: Cat a -> Bool
null c = case c of
  Shallow d
    | D.null d -> True
    | otherwise   -> False
  _ -> False
{-# INLINE null #-}

cons :: a -> Cat a -> Cat a
cons x c = case c of
  Shallow d      -> Shallow (D.cons x d)
  Deep f a m b r -> Deep (D.cons x f) a m b r
{-# INLINE cons #-}

head :: Cat a -> a
head c = case c of
  Shallow d
    | D.null d -> emptyQueueException
    | otherwise   -> D.head d
  Deep f _ _ _ _ -> D.head f
{-# INLINE head #-}

snoc :: a -> Cat a -> Cat a
snoc x c = case c of
  Shallow d      -> Shallow (D.snoc x d)
  Deep f a m b r -> Deep f a m b (D.snoc x r)
{-# INLINE snoc #-}

last :: Cat a -> a
last c = case c of
  Shallow d
    | D.null d -> emptyQueueException
    | otherwise   -> D.last d
  Deep _ _ _ _ r -> D.last r
{-# INLINE last #-}

share :: Queue a -> Queue a -> (Queue a, Queue a, Queue a)
share f r = (D.init f, D.cons (D.last f) (D.cons (D.head r) D.empty), D.tail r)
{-# INLINE share #-}

shortAppendL :: Queue a -> Queue a -> Queue a
shortAppendL d1 d2
  | D.null d1 = d2
  | otherwise    = shortAppendL (D.init d1) (D.cons (D.last d1) d2)
{-# INLINE shortAppendL #-}

shortAppendR :: Queue a -> Queue a -> Queue a
shortAppendR d1 d2
  | D.null d2 = d1
  | otherwise    = shortAppendR (D.snoc (D.head d2) d1) (D.tail d2)
{-# INLINE shortAppendR #-}

(++) :: Cat a -> Cat a -> Cat a
c1 ++ c2 = case (c1,c2) of
  (Shallow d1, Shallow d2)
    | D.lt4 d1 -> Shallow (shortAppendL d1 d2)
    | D.lt4 d2 -> Shallow (shortAppendR d1 d2)
    | otherwise -> case share d1 d2 of ~(f, m, r) -> Deep f empty m empty r
  (Shallow d, Deep f a m b r)
    | D.lt3 d   -> Deep (shortAppendL d f) a m b r
    | otherwise -> Deep d (cons (Simple f) a) m b r
  (Deep f a m b r, Shallow d)
    | D.lt3 d   -> Deep f a m b (shortAppendR r d)
    | otherwise -> Deep f a m (snoc (Simple r) b) d
  (Deep f1 a1 m1 b1 r1, Deep f2 a2 m2 b2 r2) -> case share r1 f2 of
    (r1', m, f2') -> case (snoc (CE m1 b1 r1') a1, cons (CE f2' a2 m2) b2) of
      (a1', b2') -> Deep f1 a1' m b2' r2
{-# INLINE (++) #-}

replaceHead :: a -> Cat a -> Cat a
replaceHead x c = case c of
  Shallow d      -> Shallow (D.cons x (D.tail d))
  Deep f a m b r -> Deep (D.cons x (D.tail f)) a m b r
{-# INLINE replaceHead #-}

tail :: Cat a -> Cat a
tail c = case c of
  Shallow d
    | D.null d -> emptyQueueException
    | otherwise   -> Shallow (D.tail d)
  Deep f a m b r
    | not (D.lt3 f) -> Deep (D.tail f) a m b r
    | not (null a) -> case head a of
      Simple d    -> case shortAppendL (D.tail f) d of
        f' -> Deep f' (tail a) m b r
      CE f' a' r' ->
        case (shortAppendL (D.tail f) f', a' ++ replaceHead (Simple r') a) of
          (f'', a'') -> Deep f'' a'' m b r
    | not (null b) -> case head b of
      Simple d -> case shortAppendL (D.tail f) m of
        f' -> Deep f' empty d (tail b) r
      CE f' a' r' ->
        case (shortAppendL (D.tail f) m, cons (Simple f') a') of
          (f'',a'') -> Deep f'' a'' r' (tail b) r
    | otherwise -> Shallow (shortAppendL (D.tail f) m) ++ Shallow r
{-# INLINE tail #-}

replaceLast :: a -> Cat a -> Cat a
replaceLast x c = case c of
  Shallow d -> Shallow (D.snoc x (D.init d))
  Deep f a m b r -> Deep f a m b (D.snoc x (D.init r))
{-# INLINE replaceLast #-}

init :: Cat a -> Cat a
init c = case c of
  Shallow d
    | D.null d -> emptyQueueException
    | otherwise   -> Shallow (D.init d)
  Deep f a m b r
    | not (D.lt3 r) -> Deep f a m b (D.init r)
    | not (null b) -> case last b of
      Simple d ->
        case shortAppendR d (D.init r) of r' -> Deep f a m (init b) r'
      CE f' b' r' ->
        case (shortAppendR (D.init r) r', replaceLast (Simple f') b ++ b') of
          (r'', b'') -> Deep f a m b'' r''
    | not (null a) -> case last a of
      Simple d -> case shortAppendR m (D.init r) of
        r' -> Deep f (init a) d empty r'
      CE f' b' r' ->
        case (shortAppendR (D.init r) m, snoc (Simple r') b') of
          (r'', b'') -> Deep f (init a) f' b'' r''
    | otherwise -> Shallow f ++ Shallow (shortAppendR m (D.init r))
{-# INLINE init #-}

------------------------------------------------------------------------------
-- Instances

instance Functor Cat where
  {-# INLINE fmap #-}
  fmap f c = case c of
    Shallow d       -> Shallow (fmap f d)
    Deep ft a m b r ->
      Deep (fmap f ft) (fmap (fmap f) a) (fmap f m) (fmap (fmap f) b) (fmap f r)

instance NFData a => NFData (Cat a) where
  {-# INLINE rnf #-}
  rnf c = case c of
    Shallow d      -> rnf d `seq` ()
    Deep f a m b r ->
      rnf f `seq` rnf a `seq` rnf m `seq` rnf b `seq` rnf r `seq` ()

instance Functor CmpdElem where
  {-# INLINE fmap #-}
  fmap f c = case c of
    Simple d -> Simple (fmap f d)
    CE ft m b -> CE (fmap f ft) (fmap (fmap f) m) (fmap f b)

instance NFData a => NFData (CmpdElem a) where
  {-# INLINE rnf #-}
  rnf c = case c of
    Simple d -> rnf d `seq` ()
    CE f m r -> rnf f `seq` rnf m `seq` rnf r `seq` ()


------------------------------------------------------------------------------
-- Converting

toList :: Cat a -> [a]
toList c = case c of
  Shallow d      -> D.toList d
  Deep f a m b r ->
    D.toList f P.++ concatMap toListCE (toList a) P.++ D.toList m P.++
    concatMap toListCE (toList b) P.++ D.toList r
{-# INLINE toList #-}

toListCE :: CmpdElem a -> [a]
toListCE c = case c of
  Simple d -> D.toList d
  CE f m r -> D.toList f P.++ concatMap toListCE (toList m) P.++ D.toList r
{-# INLINE toListCE #-}
