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

Implicit deque, from figure 8.2. This deque is used by catenable deque.

-}
module RecSlowdown.ImplicitDeque
  ( -- * Implicit Deque
    Queue
    -- * Queue functions
  , empty
  , null
  , cons
  , head
  , tail
  , snoc
  , last
  , init
    -- * Utilities for getting size
  , geq2
  , lt2
  , lt4
  , lt3
    -- * Converting to list
  , toList
  ) where

import Prelude hiding (null, head, tail, last, init)
import qualified Prelude as P
import Queue.Initial (emptyQueueException)

import Control.DeepSeq (NFData(..))
import Criterion.Main
import System.Random

------------------------------------------------------------------------------
-- Digits

-- | Guts of recursive slowdown.
--
-- Used as a layer for supporting element addition in /O(1)/.
--
data D a
  = Zero
  | One a
  | Two a a
  | Three a a a
  deriving (Show)

instance Functor D where
  {-# INLINE fmap #-}
  fmap f d = case d of
    Zero        -> Zero
    One a       -> One (f a)
    Two a b     -> Two (f a) (f b)
    Three a b c -> Three (f a) (f b) (f c)

instance NFData a => NFData (D a) where
  {-# INLINE rnf #-}
  rnf d = case d of
    Zero        -> ()
    One a       -> rnf a `seq` ()
    Two a b     -> rnf a `seq` rnf b `seq` ()
    Three a b c -> rnf a `seq` rnf b `seq` rnf c `seq` ()

dcons :: a -> D a -> D a
dcons x d = case d of
  Zero     -> One x
  One a   -> Two x a
  Two a b -> Three x a b
{-# INLINE dcons #-}

dsnoc :: a -> D a -> D a
dsnoc x d = case d of
  Zero      -> One x
  One a    -> Two a x
  Two a b -> Three a b x
{-# INLINE dsnoc #-}

dhead :: D a -> a
dhead d = case d of
  Zero         -> emptyQueueException
  One a       -> a
  Two a _     -> a
  Three a _ _ -> a
{-# INLINE dhead #-}

dlast :: D a -> a
dlast d = case d of
  Zero         -> emptyQueueException
  One a       -> a
  Two _ a     -> a
  Three _ _ a -> a
{-# INLINE dlast #-}

dtail :: D a -> D a
dtail d = case d of
  Zero          -> emptyQueueException
  One a        -> Zero
  Two _ b      -> One b
  Three _ a b -> Two a b
{-# INLINE dtail #-}

dinit :: D a -> D a
dinit d = case d of
  Zero          -> emptyQueueException
  One a        -> Zero
  Two a _      -> One a
  Three a b _ -> Two a b
{-# INLINE dinit #-}

dlist :: D a -> [a]
dlist d = case d of
  Zero        -> []
  One a       -> [a]
  Two a b     -> [a,b]
  Three a b c -> [a,b,c]
{-# INLINE dlist #-}

------------------------------------------------------------------------------
-- Implicit deque

data Queue a
  = Shallow (D a)
  | Deep (D a) (Queue (a,a)) (D a)

instance Show a => Show (Queue a) where
  show q = "Queue " ++ showList (toList q) ""

instance Functor Queue where
  {-# INLINE fmap #-}
  fmap f q = case q of
    Shallow d -> Shallow (fmap f d)
    Deep front m r ->
      Deep (fmap f front) (fmap (\(x,y) -> (f x,f y)) m) (fmap f r)

instance NFData a => NFData (Queue a) where
  {-# INLINE rnf #-}
  rnf q = case q of
    Shallow d  -> rnf d `seq` ()
    Deep f m r -> rnf f `seq` rnf m `seq` rnf r `seq` ()

------------------------------------------------------------------------------
-- Queue size utils

-- | Whether the size greater or equal to 2
geq2 :: Queue a -> Bool
geq2 q = case q of
  Shallow d ->
    case d of Zero -> False; One _ -> False; _ -> True
  _         -> True
{-# INLINE geq2 #-}

-- | Whether the size lesser than 2
lt2 :: Queue a -> Bool
lt2 q = case q of
  Shallow d ->
    case d of Zero -> True; One _ -> True; _ -> False
  _         -> False
{-# INLINE lt2 #-}

-- | Whether the size lesser than 4
lt4 :: Queue a -> Bool
lt4 q = case q of
  Shallow _ -> True
  _         -> False
{-# INLINE lt4 #-}

-- | Whether the size lesser than 3
lt3 :: Queue a -> Bool
lt3 q = case q of
  Shallow d -> case d of
    Three _ _ _ -> False
    _           -> True
  _ -> False
{-# INLINE lt3 #-}

isDeep :: Queue a -> Bool
isDeep q = case q of Shallow _ -> False; _ -> True
{-# INLINE isDeep #-}

isShallow :: Queue a -> Bool
isShallow = not . isDeep
{-# INLINE isShallow #-}

empty :: Queue a
empty = Shallow Zero
{-# INLINE empty #-}

null :: Queue a -> Bool
null q = case q of Shallow Zero -> True; _ -> False
{-# INLINE null #-}

toList :: Queue a -> [a]
toList q = case q of
  Shallow d  -> dlist d
  Deep f m r -> dlist f ++ concatMap (\(a,b) -> [a,b]) (toList m) ++ dlist r
{-# INLINE toList #-}

cons :: a -> Queue a -> Queue a
cons x ~q = case q of
  Shallow (Three a b c)   -> Deep (Two x a) empty (Two b c)
  Shallow d               -> Shallow (dcons x d)
  Deep (Three a b c) ~m r -> Deep (Two x a) (cons (b, c) m) r
  Deep f ~m r             -> Deep (dcons x f) m r
{-# INLINE cons #-}

head :: Queue a -> a
head ~q = case q of
  Shallow d  -> dhead d
  Deep f _ _ -> dhead f
{-# INLINE head #-}

tail :: Queue a -> Queue a
tail ~q = case q of
  Shallow d -> Shallow (dtail d)
  Deep (One a) ~m r
    | null m    -> Shallow r
    | otherwise -> case head m of (b,c) -> Deep (Two b c) (tail m) r
  Deep f ~m r -> Deep (dtail f) m r
{-# INLINE tail #-}

snoc :: a -> Queue a -> Queue a
snoc x ~q = case q of
  Shallow (Three a b c)   -> Deep (Two a b) empty (Two c x)
  Shallow d               -> Shallow (dsnoc x d)
  Deep f ~m (Three a b c) -> Deep f (snoc (a, b) m) (Two c x)
  Deep f ~m r             -> Deep f m (dsnoc x r)
{-# INLINE snoc #-}

last :: Queue a -> a
last ~q = case q of
  Shallow d  -> dlast d
  Deep _ _ r -> dlast r
{-# INLINE last #-}

init :: Queue a -> Queue a
init ~q = case q of
  Shallow d -> Shallow (dinit d)
  Deep f ~m (One a)
    | null m    -> Shallow f
    | otherwise -> case last m of (b,c) -> Deep f (init m) (Two b c)
  Deep f ~m r -> Deep f m (dinit r)
{-# INLINE init #-}
