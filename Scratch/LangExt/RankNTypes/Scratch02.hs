{-# LANGUAGE RankNTypes #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading web pages about rank n types.
-}
module Scratch02 where

-- | This function is rank 1.
f1a :: forall a b. a -> b -> a
f1a x y = x

-- | Again, rank 1 function.
f1b :: forall a. a -> forall b. b -> a
f1b x y = x

f2a :: (forall a. a -> a) -> (forall b. b -> b)
f2a g = id

f2b :: forall b. (forall a. a -> a) -> b -> b
f2b g = id