{-# LANGUAGE Rank2Types #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading web pages about rank n types.
-}
module Scratch01 where

import Control.Monad.ST
import Data.STRef

-- | We cannot write this function in Haskell 98.
--
-- From: <http://en.wikibooks.org/wiki/Haskell/Polymorphism>
--
foo :: (forall a. a -> a) -> (Char, Bool)
foo f = (f 'c', f True)

-- > > foo id
-- > ('c',True)

-- ---------------------------------------------------------------------------
-- From:
-- <http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/other-type-extensions.html>

data T a = T1 (forall b. b -> b -> b) a


-- | When adding {-# LANGUAGE GADTs #-} on top of the code, this data type will
-- not compile. Suppose there's a conflict in Rank2Types and GADT?
--
data MonadT m = MkMonad
  { returnT :: forall a. a -> m a
  , bindT   :: forall a b. m a -> (a -> m b) -> m b }

-- Not working?
--
-- data MonadT2 m where
--   MonadT2 :: (forall a. a -> m a) -> (forall a b. m a -> (a -> m b) -> m b) -> MonadT2 m

newtype Swizzle = MkSwizzle (Ord a => [a] -> [a])

a1 :: T Int
a1 = T1 (\x y -> x) 3

a2, a3 :: Swizzle
a2 = MkSwizzle (concat . replicate 3)
a3 = MkSwizzle reverse

a4 :: MonadT Maybe
a4 = let r x = Just x
         b m k = case m of Just y -> k y; Nothing -> Nothing
     in  MkMonad r b

mkTs :: (forall b. b -> b -> b) -> a -> a -> [T a]
mkTs f x y = [T1 f x, T1 f y]

f :: T a -> a -> (a, Char)
f (T1 w k) x = (w k x, w 'c' 'd')

-- > f a1 == (3,'c')

g :: (Ord a, Ord b) => Swizzle -> [a] -> (a -> b) -> [b]
g (MkSwizzle s) xs f = s (map f (s xs))

h :: MonadT m -> [m a] -> m [a]
h m [] = returnT m []
h m (x:xs) =
  bindT m x $ \y ->
  bindT m (h m xs) $ \ys ->
  returnT m (y:ys)
