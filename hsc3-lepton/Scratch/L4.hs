{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

L Instances of classes in PC02, using System.Random.MWC and ST monad.

-}
module Scratch.L4 where

import Control.Applicative
import Control.Monad

import Control.Monad.ST
import Control.Monad.Primitive
import Data.Word
import Data.Vector.Generic (Vector(..))
import Sound.SC3
import System.Random.MWC

import Scratch.PC02
import Scratch.Type00

import qualified Data.Vector.Primitive as V
import qualified Data.Map as M
import qualified Data.Traversable as T

------------------------------------------------------------------------------
-- L, with MWC random and ST

newtype L h a = L {unL :: forall s. h -> Gen (PrimState (ST s)) -> ST s [a]}

toL :: L () a -> L () a
toL = id

-- | Run L with default seed.
runL' :: L () a -> [a]
runL' l = runST (unL l () =<< create)

-- | Run L with given seed.
--
-- > *Scratch.L4> runL (pint 3 *! pint 8) (initialize (V.fromList [1,2,3]))
-- > [24]
--
runL :: L () a -> (forall s. ST s (Gen s)) -> [a]
runL l sd = runST ((unL l ()) =<< sd)

-- | Run L with using seed from system random.
runLIO :: L () a -> IO [a]
runLIO l = withSystemRandom (unL l ())

------------------------------------------------------------------------------
-- Helpers

constL2 :: a -> L h a
constL2 x = L $ \_ _ -> return [x]

liftL2 :: ([a]->[b]->[c]) -> L h a -> L h b -> L h c
liftL2 f a b = L $ \h g -> do
  as <- unL a h g
  bs <- unL b h g
  return $ f as bs

rangeL :: Variate a => L h a -> L h a -> L h a
rangeL lo hi = L $ \h g -> do
  los <- unL lo h g
  his <- unL hi h g
  zipWithM uniformR (zip los his) (repeat g)

------------------------------------------------------------------------------
-- Base classes

instance Functor (L h) where
  fmap f (L l) = L $ \h g -> l h g >>= return . map f

instance Applicative (L h) where
  pure a = L $ \_ _ -> return $ repeat a
  L a <*> L b = L $ \h g -> do
    as <- a h g
    bs <- b h g
    return $ zipWith ($) as bs

instance Pint L where
  pint = constL2
  (+!) = liftA2 (+)
  (*!) = liftA2 (*)
  (-!) = liftA2 (-)
  piabs = fmap abs
  pinegate = fmap negate
  pisignum = fmap signum
  pirange = rangeL

instance Pappend L where
  pappend = liftL2 (++)

instance Pconcat L where
  pconcat = foldr1 pappend

instance Preplicate L where
  preplicate n p = L $ \h g -> do
    n' <- unL n h g
    concat <$> mapM (\p' -> unL p' h g) (replicate (sum n') p)

instance Pseq L where
  pseq n = preplicate n . pconcat

-- | XXX: Using 'unsafeInterleaveST'.
instance Pforever L where
  pforever p = L $ \h g ->
    let go = do
          xs <- unL p h g
          (xs ++) `liftM` unsafeInterleaveST go
    in  go

instance Pcycle L where
  pcycle = pforever . pconcat

instance Prand L where
  prand n ps = L $ \h g0 -> do
    num <- sum `liftM` (unL n h g0)
    let go g' = do
          i <- uniformR (0,length ps-1) g'
          unL (ps!!i) h g'
    concat `liftM` replicateM num (go g0)

instance Ptuple L where
  pzip = liftL2 zip
  pfst = fmap fst
  psnd = fmap snd

instance Plambda L where
  pz = L $ \(a,_) _ -> return [a]
  ps v = L $ \(_,h) g -> unL v h g
  plam _ k = L $ \h g -> undefined
  papp e1 e2 = L $ \h g -> do
    e1' <- unL e1 h g
    e2' <- unL e2 h g
    return $ concat $ zipWith ($) e1' e2'

llam :: L (a,h) b -> L h (a->[b])
llam k = L $ \h g -> undefined
