{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Data type to duplicate expression.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.Dup where

import Sound.SC3
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.S
import Sound.SC3.Lepton.Pattern.Interpreter.TH

-- | 'Dup' to duplicate pattern expression to two interpreter without
-- fixing type.
data Dup l r a = Dup (l a) (r a)

instance (Show (l a), Show (r a)) => Show (Dup l r a) where
  show (Dup a b) = "Dup (" ++ show a ++ ") (" ++ show b ++ ")"
  
instance (Eq (l a), Eq (r a)) => Eq (Dup l r a) where
  Dup a1 b1 == Dup a2 b2 = (a1 == a2) && (b1 == b2)
  
instance (Functor l, Functor r) => Functor (Dup l r) where
  fmap f (Dup a b) = Dup (fmap f a) (fmap f b)

instance (Num (l a), Num (r a)) => Num (Dup l r a) where
  Dup a1 b1 + Dup a2 b2 = Dup (a1+a2) (b1+b2)
  Dup a1 b1 * Dup a2 b2 = Dup (a1*a2) (b1*b2)
  Dup a1 b1 - Dup a2 b2 = Dup (a1-a2) (b1-b2)
  negate (Dup a b) =  Dup (negate a) (negate b)
  abs (Dup a b) = Dup (abs a) (abs b)
  signum (Dup a b) = Dup (signum a) (signum b)
  fromInteger a = Dup (fromInteger a) (fromInteger a)

duplicate :: Dup l r a -> Dup l r a
duplicate = id

dupl :: Dup l r a -> l a
dupl (Dup x _) = x

dupr :: Dup l r a -> r a
dupr (Dup _ x) = x

dupls :: [Dup l r a] -> [l a]
dupls = map dupl

duprs :: [Dup l r a] -> [r a]
duprs = map dupr

$(idup0 ''Dup 'Dup)
$(idup1 ''Dup 'Dup)
$(idup1p ''Dup 'Dup)
$(idup1ps ''Dup 'Dup 'dupls 'duprs)
$(idup2pps ''Dup 'Dup 'dupls 'duprs)

instance (Pforever a, Pforever b) => Pforever (Dup a b) where
  pforever p = Dup (pforever $ dupl p) (pforever $ dupr p)
  
-- liftD :: (forall p. (p a -> p b)) -> Dup l1 r1 a -> Dup l1 r1 b
-- liftD f (Dup a b) = Dup (f a) (f b)

-- instance (Pempty a, Pempty b) => Pempty (Dup a b) where
--   pempty = Dup pempty pempty

-- instance (Prandom a, Prandom b) => Prandom (Dup a b) where
--   prandom = Dup prandom prandom

-- instance (Pval a, Pval b) => Pval (Dup a b) where
--   pval a = Dup (pval a) (pval a)

-- instance (Prepeat a, Prepeat b) => Prepeat (Dup a b) where
--   prepeat a = Dup (prepeat a) (prepeat a)

-- instance (Plist a, Plist b) => Plist (Dup a b) where
--   plist a = Dup (plist a) (plist a)

-- instance (Pappend a, Pappend b) => Pappend (Dup a b) where
--   pappend (Dup a1 b1) (Dup a2 b2) = Dup (pappend a1 a2) (pappend b1 b2)

-- instance (Pconcat a, Pconcat b) => Pconcat (Dup a b) where
--   pconcat xs = Dup (pconcat (dupls xs)) (pconcat (duprs xs))

-- instance (Pcycle a, Pcycle b) => Pcycle (Dup a b) where
--   pcycle ps = Dup (pcycle (dupls ps)) (pcycle (duprs ps))

-- instance (Preplicate a, Preplicate b) => Preplicate (Dup a b) where
--   preplicate (Dup na nb) (Dup pa pb) =
--     Dup (preplicate na pa) (preplicate nb pb)

-- instance (Prange a, Prange b) => Prange (Dup a b) where
--   prange (Dup la lb) (Dup ha hb) = Dup (prange la ha) (prange lb hb)

-- instance (Pshuffle a, Pshuffle b) => Pshuffle (Dup a b) where
--   pshuffle xs = Dup (pshuffle (dupls xs)) (pshuffle (duprs xs))

