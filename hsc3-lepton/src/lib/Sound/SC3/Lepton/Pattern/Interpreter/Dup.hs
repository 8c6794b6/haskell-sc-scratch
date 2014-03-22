{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Data type to duplicate expression.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.Dup
  ( Dup(..), duplicate, dupl, dupr
  ) where

import Control.Arrow
import Sound.SC3.Lepton.Pattern.Expression

data Dup l r h a = Dup (l h a) (r h a)

duplicate :: Dup l r h a -> Dup l r h a
duplicate = id

dupl :: Dup l r h a -> l h a
dupl (Dup l _) = l

dupr :: Dup l r h a -> r h a
dupr (Dup _ r) = r

dupls :: [Dup l r h a] -> [l h a]
dupls = map dupl

duprs :: [Dup l r h a] -> [r h a]
duprs = map dupr

instance (Show (l h a), Show (r h a)) => Show (Dup l r h a) where
  show (Dup a b) = "Dup (" ++ show a ++ ") (" ++ show b ++ ")"

instance (Eq (l h a), Eq (r h a)) => Eq (Dup l r h a) where
  Dup a1 b1 == Dup a2 b2 = a1 == a2 && b1 == b2

dupI0 :: (Pint r, Pint l) => Int -> Dup r l h Int
dupI0 x = Dup (pint x) (pint x)

dupI1 ::
  (Pint r, Pint l)
  => (forall g. Pint g => g h a -> g h b) -> Dup r l h a -> Dup r l h b
dupI1 f (Dup a b) = Dup (f a) (f b)

dupI2 ::
  (Pint r, Pint l)
  => (forall g. Pint g => g h a -> g h b -> g h c)
  -> Dup r l h a -> Dup r l h b -> Dup r l h c
dupI2 f (Dup al bl) (Dup ar br) = Dup (f al ar) (f bl br)

dupD0 :: (Pdouble r, Pdouble l) => Double -> Dup r l h Double
dupD0 x = Dup (pdouble x) (pdouble x)

dupD1 ::
  (Pdouble r, Pdouble l)
  => (forall g. Pdouble g => g h a -> g h b) -> Dup r l h a -> Dup r l h b
dupD1 f (Dup a b) = Dup (f a) (f b)

dupD2 ::
  (Pdouble r, Pdouble l)
  => (forall g. Pdouble g => g h a -> g h b -> g h c)
  -> Dup r l h a -> Dup r l h b -> Dup r l h c
dupD2 f (Dup al bl) (Dup ar br) = Dup (f al ar) (f bl br)

ppiD :: (Pdouble r, Pdouble l) =>  Dup r l h Double
ppiD = Dup ppi ppi

-- XXX:
-- Dup has different kind than other interpreters. Need a custom helper.

$(derivePintDup ''Dup 'dupI0 'dupI1 'dupI2)
$(derivePdoubleDup ''Dup 'dupD0 'ppiD 'dupD1 'dupD2)

instance (Pappend l, Pappend r) => Pappend (Dup l r) where
  pappend (Dup al ar) (Dup bl br) = Dup (pappend al bl) (pappend ar br)

instance (Pconcat l, Pconcat r) => Pconcat (Dup l r) where
  pconcat ds = Dup (pconcat (dupls ds)) (pconcat (duprs ds))

instance (Preplicate l, Preplicate r) => Preplicate (Dup l r) where
  preplicate (Dup al ar) (Dup bl br) = Dup (preplicate al bl) (preplicate ar br)

instance (Pseq l, Pseq r) => Pseq (Dup l r) where
  pseq (Dup al ar) ds = Dup (pseq al (dupls ds)) (pseq ar (duprs ds))

instance (Pforever l, Pforever r) => Pforever (Dup l r) where
  pforever d = Dup (pforever (dupl d)) (pforever (dupr d))

instance (Pcycle l, Pcycle r) => Pcycle (Dup l r) where
  pcycle ds = Dup (pcycle (dupls ds)) (pcycle (duprs ds))

instance (Prand l, Prand r) => Prand (Dup l r) where
  prand d ds = Dup (prand (dupl d) (dupls ds)) (prand (dupr d) (duprs ds))

instance (Pshuffle l, Pshuffle r) => Pshuffle (Dup l r) where
  pshuffle ds = Dup (pshuffle (dupls ds)) (pshuffle (duprs ds))

instance (Ptuple l, Ptuple r) => Ptuple (Dup l r) where
  pzip a b = Dup (pzip (dupl a) (dupl b)) (pzip (dupr a) (dupr b))
  pfst a = Dup (pfst (dupl a)) (pfst (dupr a))
  psnd a = Dup (psnd (dupl a)) (psnd (dupr a))

instance (Pfsm l, Pfsm r) => Pfsm (Dup l r) where
  pfsm is dps = Dup (pfsm is ls) (pfsm is rs) where
    ls = map (first dupl) dps
    rs = map (first dupr) dps

instance (Plambda l, Plambda r) => Plambda (Dup l r) where
  pz = Dup pz pz
  ps (Dup a b) = Dup (ps a) (ps b)
  plam t (Dup a b) = Dup (plam t a) (plam t b)
  papp (Dup a1 b1) (Dup a2 b2) = Dup (papp a1 a2) (papp b1 b2)

instance (Psnew l, Psnew r) => Psnew (Dup l r) where
  psnew def nid aa tid dps =
    Dup (psnew def nid aa tid ls) (psnew def nid aa tid rs) where
      ls = map (second dupl) dps
      rs = map (second dupr) dps

instance (Pnset l, Pnset r) => Pnset (Dup l r)where
  pnset nid dps = Dup (pnset nid ls) (pnset nid rs) where
    ls = map (second dupl) dps
    rs = map (second dupr) dps

instance (Pmerge l, Pmerge r) => Pmerge (Dup l r) where
  pmerge a b = Dup (pmerge (dupl a) (dupl b)) (pmerge (dupr a) (dupr b))

instance (Ppar l, Ppar r) => Ppar (Dup l r) where
  ppar ds = Dup (ppar (dupls ds)) (ppar (duprs ds))

instance (Ptake l, Ptake r) => Ptake (Dup l r) where
  ptakeT n d = Dup (ptakeT (dupl n) (dupl d)) (ptakeT (dupr n) (dupr d))

instance (Pdrop l, Pdrop r) => Pdrop (Dup l r) where
  pdropT n d = Dup (pdropT (dupl n) (dupl d)) (pdropT (dupr n) (dupr d))
