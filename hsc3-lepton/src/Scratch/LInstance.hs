{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

L Instances of classes in PC01
-}
module Scratch.LInstance where

import Control.Applicative
import System.Random (next,randomR)

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Interpreter.R (gens,shiftT,initialT)
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Data.Map as M
import qualified Data.Traversable as T

import Scratch.PC01
import Scratch.L

------------------------------------------------------------------------------
-- For list pattern

instance Pval L where pval x = L $ \_ _ -> [x]

instance Plist L where plist xs = L $ \_ _ -> xs

instance Prepeat L where prepeat x = L $ \_ _ -> repeat x

instance Pempty L where pempty = L $ \_ _ -> []

instance Pprim L where
  pint x = L $ \_ _ -> [x]
  pdouble x = L $ \_ _ -> [x]
  pbool x = L $ \_ _ -> [x]

instance Pappend L where
  pappend a b = L $ \h g -> unL a h g ++ unL b h (snd (next g))

instance Pconcat L where
  pconcat xs = foldr1 pappend xs

instance Prand L where
  prand i xs = L $ \h g0 ->
    let g1 = snd (next g0)
        gs = take (sum $ unL i h g0) (gens g1)
        f x = let (j,_) = randomR (0,length xs-1) x in unL (xs!!j) h x
    in  concatMap f gs

instance Prange L where
  prange a b = L $ \h g0 ->
    let g1 = snd (next g0)
        g2 = snd (next g1)
    in  zipWith3 (\lo hi g' -> fst (randomR (lo,hi) g'))
        (unL a h g0) (unL b h g1) (gens g2)

instance Prng L where
  pirange = prange
  pdrange = prange

instance Preplicate L where
  preplicate n p = L $ \h g ->
    let p' = concatMap (`replicate` p) (unL n h g)
    in  concat $ zipWith (\q g' -> unL q h g') p' (gens g)

instance Pseq L where
  pseq n = preplicate n . pconcat

instance Pforever L where
  pforever p = L $ \h g -> concatMap (unL p h) (gens g)

instance Pcycle L where
  pcycle ps = case ps of
    [] -> pempty
    _  -> pforever $ pconcat ps

instance Ptuple L where
  pzip a b = L $ \h g -> zip (unL a h g) (unL b h (snd $ next g))
  pfst = fmap fst
  psnd = fmap snd

instance Plc L where
  pz = L $ \(a,_) _ -> [a]
  ps v = L $ \(_,h) g -> unL v h g
  lam k = L $ \h g -> repeat (\x -> unL k (x,h) g)
  app e1 e2 = L $ \h g -> concat $ zipWith ($) (unL e1 h g) (unL e2 h g)

instance Psnew L where
  psnew def nid aa tid ms = ToOSC sn <$> ms' where
    sn = Snew def nid aa tid
    ms' = L $ \h g ->
      tail $ shiftT 0 $
      unL (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) h g

instance Pnum L where
  padd = (+)
  pmul = (*)
  psub = (-)
  pnegate = negate
  pabs = abs
  psignum = signum
  pfromInteger = fromInteger

instance Punary L where
  pmidiCPS = midiCPS
