{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

L Instances of classes in PC02
-}
module Scratch.LInstance2 where

import Control.Applicative
import System.Random (Random,next,randomR)
import System.Random.Shuffle

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Interpreter.R (gens,shiftT,initialT)
import Sound.SC3.Lepton.Pattern.Expression (Mergable(..))
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Control.Parallel as CP
import qualified Data.Map as M
import qualified Data.Traversable as T

import Scratch.PC02
import Scratch.L

------------------------------------------------------------------------------
-- Helper functions

constL2 :: a -> L h a
constL2 x = L $ \_ _ -> [x]

rangeL :: Random a => L h a -> L h a -> L h a
rangeL a b = L $ \h g0 ->
  let g1 = snd (next g0)
      g2 = snd (next g1)
  in  zipWith3 (\lo hi g' -> fst (randomR (lo,hi) g'))
      (unL a h g0) (unL b h g1) (gens g2)

mergeL p1 p2 = L $ \h g ->
    let p1' = unL p1 h g
        p2' = unL p2 h g
    in  p1' `CP.par` (p2' `CP.pseq` merge p1' p2')

------------------------------------------------------------------------------
-- Prim patterns

instance Pint L where
  pint = constL2
  (+!) = (+)
  (*!) = (*)
  (-!) = (-)
  pinegate = negate
  piabs = abs
  pisignum = signum
  pirange = rangeL

instance Pdouble L where
  pdouble = constL2
  (+@) = (+)
  (*@) = (*)
  (-@) = (-)
  pdnegate = negate
  pdabs = abs
  pdsignum = signum
  pdrange = rangeL
  (/@) = (/)
  precip = recip
  ppi = pi
  pexp = exp
  psqrt = sqrt
  plog = log
  (**@) = (**)
  plogBase = logBase
  psin = sin
  ptan = tan
  pcos = cos
  pasin = asin
  patan = atan
  pacos = acos
  psinh = sinh
  ptanh = tanh
  pcosh = cosh
  pasinh = asinh
  patanh = atanh
  pacosh = acosh
  pampDb = ampDb
  pasFloat = asFloat
  pasInt = asInt
  pbitNot = bitNot
  pcpsMIDI = cpsMIDI
  pcpsOct = cpsOct
  pcubed = cubed
  pdbAmp = dbAmp
  pdistort = distort
  pfrac = frac
  pisNil = isNil
  plog10 = log10
  plog2 = log2
  pmidiCPS = midiCPS
  pmidiRatio = midiRatio
  pnotE = notE
  pnotNil = notNil
  poctCPS = octCPS
  pramp_ = ramp_
  pratioMIDI = ratioMIDI
  psoftClip = softClip
  psquared = squared

------------------------------------------------------------------------------
-- List pattern

instance Pappend L where
  pappend a b = L $ \h g -> unL a h g ++ unL b h (snd (next g))

instance Pconcat L where
  pconcat xs = foldr1 pappend xs

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
    [] -> L $ \_ _ -> []
    _  -> pforever $ pconcat ps

------------------------------------------------------------------------------
-- Random patterns

instance Prand L where
  prand i xs = L $ \h g0 ->
    let g1 = snd (next g0)
        gs = take (sum $ unL i h g0) (gens g1)
        f x = let (j,_) = randomR (0,length xs-1) x in unL (xs!!j) h x
    in  concatMap f gs

instance Pshuffle L where
  pshuffle ps = L $ \h g ->
    let g' = snd (next g)
        f x y = unL x h y
    in  concat $ zipWith f (shuffle' ps (length ps) g) (gens g')

------------------------------------------------------------------------------
-- Combination patterns

instance Ptuple L where
  pzip a b = L $ \h g -> zip (unL a h g) (unL b h (snd $ next g))
  pfst = fmap fst
  psnd = fmap snd

instance Plambda L where
  pz = L $ \(a,_) _ -> [a]
  ps v = L $ \(_,h) g -> unL v h g
  plam _ k = L $ \h g -> repeat (\x -> unL k (x,h) g)
  papp e1 e2 = L $ \h g -> concat $ zipWith ($) (unL e1 h g) (unL e2 h g)

------------------------------------------------------------------------------
-- OSC patterns

instance Psnew L where
  psnew def nid aa tid ms = ToOSC sn <$> ms' where
    sn = Snew def nid aa tid
    ms' = L $ \h g ->
      tail $ shiftT 0 $
      -- XXX: Check whether this works or not.
      initialT : unL (T.sequenceA $ M.fromList ms) h g

instance Pmerge L where
  pmerge = mergeL

instance Ppar L where
  ppar = foldr1 pmerge
