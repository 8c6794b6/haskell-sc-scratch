{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

L Instances of classes in PC02
-}
module Scratch.L2 where

import Control.Applicative
import Prelude hiding
  ((++), (!!), length, concat, concatMap, reverse, head, take, tail
  ,replicate,zipWith, zipWith3, cycle, repeat, iterate, sum
  ,foldr1, Monad(..), (=<<), map, mapM_, zip)
import System.Random (Random(..),next,randomR)

import Sound.SC3
import Data.List.Stream
import Control.Monad.Stream

import System.Random.Mersenne.Pure64
import System.Random.Shuffle

import Sound.SC3.Lepton.Pattern.Interpreter.R (shiftT,initialT)
import Sound.SC3.Lepton.Pattern.Expression (Mergable(..))
import Sound.SC3.Lepton.Pattern.Play
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Control.Parallel as CP
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Traversable as T

import Scratch.PC02

------------------------------------------------------------------------------
-- Exposed functions

newtype L h a = L {unL :: h -> PureMT -> [a]}

toL :: L () a -> L () a
toL = id

runL :: L () a -> PureMT -> [a]
runL p g = unL p () g

runLIO :: L () a -> IO [a]
runLIO p = unL p () <$> newPureMT

mapLIO_ :: (a -> IO b) -> L () a -> IO ()
mapLIO_ k p = mapM_ k =<< runLIO p

foldLIO :: (b -> a -> IO b) -> b -> L () a -> IO b
foldLIO k z p = foldM k z =<< runLIO p

foldLIO_ :: (b -> a -> IO b) -> b -> L () a -> IO ()
foldLIO_ k z p = foldLIO k z p >> return ()

------------------------------------------------------------------------------
-- Helper functions

constL2 :: a -> L h a
constL2 x = L $ \_ _ -> [x]
{-# INLINE constL2 #-}

rangeL :: Random a => L h a -> L h a -> L h a
rangeL a b = L $ \h g0 ->
  let g1 = snd . next $ g0
      g2 = snd . next $ g1
  in  zipWith3 (\lo hi g' -> fst $ randomR (lo,hi) g')
        (unL a h g0) (unL b h g1) (gens g2)
{-# INLINE rangeL #-}
{-# SPECIALIZE rangeL :: L h Int -> L h Int -> L h Int #-}
{-# SPECIALIZE rangeL :: L h Double -> L h Double -> L h Double #-}

mergeL :: L h (ToOSC Double) -> L h (ToOSC Double) -> L h (ToOSC Double)
mergeL p1 p2 = L $ \h g ->
    let p1' = unL p1 h g
        p2' = unL p2 h g
    in  p1' `CP.par` (p2' `CP.pseq` merge p1' p2')
{-# INLINE mergeL #-}

gens :: PureMT -> [PureMT]
gens = iterate (snd . next)
{-# INLINE gens #-}

------------------------------------------------------------------------------
-- Base classes

instance Show (L h a) where
  show _ = "L"

instance Functor (L h) where
  fmap f (L l) = L $ \h g -> map f (l h g)

instance Applicative (L h) where
  pure x = L $ \_ _ -> repeat x
  L f <*> L a = L $ \h g ->
    let g' = snd . next $ g
    in  zipWith ($) (f h g) (a h g')

instance Monad (L h) where
  return x = L $ \_ _ -> [x]
  a >>= k = L $ \h g ->
    let g' = snd . next $ g
    in  concatMap (\x -> unL (k x) h g) (unL a h g)

instance Eq (L h a) where
  _ == _ = True

instance Ord (L h a) where
  compare _ _ = EQ

instance Playable (L ()) where
  playIO = foldLIO_

------------------------------------------------------------------------------
-- Numeric classes

instance Num a => Num (L h a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger x = L $ \_ _ -> [fromInteger x]

instance Fractional a => Fractional (L h a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational x = L $ \_ _ -> [fromRational x]

instance Floating a => Floating (L h a) where
  pi = L $ \_ _ -> [pi]
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance UnaryOp a => UnaryOp (L h a) where
  ampDb = fmap ampDb
  asFloat = fmap asFloat
  asInt = fmap asInt
  bitNot = fmap bitNot
  cpsMIDI = fmap cpsMIDI
  cpsOct = fmap cpsOct
  cubed = fmap cubed
  dbAmp = fmap dbAmp
  distort = fmap distort
  frac = fmap frac
  isNil = fmap isNil
  log10 = fmap log10
  log2 = fmap log2
  midiCPS = fmap midiCPS
  midiRatio = fmap midiRatio
  notE = fmap notE
  notNil = fmap notNil
  octCPS = fmap octCPS
  ramp_ = fmap ramp_
  ratioMIDI = fmap ratioMIDI
  softClip = fmap softClip
  squared = fmap squared

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
  pconcat = foldr1 pappend

instance Preplicate L where
  preplicate n p = L $ \h g ->
    let p' = concatMap (`replicate` p) (unL n h g)
    in  concat $ zipWith (\q g' -> unL q h g') p' (gens g)

instance Pseq L where
  pseq n = preplicate n . foldr1 pappend

instance Pforever L where
  pforever p = L $ \h g -> concatMap (unL p h) (gens g)

instance Pcycle L where
  pcycle ps = case ps of
    [] -> L $ \_ _ -> []
    _  -> pforever $ pseq 1 ps

------------------------------------------------------------------------------
-- Random patterns

instance Prand L where
  prand i xs = L $ \h g0 ->
    let g1 = snd . next $ g0
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

instance Pfsm L where
  pfsm is cs = L $ \h g0 ->
    let cm = I.fromList (zip [0..] cs)
        go idx g = case I.lookup idx cm of
          Nothing     -> []
          Just (p,[]) -> unL p h g
          Just (p,js) ->
            let g'   = snd (next g)
                idx' = head $ shuffle' js (length js) g
            in  unL p h g ++ go idx' g'
    in  go (head $ shuffle' is (length is) g0) g0

instance Plambda L where
  pz = L $ \(a,_) _ -> [a]
  ps v = L $ \(_,h) g -> unL v h g
  plam _ k = L $ \h g -> repeat (\x -> unL k (x,h) g)
  papp e1 e2 = L $ \h g -> concat $ zipWith ($) (unL e1 h g) (unL e2 h g)

------------------------------------------------------------------------------
-- OSC patterns

instance Psnew L where
  psnew = lsnew

lsnew :: String -> Maybe Int -> AddAction -> Int -> [(String,L h Double)]
      -> L h (ToOSC Double)
lsnew def nid aa tid ms = L $ \h g ->
  let o = Snew def nid aa tid
  in  ToOSC o <$>
      (tail $ shiftT 0 $ initialT : unL (T.sequenceA $ M.fromList ms) h g)
{-# INLINE lsnew #-}

instance Pnset L where
  pnset = lnset

lnset nid ms = L $ \h g ->
  let o = Nset nid
  in  ToOSC o <$>
      (tail $ shiftT 0 $ initialT:unL (T.sequenceA $ M.fromList ms) h g)
{-# INLINE lnset #-}

instance Pmerge L where
  pmerge = mergeL

instance Ppar L where
  ppar = foldr1 pmerge

instance Ptake L where
  ptakeT t p = L $ \h g ->
    let ps = unL p h g
        t' = head $ unL t h g
        f cur rs = case rs of
          [] -> []
          (q:qs) | cur <= t' -> q : f (cur+getDur q) qs
                 | otherwise -> []
    in  f 0 ps

instance Pdrop L where
  pdropT t p = L $ \h g ->
    let ps = unL p h g
        t' = head $ unL t h g
        f cur rs = case rs of
          [] -> []
          (q:qs) | t' < cur  -> q : f (cur+getDur q) qs
                 | otherwise -> []
    in f 0 ps