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
  ,foldr1, Monad(..), (=<<), map, mapM_, zip, sequence_)
import System.Random (Random(..),next,randomR)

import Sound.SC3
import Data.List.Stream
import Control.Monad.Stream

import System.Random.Mersenne.Pure64
import System.Random.Shuffle

-- import Sound.SC3.Lepton hiding (shuffle')
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Play
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Control.Parallel as CP
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Traversable as T

-- import Scratch.PC02

------------------------------------------------------------------------------
-- Exposed functions

newtype PL h a = PL {unPL :: h -> PureMT -> [a]}

toPL :: PL () a -> PL () a
toPL = id

runPL :: PL () a -> PureMT -> [a]
runPL p g = unPL p () g

runPLIO :: PL () a -> IO [a]
runPLIO p = unPL p () <$> newPureMT

mapPLIO_ :: (a -> IO b) -> PL () a -> IO ()
mapPLIO_ k p = mapM_ k =<< runPLIO p

foldPLIO :: (b -> a -> IO b) -> b -> PL () a -> IO b
foldPLIO k z p = foldM k z =<< runPLIO p

foldPLIO_ :: (b -> a -> IO b) -> b -> PL () a -> IO ()
foldPLIO_ k z p = foldPLIO k z p >> return ()

------------------------------------------------------------------------------
-- Helper functions

constPL2 :: a -> PL h a
constPL2 x = PL $ \_ _ -> [x]
{-# INLINE constPL2 #-}

rangePL :: Random a => PL h a -> PL h a -> PL h a
rangePL a b = PL $ \h g0 ->
  let g1 = snd . next $ g0
      g2 = snd . next $ g1
  in  zipWith3 (\lo hi g' -> fst $ randomR (lo,hi) g')
        (unPL a h g0) (unPL b h g1) (gens g2)
{-# INLINE rangePL #-}
{-# SPECIALIZE rangePL :: PL h Int -> PL h Int -> PL h Int #-}
{-# SPECIALIZE rangePL :: PL h Double -> PL h Double -> PL h Double #-}

mergePL :: PL h (ToOSC Double) -> PL h (ToOSC Double) -> PL h (ToOSC Double)
mergePL p1 p2 = PL $ \h g ->
    let p1' = unPL p1 h g
        p2' = unPL p2 h g
    in  p1' `CP.par` (p2' `CP.pseq` mergePL' 0 (0,0) p1' p2')
{-# INLINE mergePL #-}

mergePL' :: Double -> (Double,Double) ->
          [ToOSC Double] -> [ToOSC Double] -> [ToOSC Double]
mergePL' _ _ [] [] = []
mergePL' t (ta,_) (a:as) [] = tadjust "dur" (const $ getDur a + ta - t) a : as
mergePL' t (_,tb) [] (b:bs) = tadjust "dur" (const $ getDur b + tb - t) b : bs
mergePL' t (ta,tb) (a:as) (b:bs)
  | ua <= ub  = tadjust "dur" (const $ ua-t) a : mergePL' ua (ua,tb) as (b:bs)
  | otherwise = tadjust "dur" (const $ ub-t) b : mergePL' ub (ta,ub) (a:as) bs
  where
    ua = ta + getDur a
    ub = tb + getDur b

gens :: PureMT -> [PureMT]
gens = iterate (snd . next)
{-# INLINE gens #-}

shiftT :: Num a => a -> [M.Map String a] -> [M.Map String a]
shiftT t ms = case ms of
  (m1:m2:r) ->
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  (m1'):shiftT t' (m2:r)
  [m1] ->
    -- XXX:
    -- Sending dummy silent event at the end of list
    -- to receive /n_go response from server.
    --
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  [m1',M.fromList [("freq",0),("dur",t')]]
  _ -> []
{-# INLINE shiftT #-}

initialT :: Num a => M.Map String a
initialT = M.singleton "dur" 0
{-# INLINE initialT #-}
{-# SPECIALIZE initialT :: M.Map String Double #-}

------------------------------------------------------------------------------
-- Base classes

instance Show (PL h a) where
  show _ = "PL"

instance Functor (PL h) where
  fmap f (PL l) = PL $ \h g -> map f (l h g)

instance Applicative (PL h) where
  pure x = PL $ \_ _ -> repeat x
  PL f <*> PL a = PL $ \h g ->
    let g' = snd . next $ g
    in  zipWith ($) (f h g) (a h g')

instance Monad (PL h) where
  return x = PL $ \_ _ -> [x]
  a >>= k = PL $ \h g ->
    let g' = snd . next $ g
    in  concatMap (\x -> unPL (k x) h g) (unPL a h g)

instance Eq (PL h a) where
  _ == _ = True

instance Ord (PL h a) where
  compare _ _ = EQ

instance Playable (PL ()) where
  playIO = foldPLIO_

------------------------------------------------------------------------------
-- Numeric classes

instance Num a => Num (PL h a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger x = PL $ \_ _ -> [fromInteger x]

instance Fractional a => Fractional (PL h a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational x = PL $ \_ _ -> [fromRational x]

instance Floating a => Floating (PL h a) where
  pi = PL $ \_ _ -> [pi]
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

instance UnaryOp a => UnaryOp (PL h a) where
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

instance Pint PL where
  pint = constPL2
  (+!) = (+)
  (*!) = (*)
  (-!) = (-)
  pinegate = negate
  piabs = abs
  pisignum = signum
  pirange = rangePL

instance Pdouble PL where
  pdouble = constPL2
  (+@) = (+)
  (*@) = (*)
  (-@) = (-)
  pdnegate = negate
  pdabs = abs
  pdsignum = signum
  pdrange = rangePL
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

instance Pappend PL where
  pappend a b = PL $ \h g -> unPL a h g ++ unPL b h (snd (next g))

instance Pconcat PL where
  pconcat = foldr1 pappend

instance Preplicate PL where
  preplicate n p = PL $ \h g ->
    let p' = concatMap (`replicate` p) (unPL n h g)
    in  concat $ zipWith (\q g' -> unPL q h g') p' (gens g)

instance Pseq PL where
  pseq n = preplicate n . foldr1 pappend

instance Pforever PL where
  pforever p = PL $ \h g -> concatMap (unPL p h) (gens g)

instance Pcycle PL where
  pcycle ps = case ps of
    [] -> PL $ \_ _ -> []
    _  -> pforever $ pseq 1 ps

------------------------------------------------------------------------------
-- Random patterns

instance Prand PL where
  prand i xs = PL $ \h g0 ->
    let g1 = snd . next $ g0
        gs = take (sum $ unPL i h g0) (gens g1)
        f x = let (j,_) = randomR (0,length xs-1) x in unPL (xs!!j) h x
    in  concatMap f gs

instance Pshuffle PL where
  pshuffle ps = PL $ \h g ->
    let g' = snd (next g)
        f x y = unPL x h y
    in  concat $ zipWith f (shuffle' ps (length ps) g) (gens g')

------------------------------------------------------------------------------
-- Combination patterns

instance Ptuple PL where
  pzip a b = PL $ \h g -> zip (unPL a h g) (unPL b h (snd $ next g))
  pfst = fmap fst
  psnd = fmap snd

instance Pfsm PL where
  pfsm is cs = PL $ \h g0 ->
    let cm = I.fromList (zip [0..] cs)
        go idx g = case I.lookup idx cm of
          Nothing     -> []
          Just (p,[]) -> unPL p h g
          Just (p,js) ->
            let g'   = snd (next g)
                idx' = head $ shuffle' js (length js) g
            in  unPL p h g ++ go idx' g'
    in  go (head $ shuffle' is (length is) g0) g0

instance Plambda PL where
  pz = PL $ \(a,_) _ -> [a]
  ps v = PL $ \(_,h) g -> unPL v h g
  plam _ k = PL $ \h g -> repeat (\x -> unPL k (x,h) g)
  papp e1 e2 = PL $ \h g -> concat $ zipWith ($) (unPL e1 h g) (unPL e2 h g)

------------------------------------------------------------------------------
-- OSC patterns

instance Psnew PL where
  psnew = lsnew

lsnew :: String -> Maybe Int -> AddAction -> Int -> [(String,PL h Double)]
      -> PL h (ToOSC Double)
lsnew def nid aa tid ms = PL $ \h g ->
  let o = Snew def nid aa tid
  in  ToOSC o <$>
      (tail $ shiftT 0 $ initialT : unPL (T.sequenceA $ M.fromList ms) h g)
{-# INLINE lsnew #-}

instance Pnset PL where
  pnset = lnset

lnset nid ms = PL $ \h g ->
  let o = Nset nid
  in  ToOSC o <$>
      (tail $ shiftT 0 $ initialT:unPL (T.sequenceA $ M.fromList ms) h g)
{-# INLINE lnset #-}

instance Pmerge PL where
  pmerge = mergePL

instance Ppar PL where
  ppar = foldr1 pmerge

instance Ptake PL where
  ptakeT t p = PL $ \h g ->
    let ps = unPL p h g
        t' = head $ unPL t h g
        f cur rs = case rs of
          [] -> []
          (q:qs) | cur <= t' -> q : f (cur+getDur q) qs
                 | otherwise -> []
    in  f 0 ps

instance Pdrop PL where
  pdropT t p = PL $ \h g ->
    let ps = unPL p h g
        t' = head $ unPL t h g
        f cur rs = case rs of
          [] -> []
          (q:qs) | t' < cur  -> q : f (cur+getDur q) qs
                 | otherwise -> []
    in f 0 ps
