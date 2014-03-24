{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable

'L' interpreter, for making list from pattern.

-}
module Scratch.L
  ( L(..)
  , toL
  , foldLIO
  , foldLIO_
  , mapLIO_
  , runLIO
  ) where

import Control.Applicative
import Control.Monad
import System.Random

import Sound.SC3
import System.Random.Mersenne.Pure64
import System.Random.Shuffle

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Control.Parallel as CP
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Traversable as T

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
-- Pattern classes

instance Pval (L h) where pval a = L $ const2 [a]

instance Plist (L h) where plist as = L $ const2 as

instance Prepeat (L h) where prepeat a = L $ const2 (repeat a)

instance Pempty (L h) where pempty = L $ const2 []

instance Prandom (L h) where prandom = L $ \_ g -> [fst $ random g]

instance Pappend (L h) where
  pappend a b = L $ \h g -> let g' = snd (next g) in unL a h g ++ unL b h g'

instance Pconcat (L h) where pconcat = foldr1 pappend

instance Preplicate (L h) where
  preplicate n p = L $ \h g ->
    let p' = concatMap (`replicate` p) (unL n h g)
    in  concat $ zipWith (\q g' -> unL q h g') p' (gens g)

instance Pseq (L h) where pseq n = preplicate n . pconcat

instance Prand (L h) where
  prand n p = L $ \h g ->
    let g' = snd . next $ g
        gs = take (sum $ unL n h g) (gens g')
    in  concatMap (\x -> let (j,_) = randomR (0,length p - 1) x
                         in  unL (p!!j) h x) gs

instance Pchoose (L h) where pchoose = prand

instance Prange (L h) where
  prange a b = L $ \h g0 ->
    let g1 = snd (next g0)
        g2 = snd (next g1)
    in  zipWith3 (\lo hi g' -> fst (randomR (lo,hi) g'))
        (unL a h g0) (unL b h g1) (gens g2)

instance Pforever (L h) where
  pforever p = L $ \h g -> concatMap (unL p h) (gens g)

instance Pcycle (L h) where
  pcycle ps = case ps of
    [] -> pempty
    _  -> pforever $ pconcat ps

instance Pshuffle (L h) where
  pshuffle ps = L $ \h g ->
    let g' = snd (next g)
        f x y = unL x h y
    in  concat $ zipWith f (shuffle' ps (length ps) g) (gens g')

instance Pfsm (L h) where
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

instance Psnew (L h) where
  psnew def nid aa tid ms = ToOSC sn <$> ms' where
    sn = Snew def nid aa tid
    ms' = L $ \h g ->
      tail $ shiftT 0 $
      unL (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) h g

instance Pnset (L h) where
  pnset nid ms = ToOSC o <$> ms' where
    o = Nset nid
    ms' = L $ \h g ->
      tail $ shiftT 0 $
      unL (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) h g

instance (Ord a, Num a) => Mergable (L h (ToOSC a)) where
  merge p1 p2 = L $ \h g ->
    let p1' = unL p1 h g
        p2' = unL p2 h g
    in  p1' `CP.par` (p2' `CP.pseq` merge p1' p2')

instance Pmerge (L h) where pmerge = merge

instance Ppar (L h) where ppar = foldr1 pmerge

instance Plc L where
  pz   = L $ \(a,_) _ -> [a]
  ps v = L $ \(_,h) g -> unL v h g
  -- lam :: L (a,h) b -> L h (a -> p h b)
  lam k = L $ \h g -> repeat (\x -> L $ \_ _ -> unL k (x,h) g)
  app e1 e2 = L $ \h g ->
    unL (pconcat $ zipWith ($) (unL e1 h g) (unL e2 h g)) h g

------------------------------------------------------------------------------
-- Helper functions

const2 :: a -> b -> c -> a
const2 a _ _ = a
