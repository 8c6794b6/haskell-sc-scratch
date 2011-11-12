{-# LANGUAGE Arrows, DoRec #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Scratch written while reading:

* Audio Processing and Sound Synthesis in Haskell

-}
module APSS.Scratch02 where

import Control.Arrow
import Control.Category
import Data.Word (Word8)
import Data.Array.Unboxed
import Prelude hiding ((.), id)
import System.Environment (getArgs)
import System.Random

import Sound.Iteratee
import Sound.Iteratee.Codecs.Wave

import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
import qualified Data.Vector.Storable as V

type Sample = Double

newtype SF p a b = SF {unSF :: [a] -> [b]}

mkStream :: SF p () Sample -> [Sample]
mkStream sf = unSF sf (repeat ())

instance Category (SF p) where
  SF f . SF g = SF (f . g)
  id = SF id

instance Arrow (SF p) where
  arr f = SF (map f)
  first (SF f) = SF g where
    g l = let (x, y) = unzip l in zip (f x) y

instance ArrowLoop (SF p) where
  loop (SF f) = SF $ \x -> let (y,z) = unzip (f (zip x (stream z))) in y

stream :: [a] -> [a]
stream ~(x:xs) = x : stream xs

class Num r => Clock p r | p -> r where
  rate :: p -> r

data AudRate
data CtrRate

instance Clock AudRate Int where
  rate _ = 44100

instance Clock CtrRate Int where
  rate _ = 4410

type AR = SF AudRate () Double
type CR = SF CtrRate () Double

__ :: a
__ = undefined

upSample ::
  forall p1 p2 a c. (Clock p1 Int, Clock p2 Int) =>
  SF p1 a c -> SF p2 a c
upSample (SF f) =
  let inRate = rate (__ :: p1)
      outRate = rate (__ :: p2)
      ratio = outRate `div` inRate
  in  SF (\inp -> concatMap (replicate ratio) (f inp))

delay :: a -> SF p a a
delay i = SF (i:)

sine :: forall a p. Clock p Int => Double -> SF p a Sample
sine freq =
  let omh = 2 * pi * freq / (fromIntegral sr)
      d = sin omh
      c = 2 * cos omh
      sr = rate (__ :: p) :: Int
      sf = proc _ -> do
        rec let r = c * d2 - d1
            d2 <- delay d -< r
            d1 <- delay 0 -< d2
        returnA -< r
  in  sf

type Frequency = Double

-- | straight forward implementation of sine signal.
-- Using `sin` from GHC.Float.
sinF :: forall p a. Clock p Int => Frequency -> SF p a Sample
sinF freq = SF (\_ -> cycle $ take n $ map sin [0,d..]) where
  d = 2 * pi * freq / sr
  n = truncate (sr / freq)
  sr = fromIntegral $ rate (__ :: p)

-- | Fast sine, Goertzel's algorithm.
-- Efficient but unstable on frequency change.
fastSine :: Frequency -> Int -> [Sample]
fastSine freq sr =
  let omh = 2 * pi * freq / fromIntegral sr
      i = sin omh
      c = 2 * cos omh
      sine = 0:i:zipWith (\b a -> c * a - b) sine (tail sine)
  in  sine

-- | Using fastSine.
sinA :: forall p a. Clock p Int => Frequency -> SF p a Sample
sinA freq = SF (\_ -> fastSine freq sr) where sr = rate (__ :: p)

audioRate :: Int
audioRate = rate (__ :: AudRate)

integral :: forall p. Clock p Int => SF p Sample Sample
integral =
  loop (arr (\(x,i) -> i + h * x) >>> delay 0 >>> arr (\x -> (x, x)))
  where
    h = recip (fromIntegral sr)
    sr = rate (__ :: p) :: Int

------------------------------------------------------------------------------
-- * Variable frequency algorithms

oscSine :: Clock p Int => Frequency -> SF p Sample Sample
oscSine f0 = proc cv -> do
  let f = f0 * (2 ** cv)
  phi <- integral -< 2 * pi * f
  returnA -< sin phi

-- The paper claims that oscSine' runs faster than oscSine.
-- Do benchmark, recent GHC might have some performance improvement in
-- desugaring arrow syntax.
--
oscSine' :: Clock p Int => Frequency -> SF p Sample Sample
oscSine' f0 =
  arr (2**) >>>
  arr (* (2 * pi * f0)) >>>
  integral >>>
  arr sin

-- XXX: Using (!) instead of unsafeAt.
--
oscA' :: forall p. Clock p Int => SF p Sample Sample
oscA' =
  let sr = rate (__ :: p)
      sin1 = fastSine 1 sr
      array = listArray (0, sr-1) sin1 :: UArray Int Sample
      idx = loop (arr (\(d,i) -> (i + d) `mod` sr) >>> delay 0 >>> arr dup)
      dup x = (x, x)
  in  arr truncate >>> idx >>> arr (array !)

-- Manually writing the contents of loop in oscA'.
-- The paper claims that oscA'' is faster than oscA'. Do bench mark.
--
oscA'' :: forall p. Clock p Int => SF p Sample Sample
oscA'' =
  let sr = rate (__ :: p)
      sin1 = fastSine 1 sr
      array = listArray (0, sr-1) sin1 :: UArray Int Sample
      idx = SF $ \as -> let bs = 0:zipWith (\d i -> (i+d) `mod` sr) bs as in bs
  in  arr truncate >>> idx >>> arr (array !)

integral' :: forall p. Clock p Int => SF p Sample Sample
integral' = SF $ \as -> let bs = 0:zipWith (\x i -> i + dt * x) bs as in bs
  where
    dt = 1 / fromIntegral sr
    sr = rate (__ :: p)

oscSine'' :: Clock p Int => Frequency -> SF p Sample Sample
oscSine'' f0 = arr (2**) >>> arr (* (2 * pi * f0)) >>> integral' >>> arr sin

sinF' :: forall p. Clock p Int => SF p Sample Sample
sinF' = SF $ \freqs ->
  let f = 0:zipWith (\f0 fr -> f0 + d * fr) f freqs
  in  map sin f
  where
    d = 2 * pi / fromIntegral sr
    sr = rate (__ :: p)

------------------------------------------------------------------------------
-- * Wave guide

delayt :: forall p. Clock p Int => Time -> SF p Sample Sample
delayt dur = SF $ \st -> replicate samples 0 ++ st where
  samples = time2samples sr dur
  sr = fromIntegral $ rate (__ :: p)

type Time = Double

time2samples :: Double -> Time -> Int
time2samples sr dur = truncate (dur * sr)

lineSeg :: forall a p. Clock p Int => [Double] -> [Time] -> SF p a Sample
lineSeg amps durs =
  let ls0 (a1:a2:as) (dur:ds) = sig ++ ls0 (a2:as) ds where
        sig = take samples $ iterate (+del) a1
        del = (a2-a1) / (dur*sr)
        samples = truncate (dur*sr)
        sr = fromIntegral $ rate (__ :: p)
      ls0 _ _ = []
  in  SF $ \_ -> ls0 amps durs

rand :: forall p. Clock p Int => Double -> SF p Sample Sample
rand amp = SF $ \amps -> zipWith (*) (randomRs (negate amp, amp) g) amps
  where g = mkStdGen 1234

lowpass :: forall p. Clock p Int => Double -> SF p Sample Sample
lowpass c = SF $ \as ->
  let bs = 0 : zipWith (\inp out -> out + c * (inp - out)) as bs
  in  bs

-- | Designed after Perry Cook's STK flute.
--
flute0 ::
  Time      -- ^ Duration of sound
  -> Double -- ^ Amplitude
  -> Double -- ^ Frequency
  -> Double -- ^ Pressure
  -> Double -- ^ Breath
  -> AR
flute0 dur amp fqc press breath =
  let kenv1 = lineSeg
        [0, 1.1 * press, press, press, 0]
        [0.06, 0.2, dur - 0.16, 0.02] :: CR
      kenv2 = lineSeg
        [0,1,1,0]
        [0.01, dur - 0.02, 0.01] :: CR
      kenvibr = lineSeg
        [0, 0, 1, 1]
        [0.5, 0.5, dur - 1] :: CR
      bore = delayt (1 / fqc)    -- bore delay
      emb = delayt (1 / fqc / 2) -- embouchre delay
      feedbk1 = 0.4
      feedbk2 = 0.4
  in  proc _ -> do
    rec env1 <- upSample kenv1 -< ()
        env2 <- upSample kenv2 -< ()
        envibr <- upSample kenvibr -< ()
        flow <- rand 1.0 -< env1
        sin5 <- sinA 5 -< ()
        let vibr = sin5 * 0.1 * envibr         -- vibrato signal
            sum1 = breath * flow + env1 + vibr -- loop part
        flute <- bore -< out
        x <- emb -< sum1 + flute * feedbk1
        out <- lowpass 0.27 -< x - x * x * x + flute * feedbk2
    returnA -< out * amp * env2

-- | Write WAVE, single channel.
ww :: FilePath -> [Sample] -> IO ()
ww path xs =
  let mkChunks as = case as of
        [] -> []
        _  -> let (pre,post) = splitAt chunkSize as
              in  V.fromList pre : mkChunks post
      chunkSize = 1024
      sr = rate (__ :: AudRate)
      writer = writeWave path (AudioFormat 1 (fromIntegral sr) 16)
  in  runAudioMonad (I.enumList (mkChunks xs) writer >>= I.run)

main :: IO ()
main = do
  args <- getArgs
  case args of
    path:freq:_ ->
      let sr = rate (__ :: AudRate)
          sig = sinA (read freq) :: AR
          -- sig = flute0 (fromIntegral sr) 0.3 440 0.5 0.8
      in  ww path (take sr $ mkStream sig)
    _           -> putStrLn "Usage: <outfile> <freq>"

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     base:freq:_ ->
--       let sr = rate (__ :: AudRate)
--           freq' = read freq
--           mkFlute p b = take sr $ mkStream (flute0 (fromIntegral sr) 0.5 freq' p b)
--       in  sequence_ [ ww n (mkFlute (i*0.1) (j*0.1))
--                     | i <- [0..9], j <- [0..9]
--                     , let n = base ++ show i ++ "_" ++ show j ++ ".wav"
--                     , let x = i * 0.1
--                     , let y = j * 0.1 ]
