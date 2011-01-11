------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_tea.html>
--
-- Try:
--
-- > > n <- anon material
-- > > metal n
-- > > glass1 n
-- > > glass2 n
-- > > china n
-- > > plastic n
-- > > wood n
-- > > cardBoard n
-- > > audition noiseRainbow
-- > > audition pipeVesselLiquid
--
-- TODO:
--
-- * Make other sounds
-- * Add sequence code for controlling sound
--
module Pssd.Earth.Tea where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | UGen for material sound.
material :: UGen
material = out 0 $ mce2 sig sig
  where
    sig = sum $ map f ["freq1", "freq2", "freq3", "freq4"]
    nz = whiteNoise 'a' ar * amp
    amp = linExp (linen t 1e-9 1 12e-2 DoNothing) 1e-9 1 1e-9 1
    f x = bpf nz (ctrl x 440) q
    q = ctrl "q" 0.5
    t = ctrl "t_trig" 0

-- | Hit material node.
hitMaterial :: Double -- ^ Frequency 1
            -> Double -- ^ Frequency 2
            -> Double -- ^ Frequency 3
            -> Double -- ^ Frequency 4
            -> Double -- ^ Q
            -> Int    -- ^ Node id
            -> IO ()
hitMaterial f1 f2 f3 f4 q n = withSC3 $ \fd ->
  send fd $ n_set n [("freq1", f1), ("freq2", f2), ("freq3", f3)
                    ,("freq4", f4), ("q", q), ("t_trig", 1)]

-- | Hit with metal sound frequencies.
metal :: Int -> IO ()
metal = hitMaterial 3454 5645 6433 6999 0.6

-- | Hit with glass sound frequencies, take 1.
glass1 :: Int -> IO ()
glass1 = hitMaterial 2312 2466 3200 5314 0.38

-- | Hit with glass sound frequencies, take 2.
glass2 :: Int -> IO ()
glass2 = hitMaterial 1679 1233 4251 4640 0.34

-- | Hit with china sound frequencies.
china :: Int -> IO ()
china = hitMaterial 1245 1503 2131 2575 0.1

-- | Hit with plastic sound frequencies.
plastic :: Int -> IO ()
plastic = hitMaterial 527 781 972 1184 0.07

-- | Hit with wood sound frequencies.
wood :: Int -> IO ()
wood = hitMaterial 221 333 426 527 0.04

-- | Hit with cardboard sound frequencies.
cardBoard :: Int -> IO ()
cardBoard = hitMaterial 109 230 352 413 0.01

-- | Black noise
black :: UGen
black = out 0 $ mce2 sig sig
  where
    sig = noise * mouseY kr 0 10 Linear 0.1
    noise = foldl (\n _ -> lpf n 50) (whiteNoise 'a' ar) [1..2]

-- | Helper function for applying high pass and low pass filter to whiteNoise.
mkHpLp :: UGen -- ^ Low frequency
       -> UGen -- ^ High frequency
       -> UGen -- ^ mouseY upper value
       -> UGen
mkHpLp lo hi y = mce2 sig sig
  where
    sig = n0 * mouseY kr 0 y Linear 0.1
    n0 = foldl (\n _ -> lpf n hi) n1 [1..4]
    n1 = foldl (\n _ -> hpf n lo) n2 [1..4]
    n2 = whiteNoise 'a' ar

-- | Brown noise
brown :: UGen
brown = out 0 $ mkHpLp 50 250 10

-- | Red noise
red :: UGen
red = out 0 $ mkHpLp 250 500 10

-- | Orange noise
orange :: UGen
orange = out 0 $ mkHpLp 500 1000 4

-- | Yellow noise
yellow :: UGen
yellow = out 0 $ mkHpLp 1000 3000 3

-- | Green noise
green :: UGen
green = out 0 $ mkHpLp 3000 7000 2

-- | Blue noise
blue :: UGen
blue = out 0 $ mkHpLp 7000 15000 2

-- | Violet noise
violet :: UGen
violet = out 0 $ mkHpLp 15000 20000 2

-- | Mouse x chooses noise colour, mouse y controls amplitude.
noiseRainbow :: UGen
noiseRainbow = out 0 $ mce2 sig sig
  where
    sig = sum $ zipWith manage (map constant [0..])
          [blk,brn,rd,orn,ylw,grn,blu,vlt]
    choose = mouseX kr 0 8 Linear 0.1
    manage i n = n * (i <=* choose * choose <=* (i+1))
    blk = foldl (\n _ -> lpf n 50) (whiteNoise 'a' ar) [1..2] *
          mouseY kr 0 10 Linear 0.1
    brn = mkHpLp 50 250 10
    rd  = mkHpLp 250 500 4
    orn = mkHpLp 500 1000 3
    ylw = mkHpLp 1000 3000 3
    grn = mkHpLp 3000 7000 2
    blu = mkHpLp 7000 15000 2
    vlt = mkHpLp 15000 20000 2

liquidNoise :: UGen
liquidNoise = oscil * delta
  where
    oscil = sinOsc ar lin 0
    delta = squared $ lpf (clip (cubed $ cubed $ lin) 0 1) 20 * 0.9
    lin = linExp (lag (tRand 'a' 1e-9 1 trg) 2e-3) 1e-9 1 1e-9 1 * 2800 + 200
    trg = impulse kr (1/6e-3) 0

-- | From diagram /liquids using bilinear/.
bilinearLiquid :: UGen
bilinearLiquid = out 0 $ mce2 sig sig
  where
    sig = hpf liquidNoise 20

-- | From diagram /liquid with a pipe\/vessel formant/.
-- Mouse x controls fading between pipe/vessel and mere liquid noise.
pipeVesselLiquid :: UGen
pipeVesselLiquid = mrg [ xOut 0 x $ mce2 p p
                       , xOut 0 (1-x) $ mce2 l l ]
  where
    p = sum [ bpf liquidNoise 124 8e-2
            , bpf liquidNoise 345 8e-2
            , bpf liquidNoise 678 8e-2
            , bpf liquidNoise 987 8e-2 ]
    l = hpf liquidNoise 20 * 0.5
    x = mouseX kr 0 1 Linear 1e-2

-- | Cross fading example.
xo :: UGen
xo = mrg [ xOut 0 (1-y) (p 220.023898 221.0232111)
         , xOut 0 y     (p 329.889829 330.1239281)
         , xOut 0 x     (p 120.881981 121.0013223)
         , xOut 0 (1-x) (p 439.811131 441.1846598) ]
  where
    p a b = sinOsc ar (mce [a, b]) 0 * 0.1
    x = lfNoise1 'x' kr 1 * 0.5 + 0.5
    y = lfNoise1 'y' kr 1 * 0.5 + 0.5
