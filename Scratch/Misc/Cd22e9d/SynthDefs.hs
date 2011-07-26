{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Module to play with sending node repeatedly with response to sendReply.
This module contains UGen to be parsed with writeSynthdef.

TODO:

* Add equalizer to mixer, and mix it.

-}
module Cd22e9d.SynthDefs where

import System.Random

import Control.Applicative
import Control.Monad.State
import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

--
-- master controls
--

met9d :: UGen
met9d = out ("out"@@0) (impulse kr bpm 0) where
  bpm = ("bpm"@@120) / 60

tr9d :: UGen
tr9d = mrg [out ("out"@@0) pc, sendReply t 0 "/cd22e9d" [pc]] where
  t = "t_trig"@@0
  pc = pulseCount t 0

fo9d :: UGen
fo9d = out ("out"@@0) sig where
  sig = envGen kr finish 1 0 1 DoNothing $
        env [1,0] [10] [EnvCub] (-1) (-1)
  finish = ("t_barCount"@@0) ==* 60

--
-- trigger controls
--

-- For recording  piece

cd2thuh1 :: UGen
cd2thuh1 =
  mkDemand $ sseq sinf
  [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0]

cd2thuh2 :: UGen
cd2thuh2 =
  mkDemand $ sseq 1
  [sseq 64 [0]
  ,sseq sinf
   [sseq 12 [1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0]
   ,sseq 32 [0], sseq 16 [0,1]]]

cd2thuh3 :: UGen
cd2thuh3 =
  mkDemand $ sseq 1
  [sseq 64 [0]
  ,sseq sinf
   [sseq 12 [0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1]
   ,sseq 32 [0], sseq 16 [1,0]]]

cd2that :: UGen
cd2that =
  mkDemand $ sseq 1
  [sseq 64 [0]
  ,sseq sinf
   [sseq 3 [sseq 32 [0,1]]
   ,sseq 32 [0], srand 32 [0,1]]]

cd2tsnr :: UGen
cd2tsnr =
  mkDemand $ sseq 1
  [sseq 64 [0]
  ,sseq sinf
   [sseq 3
    [sseq 3 [sseq 3 [0,0,1,0], sseq 1 [0,0,1,1]]
    ,sseq 1 [sseq 2 [0,0,1,0], srand 8 [0,1,1]]]
   ,sseq 3 [0,0,0,0, 1,0,0,0, 0,0,0,0, srand 4 [1,0]]
   ,sseq 1 [0,0,0,0, 1, srand 11 [0,1]]]]

cd2tkik :: UGen
cd2tkik =
  mkDemand $ sseq sinf
  [sseq 4 [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,1]
  ,sseq sinf
   [sseq 3
    [sseq 4 [1,0,0,0, 1,0,0,srand 1 [0,1]
            ,1,0,0,0, 1,0,0,srand 1 [0,1]]]
   ,sseq 4 [1,0,0,1, 1,0,0,1, 0,1,0,1, 1,0,0,1]]]

cd2tdrn1 :: UGen
cd2tdrn1 = mkTdrn pat where
  pat =
    sseq 1
    [sseq 32 [0]
    ,sseq sinf
     [sseq 3
      [72, 0, 0, 0,  0, 0, 0, 0,  0,67, 0, 0, 65, 0, 0, 0
      ,67, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 65, 0, 0, 0
      ,60, 0, 0, 0,  0, 0, 0, 0,  0,55, 0, 0, 65, 0, 0, 0
      ,67, sseq 15 [0]]
     ,sseq 1
      [72, sseq 31 [0]
      ,60, sseq 31 [0]]]]

cd2tdrn2 :: UGen
cd2tdrn2 = mkTdrn pat where
  pat =
    sseq 1
    [sseq 32 [0]
    ,sseq sinf
     [ 0, 0, 55,0,  0, 0,60, 0,  0, 0, 0, 0,  0, 0, 0, 0
     , 0, 0, 0, 0, 67, 0, 0, 0,  0, 0, 0, 0, 60, 0, 0, 0
     , 0, 0, 0, 0,  0, 0,67, 0,  0, 0, 0, 0,  0, 0, 0, 0
     , 0, 0, 0, 0, 60, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0]]

cd2tpu :: UGen
cd2tpu =
  out ("out"@@0) $ midiCPS $ demand ("t_trig"@@0) 0 $
  evalSupply pat $ mkStdGen 0xffafacc8809 where
    pat =
      sseq 1
      [sseq 32 [0]
      ,sseq sinf
       [srand 7
        [sseq 1 [36,55,62,36, 55,62,36,55]
        ,sseq 1 [36,60,72,36, 60,72,36,60]
        ,sseq 1 [36,53,58,36, 53,58,36,53]]
       ,36, srand 2 [60,67]
       ,36, srand 2 [67,72]
       ,srand 2 [48,53,55,60,65,67]]]

-- For playing with switching between patterns

cd2thuh1a :: UGen
cd2thuh1a  =
  mkDemand $ sseq sinf [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0]

cd2thuh2a :: UGen
cd2thuh2a  =
  mkDemand $ sseq sinf [1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0]

cd2thuh3a :: UGen
cd2thuh3a =
  mkDemand $ sseq sinf [0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1]

cd2thata :: UGen
cd2thata  =
  mkDemand $ sseq sinf [0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1]

cd2tsnra :: UGen
cd2tsnra =
  mkDemand $ sseq sinf
  [sseq 3 [sseq 3 [0,0,1,0], sseq 1 [0,0,1,1]]
  ,sseq 1 [sseq 2 [0,0,1,0], srand 8 [0,1,1]]]

cd2tkika :: UGen
cd2tkika  =
  mkDemand $ sseq sinf [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,1]

cd2thuh1b :: UGen
cd2thuh1b =
  mkDemand $ sseq sinf [0,1,0,1, 1,1,0,0, 0,0,0,1, 1,0,0,1]

cd2thatb :: UGen
cd2thatb =
  mkDemand $ srand sinf [1,0]

cd2tsnrb :: UGen
cd2tsnrb =
  mkDemand $ sseq sinf
  [sseq 1 [0,0,0,0, 1,0,0,0, 0,0,0,0], srand 4 [0,1]]

cd2tkikb :: UGen
cd2tkikb =
  mkDemand $ sseq sinf [1,0,0,1, 1,0,0,1, 0,1,0,1, 1,0,0,1]

cd2tpua :: UGen
cd2tpua =
  out ("out"@@0) $ midiCPS $ demand ("t_trig"@@0) 0 $
  evalSupply s1 $ mkStdGen 0xffafacc8809

s1 :: Supply
s1 =
  sseq sinf
  [srand 7
   [sseq 1 [36,55,62,36, 55,62,36,55]
   ,sseq 1 [36,60,72,36, 60,72,36,60]
   ,sseq 1 [36,53,58,36, 53,58,36,53]]
  ,36, srand 2 [60,67]
  ,36, srand 2 [67,72]
  ,srand 2 [48,53,55,60,65,67]]

cd2tdrn2a :: UGen
cd2tdrn2a = cd2tdrn1a

cd2tdrn1b :: UGen
cd2tdrn1b = cd2tdrn' ("t_trig"@@1)
cd2tdrn' tick = mrg [fval, rval] where
  fval = out ("outf"@@0) (midiCPS (gate val (val >* 0)) `lag` 0.6)
  rval = out ("outg"@@0) $ 0 <=* val
  val = demand tick 0 $ evalSupply pat (mkStdGen 0xFAFAFAFACCDD)
  pat = sseq sinf
        [72, 0, 0, 0,  0, 0, 0, 0,  0,67, 0, 0, 65, 0, 0, 0
        ,67, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 65, 0, 0, 0
        ,60, 0, 0, 0,  0, 0, 0, 0,  0,55, 0, 0, 65, 0, 0, 0
        ,67, sseq 15 [0]]

cd2tdrn1a :: UGen
cd2tdrn1a = mkTdrn pat where
  pat = sseq sinf
        [72, 0, 0, 0,  0, 0, 0, 0,  0,67, 0, 0, 65, 0, 0, 0
        ,67, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 65, 0, 0, 0
        ,60, 0, 0, 0,  0, 0, 0, 0,  0,55, 0, 0, 65, 0, 0, 0
        ,67, sseq 15 [0]]

cd2tdrn2b :: UGen
cd2tdrn2b = mkTdrn pat where
  pat = sseq sinf
        [ 0, 0,55, 0,  0, 0,60, 0,  0, 0, 0, 0,  0, 0, 0, 0
        , 0, 0, 0, 0, 67, 0, 0, 0,  0, 0, 0, 0, 60, 0, 0, 0
        , 0, 0, 0, 0,  0, 0,67, 0,  0, 0, 0, 0,  0, 0, 0, 0
        , 0, 0, 0, 0, 60, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0]

--
-- sound sources
--

cd2huh :: UGen
cd2huh = cd2huh' (whiteNoise 'a' ar) ("t_trig"@@0)
cd2huh' srcn tick = out ("out"@@0) (mix $ resonz srcn freq bndw * ampe) where
  freq = mce [tRand 'a' 600 800 tick
             ,tRand 'b' 1000 1400 tick
             ,tRand 'c' 2400 2800 tick] `lag` 0.1
  bndw = mce [130/700, 70/1220, 160/2600]
  ampe = envGen kr (coinGate 'g' 0.99 tick) 1 0 1 DoNothing $ envSine ed ev
  ed = tRand 'd' 0.1 0.4 tick
  ev = tRand 'e' 0.1 0.6 tick

cd2nzf :: UGen
cd2nzf = cd2nzf' ("t_amp"@@0) ("freq"@@0)
cd2nzf' amp freq = out ("out"@@0) sig where
  sig = sum [rlpf nz freq 2 * ae1
            ,rlpf nz (freq*2.002321) 1.5 * ae2
            ,rlpf nz (freq*2.9989989) 1 * ae3]
  nz = pulse ar freq (lfdNoise3 'f' kr 9.32 * 0.5 + 0.5)
  ae1 = mkAE [0,1,0.2,0.8,0] [28e-3,200e-3,100e-3,285e-3]
  ae2 = mkAE [0,0.5,0.8,0] [120e-3,30e-3, 130e-3]
  ae3 = mkAE [0,1,0.2,0] [25e-3, 180e-3, 310e-3]
  mkAE vs ts = envGen kr amp amp 0 0.25 DoNothing $ env vs ts [EnvNum 3] (-1) (-1)

cd2cgt :: UGen
cd2cgt = out ("out"@@0) (coinGate 'g' ("prob"@@0.5) ("t_trig"@@0) * ("amp"@@1))

cd2kik :: UGen
cd2kik = cd2kik' ("t_trig"@@0)
cd2kik' tick = out ("out"@@0) ((lfCub ar freq 0.05 + impl) * ampe) where
  freq = (mix $ mce [200.32, 230.32, 360.79, 110.13]) * fenv
  fenv = envGen kr tick 1 0 1 DoNothing $
         env [0.2, 0.2, 0.1, 0.1] [10e-3, 10e-3, 10e-3] [EnvSqr] 0 (-1)
  impl = impulse ar 28 0.3 * decay2 tick 1e-4 2e-3
  lvls = tRand '\131' 0.75 1 tick
  ampe = envGen kr tick lvls 0 1 DoNothing $
         env [0,1,1,0] [1e-3,25e-3,228e-3] [EnvNum (-13)] (-1) (-1)

cd2snr :: UGen
cd2snr = cd2snr' ("t_trig"@@0)
cd2snr' tick = out ("out"@@0) (sig * ampe * 0.3) where
  sig = mix $ ringz (whiteNoise 'a' ar) fs qs * 0.1
  fs = mce [943.232, 350.32, 680.192]
  qs = mce [0.1,0.05,0.025]
  lvls = tRand '\132' 0.75 1 tick
  ampe = envGen kr tick lvls 0 1 DoNothing $
         env [0,1,1,0] [1e-3,15e-3,189e-3] [EnvNum (-8)] (-1) (-1)

cd2hat :: UGen
cd2hat = cd2hat' ("t_trig"@@0)
cd2hat' tick = out ("out"@@0) (sig * amp) where
  sig = mix $ rhpf (whiteNoise 'z' ar)
        (mce [5908.32,8803,6723]) (mce [0.1,0.1,0.1])
  lvls = tExpRand '\133' 0.6 1 tick
  amp = envGen kr tick lvls 0 1 DoNothing $
        env [0,1,0] [3e-4, 80e-3] [EnvNum (-3)] (-1) (-1)

cd2drn :: UGen
cd2drn = cd2drn' ("amp"@@0.3) ("gate"@@1) ("freq"@@440)
cd2drn' amp gt freq = out ("out"@@0) sig where
  sig = foldr (\a b -> allpassN b 0.05 a 4) sig' $
        map (\i -> mce2 (rand i 0 0.05) (rand (succ i) 0 0.05)) "qrst"
  sig' = resonz sig'' q 0.5
  q = lfdNoise3 '\813' kr 1.23 * 2300 + 3000
  sig'' = lfPulse ar freq 0 bw * aenv
  bw = lfdNoise3 '\992' kr 0.7778 * 0.4 + 0.5
  aenv = envGen kr gt amp 0 1 DoNothing $
         env [0, 1, 0.8, 0.8, 0]
         [5e-3, 20e-3, 20e-3, 30e-3] [EnvCub] 2 1

cd2pu :: UGen
cd2pu = cd2pu' ("t_trig"@@0)
cd2pu' tick = out ("out"@@0) $ foldr f v (zipWith mce2 rs1 rs2) where
  v = rlpf (pulse ar (mce2 freq (freq*1.01)) bw * 0.2 * ampe * amp)
      (lfdNoise3 'n' kr 2.323 * 2000 + 2200)
      (lfdNoise3 'q' kr 1.110 * 0.498 + 0.5)
  f a b = allpassN b 0.05 a 4
  rs1 = map mkR "abcd"
  rs2 = map mkR "efgh"
  mkR x = rand x 0.001 0.05
  freq = "freq"@@0
  ampe = decay2 tick 5e-4 950e-3
  amp = "amp"@@0.3
  bw = lfdNoise3 'b' kr 0.1123 * 0.48 + 0.5

--
-- effects
--

cd2rev :: UGen
cd2rev = cd2rev' ("a_in"@@0)
cd2rev' input = replaceOut ("out"@@0) sig where
  sig = mix $ foldr f input (zipWith mce2 r1 r2)
  r1 = map mkR [(1::Int) .. 3]
  r2 = map mkR [(101::Int) .. 103]
  mkR i = rand i 0.001 0.05
  f a b = b + combC b 0.5 ("dlyt"@@0.123) a * ("dmul"@@0.25)

--
-- mixer
--

cd2mix :: UGen
cd2mix = cd2mix' ("a_in"@@0)
cd2mix' input =
  out ("out"@@0) ((pan2 input ("pan"@@0) 1) * ("amp"@@0) * ("mamp"@@1))

cd2mixm :: UGen
cd2mixm = cd2mixm' ("a_in"@@0)
cd2mixm' input = out ("out"@@0) (input * ("amp"@@1))

cd2mst :: UGen
cd2mst = cd2mst' ("amp"@@0)
cd2mst' amp =
  replaceOut ("out"@@0)
  (hpf (limiter (mce [l,r] * amp) 1 0.25) 15) where
  l = in' 1 ar 0
  r = in' 1 ar 1

--
-- utility
--

mkDemand :: Supply -> UGen
mkDemand p = out ("out"@@0) (demand ("t_trig"@@0) 0 p' * ("t_trig"@@0)) where
  p' = evalSupply p (mkStdGen 0x92af8afff)

mkTdrn :: Supply -> UGen
mkTdrn pat = mrg [fval, gval] where
  fval = out ("outf"@@0) (midiCPS (gate val (val >* 0)) `lag` ("lag"@@0.6))
  gval = out ("outg"@@0) $ 0 <=* val
  val = demand ("t_trig"@@0) ("t_reset"@@0) $
        evalSupply pat (mkStdGen 0xafcd08921fb)

-- | Multiply trigger value with buffer index value.
bufTrig :: UGen -- ^ Buffer index
        -> UGen -- ^ Trigger
        -> UGen
bufTrig i t = index i (stepper t 0 0 (bufFrames kr i -1) 1 0) * t

-- | Percussive filtered noise sound, for next piece.
cd2tkl :: UGen
cd2tkl = cd2tkl' ("t_trig"@@1)
cd2tkl' tick =
  out ("out"@@0) $ decay2 tick 1e-3 1.2 * sig * 0.2 where
    sig = foldr f sig' (map (\i -> rand i 0.001 0.05) "abwewpoew")
    f a b = allpassN b 0.05 a 4
    sig' = ringz nz freq rt
    freq = tExpRand 'f' 1020 12800 tick
    nz = pinkNoise 'a' ar
    rt = mouseX kr 0.04 4 Linear 0.1

--
-- Too glassy for now
--

-- cd2tnzfa :: UGen
-- cd2tnzfa = mrg [amp, freq] where
--   amp  = out ("outa"@@0) ("t_trig"@@1)
--   freq = out ("outf"@@0) (midiCPS . (+60) $ demand ("t_trig"@@1) 0 pat)
--   pat  = dseq 'a' dinf $ mce $
--          [2,0,7,0, 7,5,7,0,  0,0,2,7, 0,7,5,7]

-- cd2tnzfa2 :: UGen
-- cd2tnzfa2 = mrg [amp, freq] where
--   amp  = out ("outa"@@0) ("t_trig"@@1)
--   freq = out ("outf"@@0) (midiCPS . (+60) $ demand ("t_trig"@@1) 0 pat)
--   pat  = dseq 'a' dinf $ mce $
--          [0,0,0,0, 0,5,7,0, 0,0,2,7, 0,0,0,0]

-- cd2tnzfb :: UGen
-- cd2tnzfb = mrg [amp, freq] where
--   amp  = out ("outa"@@0) ("t_trig"@@1)
--   freq = out ("outf"@@0) (midiCPS . (+48) $ demand ("t_trig"@@1) 0 pat)
--   pat  = dseq 'b' dinf $ mce $
--          [0 ,0, (-5), 0,  0, 0, (-5), 0,  7, 0, 0, 0,  0, 0, (-5), 0]

-- cd2tnzfb2 :: UGen
-- cd2tnzfb2 = mrg [amp, freq] where
--   amp  = out ("outa"@@0) ("t_trig"@@1)
--   freq = out ("outf"@@0) (midiCPS . (+48) $ demand ("t_trig"@@1) 0 pat)
--   pat  = dseq 'b' dinf $ mce $
--          [0 ,0, (-5), 0,  0, 0, (-5), 0,  7, 0, 0, 0,  0, 0, (-5), 0]
