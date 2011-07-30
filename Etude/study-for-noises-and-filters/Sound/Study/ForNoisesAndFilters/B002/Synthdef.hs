------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- TODO:
--
-- * Add a noise with slow attack, 4beat.
--
-- * Add hat like sound?
--
-- * Add melody.
--
-- * Add kick?
--
module Sound.Study.ForNoisesAndFilters.B002.Synthdef where

import System.Random

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

go :: IO ()
go = audition $ mrg [qn, bo] where
  qn =
    quickNoise'
      hit
      -- 0.125
      (linLin (lfdNoise1 'd' kr (1/10.32)) (-1) 1 6.25e-2 0.25)
      -- (linLin (lfdNoise1 'd' kr (1/10.32)) (-1) 1 0.125 0.25)
      -- 1e-3
      (linLin (sinOsc kr (1/12.123) 0) (-1) 1 1e-3 999e-3 `lag` 0.8)
      -- (lfCub kr (1/12.119) 0 * 0.4 + 0.5) --
      -- (clip2 (lfdNoise0 'a' kr (1/13)) 1)
      fp
      -- (lfdNoise3 'f' kr (1/3.389898989) * 3000 + 3200) where
      (linLin (sinOsc kr (1/13.321) 0) (-1) 1 (-10) 10)
  hit = demand tick 0 (supply0 qnT1) * tick
  fp = demand tick 0 (evalSupply qnF1 g0)
  tick = impulse kr ttempo 0
  g0 = mkStdGen 0

  bo = bosc' (toSustain boG1 tick) p1 0.2

  t'' = demand tick 0 $ supply0 boF1
  t' = midiCPS $ gate t'' (t'' >* 0)
  p1 = t' / 5

ttempo = 2*290/60

r :: IO ()
r = withSC3 reset

-- | Trigger values for quickNoise.
qnT1 :: Supply
qnT1 =
  sseq sinf
    [sseq 3 [base1]
    ,sseq 1
     [1.0,   0,  0.5, 1.0, 0.3, 1.0,   0, 0.8
     ,srand 1
      [srand 8 [0, 0.2, 0.4, 1.0]
      ,sseq 8 [0]]]
    ,sseq 2 [base1]
    ,srand 32 [0.4, 0.6, 0.8, 1.0]]
  where
    base1 = sser 16
      [1.0,   0, 0.5, 1.0, 0.3, 1.0,   0, 0.8
      ,0.4, 1.0,   0, 1.0, 1.0,   0, 0.8,   0]

-- | Frequencies for quickNoise.
qnF1 :: Supply
qnF1 =
  sseq sinf
  [sseq 6
   [420, srand 3 fs, 420, 2200, srand 1 fs]
  ,swhite 16 20 12000]
  where
    fs = [440, 880, 1760, 3520, 7040, 14080]

-- | Gate values for bosc.
boG1 :: Supply
boG1 =
  sseq sinf
  [sseq 3 [base]
  ,srand 1
   [sseq 4 [1.0,srand 1 [0,-1],0.8,srand 1 [0,-1]]
   ,sseq 1 [sseq 2 [1,0.8,srand 1 [0,-1]], 1, srand 1 [0,-1]
           ,sseries 8 0.4 7.5e-2]
   ,sseq 1 [1,sseq 14 [-1],0.8]]
  ,sseq 3 [base]
  ,swhite 16 0.3 1]
  where
    base =
      sseq 1 [1,0.5,srand 1 [0,-1], 0.8,0.4,srand 1 [0,-1]
             ,0.6,0.4,srand 1 [0,-1], 0.7,0.4,srand 1 [0,-1]
             ,0.8,srand 1 [0,-1,0.7],0.8,srand 1 [0,-1,0.4]]

-- | Frequency value for bosc, in midi node.
boF1 :: Supply
boF1 =
  sseq sinf
  [sseq 1 [sseq 16 [52], srand 16 [52,64]
          ,srand 16 [52,64,76], srand 16 [52,64]
          ,sseq 16 [52], srand 16 [52,64]
          ,srand 16 [52,64,76], sseq 2 base]
  ,sseq 3 [sseq 48 base
          ,srand 112 base, sseq 2 base]]
  where
    base = [52,57,59,64, 69,71,76,78]

snt1 :: Supply
snt1 =
  sseq sinf
  [-- sseq 128 [0],
   sseq 128 [0,w,0,0]]
  where
    w = swhite 1 0.8 1

hatt1 :: Supply
hatt1 =
  sseq sinf [1,w1,w2,w1]
  where
    w1 = swhite 1 0.3 0.6
    w2 = swhite 1 0.4 0.8

-- | Synthdef to count beat.
--
-- 'outt' give 2 triggers in each single beat, 'outb' gives the number
-- of current beat count.
--
b002met :: UGen
b002met = mrg [out ("outt"@@0) sig, out ("outb"@@0) cnt] where
  sig = impulse kr f 0
  cnt = pulseCount sig 0
  f = 2 * ("bpm"@@60) / 60 -- 2 triggers per beat.

quickNoiseC :: UGen
quickNoiseC = quickNoiseC' ("t_trig"@@0)
quickNoiseC' tick = mrg [out ("outt"@@0) t,out ("outf"@@0) f] where
  t = demand tick 0 (supply0 qnT1) * tick
  f = demand tick 0 $ supply0 qnF1

quickNoise :: UGen
quickNoise = quickNoise' ("t_trig"@@0) dur atck ("freq"@@6600) envn where
  -- dur = 0.125
  dur = linLin (lfdNoise1 'd' kr (1/10.32)) (-1) 1 6.25e-2 0.25
  atck = linLin (sinOsc kr (1/12.123) 0) (-1) 1 1e-3 999e-3 `lag` 0.2
  envn = linLin (sinOsc kr (1/13.321) 0) (-1) 1 (-10) 10

quickNoise' tick dur atk freq en = out ("out"@@0) (sig * aenv * ("amp"@@1.2)) where
  sig = resonz nz (mce [freq',freq'*fmod]) rq
  fmod = linLin (lfdNoise3 'f' kr (1/8.9918)) (-1) 1 0.495 2.005
  nz = whiteNoise 'w' ar
  -- nz = brownNoise 'b' ar
  -- nz = henonC ar (sampleRate/4) 1.4 0.3 (-0.1) 0.1
  -- nz = pinkNoise 'p' ar
  freq' = lfreq
  lfreq = latch freq tick
  aenv = envGen kr tick (latch tick tick) 0 dur DoNothing $
         env [0,1,0,0] [atk,1-atk] [EnvNum en] 0 2
  rq = linLin (lfdNoise1 'r' kr (1/5.12)) (-1) 1 0.1 0.9

slowNoiseC :: UGen
slowNoiseC = out ("out"@@0) sig where
  sig = demand t 0 $ supply0 snt1
  t = "t_trig"@@0

slowNoise :: UGen
slowNoise = slowNoise' ("t_trig"@@0) ("amp"@@0.3) where

slowNoise' tick amp = out ("out"@@0) sig where
  sig = foldr f sig' (map (\x -> rand x 1e-3 3e-2) "random")
  f a b = allpassN b 3e-2 a 5e-2
  sig' = rlpf nz freq rq * aenv * amp
  freq = envGen kr tick 18000 4000 1 DoNothing $
         env [1,1,0] [1e-3,100e-3] [EnvNum (-14)] 0 (-1)
  rq = 0.85
  nz = whiteNoise 'w' ar
  aenv = envGen kr tick (latch tick tick) 0 1 DoNothing $
         env [0,1,0] [atk,30e-3] [EnvNum 4] 0 (-1)
  atk = 200e-3

hatLikeC :: UGen
hatLikeC = out ("out"@@0) (t * demand t 0 (supply0 hatt1)) where
  t = "t_trig"@@1

hatLike :: UGen
hatLike =
  hatLike' ("t_trig"@@0) ("amp"@@0.3)
  (linExp (lfdNoise3 'd' kr (1/8)) (-1) 1 20e-3 80e-3)
hatLike' t_trig amp sst = out ("out"@@0) sig where
  sig = mix (mkSig (mce [3197, 5889, 12210])) * aenv
  mkSig f = resonz nz f (0.1 + (1/f))
  -- nz = henonC ar 12500 1.4 1.3 0 0
  nz = whiteNoise 'n' ar
  aenv = envGen kr t_trig (latch t_trig t_trig) 0 1 DoNothing $
         env [0,1,1,0] [1e-3,sst,15e-3] [EnvNum (-14)] 0 (-1)

boscC :: UGen
boscC = mrg [outg, outf] where
  outg = out ("outg"@@0) (toSustain boG1 tick)
  outf = out ("outf"@@0) (freq / 5)
  freq = midiCPS $ gate freq' (freq' >* 0)
  freq' = demand tick 0 $ supply0 boF1
  tick = "t_trig"@@0

bosc :: UGen
bosc = bosc' ("gt"@@0) ("freq"@@0) ("amp"@@1)

bosc' tick freq amp = out ("out"@@0) sig where
  sig = clip2 (rlpf sig' filtf filtq) 1 * amp -- (latch amp tick)
  -- sig' = pulse ar (freq * mce [1,0.9998]) nh * aenv -- ) bf bq
  sig' = sum [(sig1 + sig2 + sig3) * aenv -- lfCub ar (freq * mce [1,0.9998]) 0 * aenv
             ,pulse ar (freq * mce [1.0001,1]) 0.7 * atk]
  sig1 = combL nz 0.2 (1/ (freq * mce [1,0.9998]))
         (0.7 + (lfdNoise3 'q' kr 1 * 0.25))
  sig2 = combL nz 0.2 (1/ (freq * mce [1.5,1.5002]))
         (0.7 + (lfdNoise3 'q' kr 1 * 0.28))
  sig3 = combL nz 0.2 (1/ (freq * mce [2,1.9998]))
         (0.7 + (lfdNoise3 'q' kr 1 * 0.29))
  -- sig1 = lfPar ar (freq * mce [1,0.999]) 0 * aenv
  nz = whiteNoise 's' ar
  aenv = envGen kr tick tick' 0 1 DoNothing $
         env [0,1,1,0.2,0] [1e-3,80e-3,20e-3,10e-3] [EnvNum (-28)] 3 (-1)
  atk = envGen kr tick tick' 0 1 DoNothing $
        envPerc 1e-3 280e-3
  tick' = latch tick tick
  filtf = 8800
  filtq = 0.8

-- | Mixer with pan, for single channel input.
b002mix1 :: UGen
b002mix1 = out ("out"@@0) sig where
  input = "a_in"@@0
  sig = pan2 flt ("pan"@@0) 1
  flt = input * ("amp"@@1)
  -- flt = sum [bBandPass input ("lf"@@80) lbw
  --           ,bBandPass input ("mf"@@1200) mbw
  --           ,bBandPass input ("hf"@@8000) hbw]
  -- lbw = mouseX kr 0 10 Linear 0.2
  -- mbw = sinOsc kr (1/5.12) 0 * 5 + 5
  -- hbw = mouseY kr 0 10 Linear 0.2

-- | Mixer without pan, for dual channel input.
b002mix2 :: UGen
b002mix2 = out ("out"@@0) sig where
  inl = "a_inl"@@0
  inr = "a_inr"@@1
  sig = mce [inl, inr] * ("amp"@@1)

-- | Master control
b002mst :: UGen
b002mst = replaceOut ("out"@@0) sig where
  sig = sig'
  sig' = mce ["a_inl"@@0, "a_inr"@@1]

-- | Sustains with given trigger.
--
-- When value 'x' is:
--
-- * x > 0  : Triggers output x and hold as x.
--
-- * x == 0 : Gives output 0
--
-- * x < 0  : Holds last value.
toSustain :: Supply -- ^ Demand ugen pattern containint above values
          -> UGen -- ^ Trigger
          -> UGen -- ^
toSustain sup tick = sig where
  sig = gate (sig' * (1 - (tick >* 0))) (sig' >=* 0)
  sig' = demand tick 0 $ supply0 sup

supply0 :: Supply -> UGen
supply0 p = evalSupply p (mkStdGen 0)