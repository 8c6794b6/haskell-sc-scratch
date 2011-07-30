------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Sound.Study.ForNoisesAndFilters.B002.Synthdef where

import Control.Arrow
import Data.Maybe (catMaybes)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

go :: IO ()
go = bngo

pp01 :: UGen
pp01 = pp' t a f p
  where
    f = tExpRand 'f' 1200 12800 t
    a = tExpRand 'a' 0.05 0.15 t
    p = tRand 'p' (-1) 1 t
    t = dust 'd' kr (mouseY kr 0.25 2 Exponential 0.1)

pp :: UGen
pp = pp' ("t_trig"@@1) ("amp"@@0.3) ("freq"@@1200) ("pan"@@0)
pp' tick amp freq pan = out ("out"@@0) sig where
  sig = foldr f sig' $ map (\x -> rand x 1e-4 5e-3) "blahouqp32813"
  f a b = allpassN b 5e-3 a 3
  sig' = pan2 (ringz (nz * aenv) freq q) pan 1
  aenv = decay2 tick 1e-3 1 * amp
  -- nz = pinkNoise 'p' ar
  nz = henonC ar (sampleRate/2) {- 8800 -} 1.4 0.3 0 0
  q = lfdNoise3 'q' kr 1 * 2.4 + 2.401

fshift :: UGen
fshift = fshift' ("a_in"@@0)
fshift' input = out ("out"@@0) sig where
  sig = freqShift input (sinOsc kr f 0 * 250.5 + 250.5) 0
  f = sinOsc kr (1/5.1) 0 * 2.5 + 2.5

fs2 :: UGen
fs2 = out ("out"@@0) sig where
  sig = freqShift source (sinOsc kr f 0 * 250.5 + 250.5) 0
  f = sinOsc kr (1/5.1) 0 * 2.5 + 2.5
  source = ringz (pinkNoise 'p' ar * aenv * 0.3) freq rq
  freq = tExpRand 'f' 1200 8800 tick
  rq = lfdNoise3 'q' kr 1 * 0.49 + 0.5
  aenv = decay2 tick 1e-3 1
  tick = dust 'd' kr 1

s1 = sseq sinf
     [sseq 3
      [1.0,   0, 0.5, 1.0, 0.3, 1.0,   0, 0.8
      ,0.4, 1.0,   0, 1.0, 1.0,   0, 0.8,   0]
     ,sseq 1
      [1.0,   0, 0.5, 1.0 ,0.3, 1.0,   0, 0.8
      ,srand 8 [0, 0.2, 0.4, 1.0]]
     ,sseq 2
      [1.0,   0, 0.5, 1.0 ,0.3, 1.0,   0, 0.8
      ,0.4, 1.0,   0, 1.0 ,1.0,   0, 0.8,   0]
     ,srand 32
      [0.4,0.6,0.8,1.0]]

-- s1 = sseq sinf
--      [sseq 3
--       [  1,  0,0.5,  0  ,0.3,1.0,  0,0.4
--       ,0.4,1.0,  0,0.4  ,1.0,  0,0.5,  0]
--      ,srand 16
--       [0,0.2,0.4,0.6,0.8,1.0]]

s2 = sseq sinf
     [srand 2 [0, 0.3, 0.6, 1.0], 1.0, srand 1 [0, 0.3, 0.6, 1.0]]

s3 =
  sseq sinf
  [sseq 6
   [420, srand 3 fs, 420, 2200, srand 1 fs]
  ,swhite 16 20 12000]
  where
    fs = [440, 880, 1760, 3520, 7040, 14080]

-- s3 =
--   sseq sinf
--   [sseq 4
--    [4400, srand 4 fs, 4400, 2200, srand 1 fs]
--   ,sstutter (siwhite sinf 1 15)
--    (swhite 16 20 12000)]
--   where
--     fs = [440, 880, 1760, 3520, 7040, 14080]

mkDemand :: UGen -> UGen -> Supply -> UGen
mkDemand tick rst sup =
  demand tick rst (evalSupply sup (mkStdGen 0x8898faab)) * tick

r = withSC3 reset

quickNoise :: UGen

-- quickNoise = quickNoise' ("t_trig"@@1) ("dur"@@1) ("atk"@@0.9) ("freq"@@6600)

quickNoise =
  quickNoise'
  hit
  0.125
  -- (linLin (lfdNoise1 'd' kr (1/10.32)) (-1) 1 0.125 0.25)
  -- 1e-3
  (linLin (sinOsc kr (1/12.123) 0) (-1) 1 1e-3 999e-3 `lag` 0.8)
  -- (lfCub kr (1/12.119) 0 * 0.4 + 0.5) --
  -- (clip2 (lfdNoise0 'a' kr (1/13)) 1)
  fp
  -- (lfdNoise3 'f' kr (1/3.389898989) * 3000 + 3200) where
  (linLin (sinOsc kr (1/13.321) 0) (-1) 1 (-10) 10)
  where
    hit = mkDemand tick 0 s1
    fp = demand tick 0 (evalSupply s3 g0)
    -- tick = impulse kr (2 * 280/60) 0
    tick = impulse kr ttempo 0
    g0 = mkStdGen 0

quickNoise' tick dur atk freq en = out ("out"@@0) (sig * aenv * ("amp"@@1.2)) where
  -- sig = rlpf nz (mce [freq',freq'*1.01]) rq
  sig = resonz nz (mce [freq',freq'*1.1]) rq
  -- nz = brownNoise 'b' ar
  nz = whiteNoise 'w' ar
  -- nz = henonC ar (sampleRate/4) 1.4 0.3 (-0.1) 0.1
  -- nz = pinkNoise 'p' ar
  -- freq' = sinOsc  kr (1/17) 0 * lfreq * 0.5 + (lfreq*0.6)
  freq' = lfreq
  lfreq = latch freq tick
  -- freq' = freq
  aenv = envGen kr tick tamp 0 dur DoNothing $
         env [0,1,0,0] [atk,1-atk] [EnvNum en] 0 2
  -- rq = (sinOsc kr (1/19) 0 * 0.495 + 0.5)
  rq = linLin (lfdNoise1 'r' kr (1/5.12)) (-1) 1 0.1 0.9
  -- rq = 0.3
  tamp = latch tick tick

-- | Run this UGen and press left mouse button.
gateWithMouse :: UGen
gateWithMouse = out 0 $ sig where
  sig = sig' * mouseButton kr 0 1 0.01
  sig' = sinOsc ar (f * mce [1,1.01]) 0 * aenv * 0.3
  f = mouseX kr 100 10000 Exponential 0.1
  aenv = envGen kr tick 1 0 1 DoNothing $
         env [0, 1, 1, 0] [2e-3, 5e-3, 2e-3] [EnvNum 3] (-1) (-1) where
  tick = dust 'd' kr df
  df = mouseY kr 1 100 Exponential 0.1

nmix :: UGen
nmix = out ("out"@@0) sig where
  -- input = "a_in"@@0
  -- input = sinOsc ar 440 0 * 0.2
  input = whiteNoise 'w' ar * 0.2
  sig = pan2 flt ("pan"@@0) 1
  flt = input
  -- flt = sum [bBandPass input ("lf"@@80) lbw
  --           ,bBandPass input ("mf"@@1200) mbw
  --           ,bBandPass input ("hf"@@8000) hbw]
  -- lbw = mouseX kr 0 10 Linear 0.2
  -- mbw = sinOsc kr (1/5.12) 0 * 5 + 5
  -- hbw = mouseY kr 0 10 Linear 0.2

bosc :: UGen
bosc = bosc' gt (freq `lag` 0.125) amps where
  -- hit = mouseButton kr 0 1 5e-3
  -- gt = gval * gtick
  -- gval = demand gtick 0 (evalSupply phit (mkStdGen 0))
  -- gtick = pulseDivider tick 3 0
  gt = toggleFF gt'
  gt' = coinGate 'g' prob tick
  amps = tExpRand 'a' 0.25 0.4 gt'
  ftick = coinGate 'f' prob tick
  prob = 0.85 -- linLin (lfClipNoise 'c' kr 1) (-1) 1 1e-3 1
  -- prob = linLin (lfClipNoise 'p' kr (1/12)) (-1) 1 (1/32) (31/32)
  tick = impulse kr 6 0
  -- freq = midiCPS 45
  freq = midiCPS $ demand ftick 0 $ evalSupply pat (mkStdGen 0)
  pat = sseq sinf
        [srand 8 (map (+ 31) penta)
        ,srand 8 (map (+ 43) penta)]
  penta = [0, 3, 5, 7, 10, 12]

bosc' tick freq amp = out ("out"@@0) sig where
  sig = clip2 (rlpf sig' filtf filtq) 1 * (latch amp tick)
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
  -- nh = envGen kr tick 1 1 80 DoNothing $
  --      env [0,1,0] [1e-3,1350e-3] [EnvLin] (-1) (-1)
  nh = linLin (lfdNoise3 'n' kr 1) (-1) 1 0 1
  tick' = latch tick tick
  -- nh = decay2 tick 1e-3 50e-3 * 20
   -- nh = 1
  filtf = 8800
  filtq = 0.8

{-|
What we want to do is, make a signal with gates from pattern.

The syntax is:

* 0 < x means hit. when it's in sequence will make sequential hits.
* x < 0 means sustain. Do not release the gate but hold the current value
* x == 0 means rest. Release the node if it's sustaining, or has been hit.

Sample input will be:

> sseq sinf [1,-1,1,1, 0,1,1,0]

This pattern should be converted to:

> hit, hold, hit, hit, release, hit, hit, release

-}
go1 :: IO ()
go1 = audition $ testGate $ gateMe $ impulse kr 4 0

gateMe :: UGen -> UGen
gateMe tick = sig where
  sig = sig'
  sig' = demand tick 0 $ supply0 patG

-- patG =
--   sseq sinf [1, -1,   1,  1,  0,  1,  1 ,0
--             ,1,  -1,  0,  1,  0,  0,  1, 0]

-- patG =
--   sseq sinf [1, -1, -1, 1, 1, 0, 1, srand 1 [-1,0,1]
--             -- ,1, -1,  1,-1, 1, srand 1 [-1,0,1],  1, srand 1 [-1,0,1]]
--             ,1, -1, -1,-1, -1, srand 1 [-1,0,1],  1, srand 1 [-1,0,1]]

patG =
  -- sseq sinf
  -- [sseq 3 [1,0.5,srand 1 [0,-1], 0.8,0.4,srand 1 [0,-1]
  --         ,0.6,0.4,srand 1 [0,-1], 0.7,0.4,srand 1 [0,-1]
  --         ,0.8,srand 1 [0,-1,0.7],0.8,srand 1 [0,-1,0.4]]
  -- ,srand 1
  --  [swhite 16 0.3 1
  --  ,sseq 4 [1.0,0,0.8,srand 1 [0,-1]]
  --  ,sseq 2 [sseq 2 [1,0,0.3], sseq 1 [1,-1]]]
  -- ,sseq 3 [1,0.5,srand 1 [0,-1], 0.8,0.4,srand 1 [0,-1]
  --         ,0.6,0.4,srand 1 [0,-1], 0.7,0.4,srand 1 [0,-1]
  --         ,0.8,srand 1 [0,-1,0.7],0.8,srand 1 [0,-1,0.4]]
  -- ,swhite 16 0.3 1]
  sseq sinf 
  [sseq 3 [base]
  ,srand 1
   [swhite 16 0.3 1
   ,sseq 4 [1.0,0,0.8,srand 1 [0,-1]]
   ,sseq 2 [sseq 2 [1,0.8,srand 1 [0,-1]], 1, srand 1 [0,-1]]]
  ,sseq 3 [base]
  ,swhite 16 0.3 1]
  where
    base = 
      sseq 1 [1,0.5,srand 1 [0,-1], 0.8,0.4,srand 1 [0,-1]
             ,0.6,0.4,srand 1 [0,-1], 0.7,0.4,srand 1 [0,-1]
             ,0.8,srand 1 [0,-1,0.7],0.8,srand 1 [0,-1,0.4]]

-- patG =
--   sseq sinf [1,1,srand 1 [0,-1], 1,1,srand 1 [0,-1]
--             ,1,1,srand 1 [0,-1], 1,1,srand 1 [0,-1],
--              1,srand 1 [0,-1,1],1,srand 1 [0,-1,1]]

ttempo = 2*290/60

{-|
Above "go" was not working as expected. How's demandEnvGen?

Made two modifications:

* Do not pass the signal when it's negative.

* Multiple the signal with '1 - signum tick', so that beginning sample
  of each sample will be 0 then signnum tick is 1.

Can we use this method for holding values use for frequencies?

-}
go2 :: IO ()
go2 = audition $ testGate $ gateMe2 $ impulse kr 4 0

gateMe2 :: UGen -> UGen
gateMe2 tick = sig where
  sig = gate (sig' * (1 - (tick >* 0))) (sig' >=* 0)
  sig' = demand tick 0 $ supply0 patG

testGate :: UGen -> UGen
testGate g =
  out ("out"@@0) $ sinOsc ar (mce [440,440.32]) 0 * 0.3 * e where
    e = envGen kr g 1 0 1 DoNothing $
        env [0,1,0.1,0.3,0] [200e-3,10e-3,10e-3,30e-3] [EnvSin] 3 (-1)

qngo = audition quickNoise >> go3
bngo = audition quickNoise >> goB

{-|
Holding frequency.

Using two demand ugens to play two patterns.

How it will be when we've write same pattern using duty and/or tduty?
-}
go3 :: IO ()
go3 = audition $ testGate2 (gateMe2 t) (gateMe3 t) where
  t = impulse kr ttempo 0

goB = do
  let t = impulse kr ttempo 0
      p1 = gateMe3 t / 5
  audition $ bosc' (gateMe2 t) p1 0.1

gateMe3 :: UGen -> UGen
gateMe3 tick = midiCPS sig where
  sig = gate sig' (sig' >* 0)
  sig' = demand tick 0 $ supply0 patF

-- patF =
--   sseq sinf [67, -1, -1, 62, -1, -1, 60, 65
--             ,67, -1, -1, 62, -1, -1, 60, -1]

-- patF =
--   sseq sinf [67, -1, -1, srand 1 [60,62,65,67], -1, -1, 60, 65
--             ,67, -1, -1, 62, -1, -1, srand 2 [-1,-1,60,62,65,67]]
-- patF = srand sinf [-1,-1,-1,60,63,65,67,70,72]

-- patF = sseq sinf [sstutter 16 (sseq sinf [60,63,58,65])]

-- patF =
--   sseq sinf
--   [sseq 64 [64], sseq 64 [59]
--   ,sseq 64 [64], sseq 32 [59]
--   ,sseq 16 [62], sseq 8 [64], sseq 8 [66]]

patF =
  sseq sinf
  [sseq 1 [sseq 16 [52], srand 16 [52,64]
          ,srand 16 [52,64,76], srand 16 [52,64]
          ,sseq 16 [52], srand 16 [52,64]
          ,srand 16 [52,64,76], sseq 2 base]
  ,sseq 3 [sseq 48 base
          ,srand 128 base]]
  where
    base = [52,57,59,64, 69,71,76,78]

-- patF =
--   sseq sinf
--   [sseq 128 [64]
--   ,sseq 128 [59]
--   ,sseq 128 [64]
--   ,sseq 64 [59]
--   ,sseq 32 [62]
--   ,sseq 16 [64]
--   ,sseq 16 [66]]

testGate2 :: UGen -> UGen -> UGen
testGate2 g freq =
  out ("out"@@0) $ sinOsc ar (mce [freq,freq*1.01]) 0 * 0.3 * e where
    e = envGen kr g 1 0 1 DoNothing $
        env [0,1,0.1,0.3,0] [200e-3,10e-3,10e-3,30e-3] [EnvSin] 3 (-1)

-- | Keyboard to frequency converter.
-- 'z' is midi note 48, 'x' is midi note 50, and so on.
pitchKeys :: UGen
pitchKeys = sig where
  sig = sum $ catMaybes $ map f "zxcvbnm,./asdfghjkl;'"
  f x = lookup x fmapping >>= \freq ->
        lookup x cmapping >>= \keycode ->
        return $ freq * keyState kr keycode 0 1 0.1
  -- fmapping = zipWith (\ch nt -> (ch,midiCPS nt)) "azsxdcfvgbhnjmk,l.;/'" [59..]
  fmapping = zipWith (\c n -> (c,midiCPS n)) "azsxdcvgbhnjm,l.;/" [59..]
  cmapping = zip "asdfghjkl;'" [38..48] ++ zip "zxcvbnm,./" [52..61]
  lookupSig c = lookup c $ zip "azsxdcfvgbhnjmk,l.;/'" [59..]

-- | Triggering with mouse button.
goMouse :: IO ()
goMouse = audition $ testGate $ mouseButton kr 0 1 1e-3

-- | Controlling demandEnvGen.
goD :: IO ()
goD = audition $ testGateD $ gateMe $ impulse kr 4 0

testGateD :: UGen -> UGen
testGateD g = out ("out"@@0) $ sinOsc ar (mce [440,439.999]) 0 * 0.3 * e where
  e = demandEnvGen kr levels times 1 1 g reset 1 0  1 DoNothing
  levels = supply0 $ sseq 1 [0,     1,     0.2,  0.2,      0]
  times  = supply0 $ sseq 1 [  1e-3,  2e-1,   0,    2e-1]
  reset = dust 'r' kr 1

supply0 :: Supply -> UGen
supply0 p = evalSupply p (mkStdGen 0)

pEx = audition $ out 0 sig  where
  sig =rlpf sig' f 0.05
  sig' = pulse ar (mce [100,250]) fw * e * 0.2
  f  = mouseY kr 100 8000 Exponential 0.2
  fw = mouseX kr 0 1 Linear 0.1
  e  = decay2 (dust 'e' kr 4) 1e-3 200e-3

cd2tkl :: UGen
cd2tkl = cd2tkl' ("t_trig"@@1)
cd2tkl' tick = out ("out"@@0) $ decay2 tick 1e-3 1.2 * sig * 0.2 where
  sig = foldr f sig' (map (\i -> rand i 0.001 0.05) "abwewpoew")
  f a b = allpassN b 0.05 a 4
  sig' = ringz nz freq rt
  freq = tExpRand 'f' 1020 12800 tick
  nz = pinkNoise 'a' ar
  rt = mouseX kr 0.04 4 Linear 0.1
