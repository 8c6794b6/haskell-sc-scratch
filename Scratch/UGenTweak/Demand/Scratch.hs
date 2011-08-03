{- |
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable
-}
module Demand.Scratch where

import System.Random

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

r :: IO ()
r = withSC3 reset

-- | Evaluate Supply with using stdgen made by 'mkStdGen 0'.
supply0 :: Supply -> UGen
supply0 = flip evalSupply (mkStdGen 0)

-- | Using DemandEnvGen, take 1.
deg01 :: UGen
deg01 = out 0 sig where
  sig = sinOsc ar f 0 * 0.1
  f = demandEnvGen kr level01 time01 shp 0 1 1 1 0 1 DoNothing
  shp = 2

level01 :: UGen
level01  = supply0 $ sseq sinf [440,9600]

time01 :: UGen
time01 = mouseY kr 1e-2 3 Exponential 1e-1

-- | Compare with above. This uses same level and time, but runs with duty.
dut01 :: UGen
dut01 = out 0 $ sinOsc ar f 0 * 0.1 where
  f = duty ar time01 0 DoNothing level01 `lag` 1e-3

-- | Second example from hsc3 help file of demandEnvGen.
deg02 :: UGen
deg02 = out 0 $ sinOsc ar (f * mce [1,1.01]) 0 * 0.1 where
  f = demandEnvGen ar level02 (time02*y) 7 0 1 1 1 0 1 DoNothing
  y = mouseY kr 1e-3 3 Exponential 1e-1

level02 = supply0 $ sseq sinf [204,400,201,502,300,200]
time02 = supply0 $ srand sinf [1.01,0.2,0.1,2.0]

-- | Compare with deg02.
tdut02 :: UGen
tdut02 = out 0 $ sinOsc ar (f * mce [1,1.01]) 0 * 0.1 where
  f = duty ar (time02 * y) 0 DoNothing level02 `lag` 1e-1
  y = mouseY kr 1e-3 3 Exponential 1e-1

deg03 :: UGen
deg03 = out 0 $ sinOsc ar f 0 * 0.1 where
  f = demandEnvGen ar (vs * mce [1,1.001]) (ts * y) 7 0 1 1 1 0 1 DoNothing
  vs = supply0 $ sseq sinf (map sval rs)
  ts = sampleDur
  rs = map (\x -> rand x 200 1000) [(1::Int)..32]
  y = mouseY kr 1 3000 Linear 1

-- | Using mouseX to hold frequency.
deg04 :: UGen
deg04 = out 0 $ sinOsc ar (freq * mce [1, 1.21]) 0 * 0.1 where
  freq = demandEnvGen kr vs 0.1 5 0.3 gt 1 1 0 1 DoNothing
  vs = supply0 $ srand sinf $ map sval [300,400..1000]
  gt = (mouseX kr 0 1 Linear 0.1 >* 0.5) + 0.1

deg05 :: UGen
deg05 = out 0 $ sinOsc ar (freq * mce [1,1.21]) 0 * 0.1 where
  freq = demandEnvGen kr vs 0.1 shp crv gt rst 1 0 1 DoNothing
  vs = supply0 $ sseq 2 [sseries 5 400 200, 800, 530, 4000, 900]
  shp = 3
  crv = 0
  gt = (mouseX kr 0 1 Linear 0.1 >* 0.5) - 0.1
  rst = (mouseButton kr 0 1 0.1 >* 0.5) * 2

mkDeg01 deg = out 0 $ sinOsc ar (deg * mce [1,1.01]) 0 * 0.1

deg06 :: UGen
deg06 =
  mkDeg01 $ demandEnvGen kr vs 0.2 shp crv gt rst ls lo ts RemoveSynth
  where
    shp = 1
    crv = 0
    gt = 1
    rst = 0
    ls = 1
    lo = 0
    ts = 1
    vs = supply0 $ sseq 1 [1300,500,800,300,400]

deg07 :: UGen
deg07 =
  mkDeg01 $ demandEnvGen kr vs 0.2 shp crv gt rst ls lo ts RemoveSynth
  where
    shp = 0
    crv = 0
    gt = 1
    rst = 0
    ls = 1
    lo = 0
    ts = 1
    vs = supply0 $ sseq 1 [1300,500,800,300,400]

deg08 = mkDeg01 deg where
  deg = demandEnvGen kr vs 0.03 1 0 gt rst 1 0 1 DoNothing
  vs = supply0 $ sseq sinf [500,800,600]
  gt = toggleFF (dust 'd' kr 5) + 0.1
  rst = 1

deg10 = out 0 sig where
  sig = demandEnvGen ar vs ts shp 0 gt rst 1 0 1 DoNothing
  vs = supply0 $ sseq sinf [sseries 20 (-0.1) 0.01
                           ,sseries 20 (-0.1) 0.01
                           ,sseries 5 (-0.1) 0.1]
  ts = sampleDur * mouseY kr 1 100 Linear 0.1
  shp = 1
  gt = impulse ar (mouseX kr 1
                   (sampleRate * mouseX kr 2e-4 1 Linear 0.1) Linear 0.1) 0
  rst = 0

-- donce
-- dfsm, is as of sc-3.4, in extra-plugins.

-- | Compare these:
--
-- >>> audition $ dswEx01 dswitch
-- >>> audition $ dswEx01 dswitch1
--
dswEx01 :: (Char -> UGen -> UGen -> UGen) -> UGen
dswEx01 dmn = out 0 o where
  o = sinOsc ar f 0 * 0.1
  f = demand t 0 d * 300 + 400
  d = dmn 'd' i $ mce [a0,a1,a2]
  t = impulse kr 4 0
  a0 = dwhite 'a' 2 3 4
  a1 = dwhite 'b' 2 0 1
  a2 = dseq 'c' 2 $ mce [1,1,1,0]
  i = dseq 'd' 2 $ mce [0,1,2,1,0]

-- | Try below and move mouse:
--
-- >>> audition $ dswEx02 dswitch
--
-- Compare with:
--
-- >>> auditon $ dswEx02 dswitch1
--
dswEx02 :: (Char -> UGen -> UGen -> UGen) -> UGen
dswEx02 ds = out 0 $ sinOsc ar f 0 * 0.1 where
  f = demand t 0 n * 30 + 340
  t = impulse kr 3 0
  n = ds 'x' x $ mce [1,3,y,2,w]
  w = dwhite 'w' dinf 20 23
  x = mouseX kr 0 4 Linear 0.1
  y = mouseY kr 1 15 Linear 0.1

-- | Left mouse button will reset the sequence.
dser01 :: UGen
dser01 = out 0 $ sinOsc ar f 0 * 0.1 where
  f = demand t r a * 30 + 340
  t = impulse kr x 0
  x = mouseX kr 1 40 Exponential 0.1
  a = dser 'a' 7 $ mce [1,3,2,7,8]
  r = mouseButton kr 0 1 1e-3

-- | Controlling demandEnvGen.
goD :: IO ()
goD = audition $ testGateD $ gateMe $ impulse kr 4 0

testGateD :: UGen -> UGen
testGateD g = out ("out"@@0) $ sinOsc ar (mce [440,439.999]) 0 * 0.3 * e where
  e = demandEnvGen kr levels times 1 1 g reset 1 0  1 DoNothing
  levels = supply0 $ sseq 1 [0,     1,     0.2,  0.2,      0]
  times  = supply0 $ sseq 1 [  1e-3,  2e-1,   0,    2e-1]
  reset = dust 'r' kr 1
