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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_footsteps.html>
--
-- Try:
--
-- > > withSC3 playFootSteps
--
-- Mouse x changes walk speed, y changed texture.
--
module Pssd.Monster.FootSteps where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Sends synthdefs, make synths for foot step sounds.
playFootSteps :: (Transport t) => t -> IO ()
playFootSteps fd = do
  let i = in' 2 ar 0
  mapM_ (adrcv fd) [("footStep1",footStep1)
                   ,("footStep2",footStep2)
                   ,("footStep3",footStep3)
                   ,("stepping",stepping)
                   ,("stepSpeed",stepSpeed)
                   ,("selectTexture",selectTexture)
                   ,("dirt",dirt i)
                   ,("grass", grass i)
                   ,("snow", snow i)
                   ,("gravel", gravel i)
                   ,("wood",wood i)]
  mkTree footStepGraph fd

footStepGraph :: SCNode
footStepGraph =
  Group 0
    [Group 1
      [Group 17
        [Group 170
          [Synth 1700 "stepSpeed"
            ["out":=pB,"lo":=0,"hi":=6]
          ,Synth 1701 "stepping"
            ["out":=sB,"freq":<-pB]
          ,Synth 1702 "selectTexture"
            ["dirtNode":=1720
            ,"grassNode":=1721
            ,"snowNode":=1722
            ,"gravelNode":=1723
            ,"woodNode":=1724]]
        ,Group 171
          [Synth 1710 "footStep3"
            ["t_trig":<-sB,"dur":<-dB]]
        ,Group 172
          [Synth 1720 "dirt" ["out":=0,"in":=0,"amp":=5]
          ,Synth 1721 "grass" ["out":=0,"in":=0,"amp":=5]
          ,Synth 1722 "snow" ["out":=0,"in":=0,"amp":=1]
          ,Synth 1723 "gravel" ["out":=0,"in":=0,"amp":=1]
          ,Synth 1724 "wood" ["out":=0,"in":=0,"amp":=1]]]]]
  where
    sB, dB, pB :: (Num a) => a
    sB = 160
    dB = 161
    pB = 162

-- | Controller for speed of steps.
stepSpeed :: UGen
stepSpeed = out outBus (mouseX kr lo hi Linear 0.1)
  where
    outBus = ctrl "out" 160
    lo = ctrl "lo" 0
    hi = ctrl "hi" 8

-- | Trigger for steps.
stepping :: UGen
stepping = mrg [out trigBus (impulse kr freq 1)
               ,out durBus dur]
  where
    freq = ctrl "freq" 2
    dur = clip2 (0.25 / freq) 1
    trigBus = ctrl "outBus" 160
    durBus = ctrl "durBus" 161

-- | Texture selecter. Using mouse y to control the texture.
selectTexture :: UGen
selectTexture = mrg [isDirt, isGrass, isSnow, isGravel,isWood]
  where
    isDirt = pause (0 <=* txt * txt <=* 1) dirtNode
    isGrass = pause (1 <=* txt * txt <=* 2) grassNode
    isSnow = pause (2 <=* txt * txt <=* 3) snowNode
    isGravel = pause (3 <=* txt * txt <=* 4) gravelNode
    isWood = pause (4 <=* txt * txt <=* 5) woodNode
    txt = mouseY kr 0 5 Linear 1
    dirtNode = ctrl "dirtNode" 1720
    grassNode = ctrl "grassNode" 1721
    snowNode = ctrl "snowNode" 1722
    gravelNode = ctrl "gravelNode" 1723
    woodNode = ctrl "woodNode" 1724

-- | Foot step, take 1.
footStep1 :: UGen
footStep1 = out2 sig
  where
    sig = lpf (sig1 * ampEnv) 100
    sig1 = whiteNoise 'a' ar
    ampEnv = sin (sinEnv * pi)
    sinEnv = envGen kr t_trig 1 0 dur DoNothing shape
    shape = env [0,0,1] [0,1] [EnvLin] (-1) 0
    dur = ctrl "dur" 50e-3
    t_trig = ctrl "t_trig" 1

-- | Foot step, take 2.
footStep2 :: UGen
footStep2 = out2 sig
  where
    sig = lpf (sig1 * ampEnv) 100
    sig1 = clip (bpf (whiteNoise 'a' ar) 100 (1/2) * 10) (-0.5) 1
    ampEnv = squared (sin (env1 * pi)) + squared (sin (env2 * pi))
    env1 = envGen kr t_trig 1 0 dur' DoNothing shape1
    shape1 = env [0,0,1] [0,1] [EnvLin] (-1) 0
    env2 = envGen kr (tDelay t_trig dur) 1 0 dur' DoNothing shape2
    shape2 = env  [0,0,1] [0,1] [EnvLin] (-1) 0
    t_trig = ctrl "t_trig" 1
    dur' = dur / 2
    dur = ctrl "dur" 200e-3

-- | Foot step, take 3.
footStep3 :: UGen
footStep3 = out2 sig
  where
    sig = lpf (sig1 * ampEnv) 100 * amp
    sig1 = clip (bpf (whiteNoise 'a' ar) 100 (1/2) * 10) (-0.5) 1
    ampEnv = sum [squared (sin (heel * pi))
                 ,squared (sin (ball * pi))
                 ,sin (outStep * pi) ]
    heel = envGen kr t_trig 1 0 dur' DoNothing heelShape
    heelShape = env [0,0,1] [0,1] [EnvLin] (-1) 0
    ball = envGen kr (tDelay t_trig dur) 1 0 dur' DoNothing ballShape
    ballShape = env  [0,0,1] [0,1] [EnvLin] (-1) 0
    outStep = envGen kr t_trig 1 0 dur DoNothing outStepShape
    outStepShape = env  [0,0,1] [0,1] [EnvLin] (-1) 0
    t_trig = ctrl "t_trig" 1
    amp = ctrl "amp" 1
    dur' = dur / 4
    dur = ctrl "dur" 200e-3

-- | Dirt texture
dirt :: UGen -- ^ Input sound
     -> UGen
dirt input = replaceOut outBus sig
  where
    sig = (noise + oscil) * input * amp
    oscil = ci * sinOsc ar (ci * 500 + 40) 0 * 0.5
    ci = cubed input
    noise = clip2 (hpf (sinOsc ar noiseFreq 0) 200) 1 * 0.04
    noiseFreq = (lpf (whiteNoise 'b' ar) 80 * 70 * input) * 70 + 70
    outBus = ctrl "out" 0
    amp = ctrl "amp" 1

-- | Grass texture.
grass :: UGen -- ^ Input sound
      -> UGen
grass input = replaceOut outBus sig
  where
    sig = (noise + oscil) * amp
    noise = hpf nz1 900 * 0.3 * input
    nz1 = bpf nz2 nz3 (0.3)
    nz2 = clip2 (cubed (hpf (lpf nz0 300 / lpf nz0 200) 2500) * 1e-5) 0.9
    nz3 = clip ((lpf nz0 16 * 23800) + 3400) 2000 10000
    nz0 = whiteNoise 'c' ar
    oscil = clip (sinOsc ar (ci * 600 + 30) 0) 0 0.5 * ci * 0.8
    ci = cubed input
    outBus = ctrl "out" 0
    amp = ctrl "amp" 1

-- | Snow texture.
snow :: UGen -- ^ Input sound
     -> UGen
snow input = replaceOut outBus sig
  where
    sig = clip2 noise 0.9 * input * amp
    noise = bpf sig1 (input * 9000 + 700) 0.2
    sig1 = hpf (clip2 n1 1) 300
    n1 = n2 * n3 * n4
    n2 = lpf n20 110 / lpf n20 900
    n20 = whiteNoise 'e' ar
    n3 = lpf n30 50 / lpf n30 70
    n4 = cubed (lpf n30 10 * 17) * 0.5
    n30 = whiteNoise 'f' ar
    outBus = ctrl "out" 0
    amp = ctrl "amp" 1

-- | Gravel texture.
gravel :: UGen -- ^ Input sound
       -> UGen
gravel input = replaceOut outBus sig
  where
    sig = input * sig1 * amp
    sig1 = hpf (bpf nz1 freq (1/3)) 200 * 2
    nz1 = clip2 nz2 0.9
    nz2 = squared (hpf (lpf wn 300 / lpf wn 200) 400) * 0.01
    wn = whiteNoise 'a' ar
    freq = clip (lpf wn 50 * 50000 + input * 1000) 500 10000
    outBus = ctrl "out" 0
    amp = ctrl "amp" 1

-- | wood texture.
wood :: UGen -- ^ Input sound
     -> UGen
wood input = replaceOut outBus sig
  where
    sig = (n1 * 0.5 + n2 * 0.6) * amp
    n1 = sum [bpf wn1 95 (1/9)
             ,bpf wn1 134 (1/9)
             ,bpf wn1 139 (1/9)
             ,bpf wn1 154 (1/9)] *
         sqrt input * 6
    n2 = sum [bpf wn2 201 (1/7)
             ,bpf wn2 123 (1/2)
             ,bpf wn2 156 (1/9)
             ,bpf wn2 180 (1/9)] *
         squared input * 16
    wn1 = whiteNoise 'a' ar
    wn2 = whiteNoise 'b' ar
    outBus = ctrl "out" 0
    amp = ctrl "amp" 1