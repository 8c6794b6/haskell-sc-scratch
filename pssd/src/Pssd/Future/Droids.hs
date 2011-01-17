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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_droids.html>
--
-- Try:
--
-- > > withSC3 playDroid
--
module Pssd.Future.Droids where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Random ramp generator used for droid sound.
generator :: UGen -- ^ Trigger
          -> UGen -- ^ Max Target value
          -> UGen -- ^ Lag period, in second
          -> UGen -- ^ Probability
          -> UGen
generator t target period prob = lag newVal dur
  where
    val = lag newVal dur
    newVal = tRand 'r' 0 target (coinGate 'c' prob t)
    dur = (tiRand 't' 0 1 t * period) + 5e-3
    outBus = ctrl "out" 100

-- | Testing generator with sinOsc.
genOsc :: UGen
genOsc = out2 sig
  where
    sig = sinOsc ar (gen+80) 0 * 0.3
    gen = generator (impulse kr 4 1) 1200 1 0.5

-- | Good old fm droid sound, from Droids 2 patch.
drdSound :: UGen -- ^ Amplitude
         -> UGen -- ^ Carrier amp
         -> UGen -- ^ Carrier frequency
         -> UGen -- ^ Modulator frequency
         -> UGen -- ^ Modulator index
         -> UGen
drdSound amp camp cf mf mindex = out2 (sig * amp * 0.3)
  where
    sig = filt $ cos (2 * pi * sig0)
    filt ug = hpf (hpf (lpf (lpf ug 10000) 10000) 100) 100
    sig0 = car + mod
    car = camp * lfSaw ar cf 0
    mod = sinOsc ar (cf * mf) 0 * mindex

-- | The droid sound.
droid :: UGen
droid = drdSound amp camp cf mf mindex
  where
    amp = envGen kr (coinGate 'z' 0.38 trg) 1 0 period DoNothing $
          env [0,1,1,0] [0.01,0.98,0.01] [EnvNum (-4)] (-1) (-1)
    camp = generator trg 5500 period 0.5 + 100
    cf = (generator trg 1000 period 0.3 / 1000) * 0.6
    mf = generator trg 1200 period 0.25 + 20
    mindex = (generator trg 200 period 0.5 / 200) + 0.2
    trg = ctrl "t_trig" 1
    period = ctrl "period" 0.2

-- | Trigger for droid.
hitDroid :: UGen
hitDroid = out outBus t
  where
    outBus = ctrl "out" 140
    t = impulse kr freq 1
    freq = ctrl "freq" 1

-- | Play droid sound
playDroid :: (Transport t) => t -> IO ()
playDroid fd = do
  let dr (n,u) = async fd $ d_recv $ synthdef n u
  mapM_ dr [("droid",droid),("hitDroid",hitDroid)]
  mkTree droidGraph fd

-- | Graph for playing droid sound.
droidGraph :: SCNode
droidGraph =
  Group 0
    [ Group 1
      [ Group 14
        [ Group 140
          [ Synth 1400 "hitDroid" ["out":=tB,"freq":=6]]
        , Group 141
          [ Synth 1410 "droid" ["t_trig":<-tB,"period":=0.25] ]]]]
  where
    tB :: (Num a) => a
    tB = 141