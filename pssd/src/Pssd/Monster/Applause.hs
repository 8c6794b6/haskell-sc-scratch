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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_applause.html>
--
-- Try:
--
-- > > withSC3 playClap
--
-- /TODO/:
--
-- * Add fading control to graph.
--
module Pssd.Monster.Applause where

import Control.Monad (forever)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Play clapping sound.
playClap :: (Transport t) => t -> IO ()
playClap fd = do
  let sineEnv = out outBus (val * amp)
      outBus = ctrl "out" 100
      dur = ctrl "dur" 20
      amp = ctrl "amp" 1
      grad = ctrl "grad" 1
      val = clip2 ((sin (line kr (-0.5*pi) (1.5*pi) dur RemoveSynth) * 0.5 + 0.5) * grad) 1
  mapM_ (adrcv fd) [("trigClap",trigClap 'a')
                   ,("clap1",clap1)
                   ,("clap2",clap2)
                   ,("applause",applause)
                   ,("sineEnv",sineEnv)
                   ,("master",master)]
  addNode 0 clappers fd

clappers :: SCNode
clappers =
  Group 0
    [Group 1
      [Group 18
         [Group 180
           [Synth 1800 "sineEnv"
             ["out":=183,"amp":=0.3,"dur":=15]
           ,Synth 1801 "sineEnv"
             ["out":=184,"amp":=0.8,"dur":=20,"grad":=2]
           ,Synth 1802 "sineEnv"
             ["out":=185,"amp":=4,"dur":=20]
           ,Synth 1803 "trigClap"
             ["out1":=180,"out2":=181,"out3":=182,"freq":<-185]]
         ,Group 181
           [Synth 1810 "clap1" ["t_trig":<-180,"amp":=0.8]
           ,Synth 1811 "clap1" ["t_trig":<-181,"amp":=0.8]
           ,Synth 1812 "clap1" ["t_trig":<-182,"amp":=0.8]
           ,Synth 1813 "applause" ["amp":<-183] ]
         ,Group 182
           [Synth 1820 "master" ["amp":<-184] ]]]]

-- | For controlling amplitude of whole clapping sound.
master :: UGen
master = replaceOut outBus input
  where
    input = in' 2 ar inBus * amp
    outBus = ctrl "out" 0
    inBus = ctrl "in" 0
    amp = ctrl "amp" 1

-- | Trigger for three clapping synthes.
trigClap :: (ID i) => i -> UGen
trigClap i = mrg [out out1 tr1, out out2 tr2, out out3 tr3]
  where
    out1 = ctrl "out1" 180
    out2 = ctrl "out2" 181
    out3 = ctrl "out3" 182
    tr1 = tr
    tr2 = tDelay tr (tRand 'a' 10e-3 800e-3 tr)
    tr3 = tDelay tr2 (tRand 'b' 10e-3 300e-3 tr)
    tr = impulse kr freq 0
    freq = ctrl "freq" 1

-- | Single hand clap.
clap1 :: UGen
clap1 = out2 sig
  where
    sig = sig1 * 0.7 * amp
    sig1 = bpf (sig2 * e2 + sig2) freq 0.5
    freq = tRand 't' 100 1900 t_trig + 800
    e2 = decay2 t_trig 3e-3 50e-3
    sig2 = squared $
           sum [lpf n1 8000
               ,bpf n2 1300 (1/100)
               ,bpf n2 2500 (1/40)
               ,bpf n2 700 (1/100) ] * ampEnv
    n1 = whiteNoise 'a' ar
    n2 = hpf n1 300
    ampEnv = squared (decay2 t_trig 1e-3 800e-3)
    t_trig = ctrl "t_trig" 1
    amp = ctrl "amp" 1

-- | Single hand clap, variant that removes itself when done.
clap2 :: UGen
clap2 = mrg [out2 sig, freeSelfWhenDone (linen t_trig 0 1 1 RemoveSynth)]
  where
    sig = sig1 * 0.7 * amp
    sig1 = bpf (sig2 * e2 + sig2) freq 0.5
    freq = tRand 't' 100 1900 t_trig + 800
    e2 = decay2 t_trig 3e-3 50e-3
    sig2 = squared $
           sum [lpf n1 8000
               ,bpf n2 1300 (1/100)
               ,bpf n2 2500 (1/40)
               ,bpf n2 700 (1/100) ] * ampEnv
    n1 = whiteNoise 'a' ar
    n2 = hpf n1 300
    ampEnv = squared (decay2 t_trig 1e-3 800e-3)
    t_trig = ctrl "t_trig" 1
    amp = ctrl "amp" 1

-- | Attempt to send s_new message for clap2 synth as response of sendTrig ugen.
newClaps :: (Transport t)
         => UGen    -- ^ Frequency for dust
         -> Double  -- ^ Amplitude
         -> t       -- ^ Connection
         -> IO ()
newClaps freq amp fd = do
  async fd $ notify True
  play fd $ sendTrig (dust 'd' kr freq)1 1
  forever aClap
  where
    aClap = do
      tr@(Message "/tr" [_,Int i,_]) <- wait fd "/tr"
      send fd $ s_new "clap2" (-1) AddToTail 1 [("amp",amp)]

-- | Texture sound for clapping mass.
--
-- This UGen is using dust and decay2 for amplitude envelope of white noise.
--
applause :: UGen
applause = out2 sig
  where
    sig = clip2 (lpf (hpf sig1 1200) 4000) 1 * amp
    sig1 = (n1*dcy1) + (n1*(dcy1+dcy2)*0.25)
    n1 = cubed n0
    n0 = whiteNoise 'a' ar * level
    freq = lfNoise2 'f' kr density * grain
    dcy1 = decay2 trg 1e-3 (tRand 'b' 1e-3 (1/grain) trg)
    dcy2 = decay2 trg 1e-3 (tRand 'c' 250e-3 800e-3 trg)
    trg = dust 'd' kr density
    density = ctrl "density" 101.7
    grain = ctrl "grain" 20
    level = ctrl "level" 1
    amp = ctrl "amp" 1

-- | Whistler, take 1.
whistler1 :: UGen
whistler1 = out2 sig
  where
    sig = hpf (lpf oscil 8000) 1000 * amp
    oscil = sinOsc ar freq 0 * ampEnv
    ampEnv = sin (linen tr atk pi dcy DoNothing)
    atk = tRand 'a' 10e-3 100e-3 tr
    dcy = tRand 'd' 200e-3 800e-3 tr
    tr = dust 'd' kr 0.5
    freq = abs (lfNoise2 'n' kr 0.5) * 1e4 + 200
    n = lfNoise2 'n' kr 0.75
    amp = ctrl "amp" 0.2