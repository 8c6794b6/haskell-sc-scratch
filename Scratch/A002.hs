------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with additive synth, take 2.
--
module A002 where

import Sound.OpenSoundControl (Transport)
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

go :: (Transport t) => t -> IO ()
go fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("k1",k1),("k2",k2),("k3",k3),("k4",k4),("aosc",aosc)]
  mkTree smallTree fd

smallTree :: SCTree
smallTree =
  Group 0
    [Group 1
       [Group 10
         [Synth 1000 "k2" ["out":=envB,"freq":=2,"amp":=0.35]
         ,Synth 1009 "k2" ["out":=dfmB,"freq":=0.5,"amp":=12]
         ,Synth 1001 "k1" ["out":=ampB,"freq":<-dfmB,"amp":=0.21]
         ,Synth 1002 "k3" ["out":=ampB,"freq":=2.8,"amp":<-envB]
         ,Synth 1003 "k4" ["out":=freqA,"freq":=(2.8/8),"factor":=1]
         ,Synth 1004 "k4" ["out":=freqB,"freq":=(2.8/8),"factor":=1.5]
         ,Synth 1005 "k4" ["out":=freqC,"freq":=(2.8/8),"factor":=2.5]]
       ,Group 20
         [Synth 1006 "aosc" ["amp":<-ampB,"freq":<-freqA,"pan":=(-0.3)]
         ,Synth 1007 "aosc" ["amp":<-ampB,"freq":<-freqB,"pan":=0]
         ,Synth 1008 "aosc" ["amp":<-ampB,"freq":<-freqC,"pan":=0.25]]]]
  where
    envB :: Num a => a
    envB = 1000
    ampB :: Num a => a
    ampB = 1001
    freqA :: Num a => a
    freqA = 1002
    freqB :: Num a => a
    freqB = 1003
    freqC :: Num a => a
    freqC = 1004
    dfmB :: Num a => a
    dfmB = 1004

-- | Single sin oscillator.
aosc :: UGen
aosc = out 0 (pan2 sig pan 1)
  where
    sig = sinOsc ar freq 0 * (lag2 amp 5e-3)
    pan = ctrl "pan" 0
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 440

k1 :: UGen
k1 = out outBus sig
  where
    sig = decay2 (dust 'd' kr freq) 5e-3 130e-3 * amp
    outBus = ctrl "out" 1000
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 1

k2 :: UGen
k2 = out outBus sig
  where
    sig = (lfdNoise3 'n' kr freq * 0.5 + 0.5) * amp
    outBus = ctrl "out" 1000
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 0.5

k3 :: UGen
k3 = out outBus sig
  where
    sig = decay2 (impulse kr freq 0) 5e-3 210e-3 * amp
    outBus = ctrl "out" 1000
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 1

k4 :: UGen
k4 = out outBus sig
  where
    sig = select idx (mce $ map ((*factor) . midiCPS) pitches)
    pitches = [53,55, 60,62,65,67, 72,74]
    idx = (lfdNoise0 'i' kr freq * 0.5 + 0.5) *
          (fromIntegral $ length pitches - 1)
    outBus = ctrl "out" 1000
    freq = ctrl "freq" (1/8)
    factor = ctrl "factor" 1
