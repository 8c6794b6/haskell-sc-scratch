------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook07
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook06_Phrase_Network/.
--
-- 

module SCHelp.PG.Cookbook07 where

import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCSched
import SCTree
import SCQuery
import qualified Scratch.ControlArgs as A

runRhythmicVariations :: IO ()
runRhythmicVariations = undefined

setRhythmicVariations :: IO OSC
setRhythmicVariations = withSC3 $ \fd -> do
  loadSynthdef "kik" kik fd
  kraftySnr >>= \ug -> loadSynthdef "kraftySnr" ug fd

-- | Synth for kick sound.
kik :: UGen
kik = out A.out $ mce [sig, sig]
    where
      sig = distort (sinOsc ar fcurve (0.5 * pi) * preamp) * e * 
            A.amp {controlDefault = 1}
      e = envGen kr 1 1 0 1 RemoveSynth $
          envCoord [(0,decay1), (decay1L,decay2), (0,0)] 1 1 EnvCub
      fcurve = envGen kr 1 1 0 1 DoNothing $ 
               envCoord [(0, basefreq * ratio), (sweeptime, basefreq)]
                            1 1 EnvCub
      preamp = A.preamp {controlDefault=1}
      decay1 = "decay1" @= 0.3
      decay2 = "decay2" @= 0.15
      decay1L = "decay1L" @= 0.8
      basefreq = "basefreq" @= 50
      ratio = "ratio" @= 7
      sweeptime = "sweeptime" @= 0.05

      
-- | Synth for snare and hihat sound.
kraftySnr :: IO UGen
kraftySnr = do
  sig <- pinkNoise ar >>. (* A.amp)
  let env = envGen kr 1 1 0 1 RemoveSynth $
            envPerc 0.01 (A.decay {controlDefault=0.3})
      sig' = bpf sig (A.freq {controlDefault=2000}) 
             (A.rq {controlDefault=3}) * env
  return $ out A.out (pan2 sig' A.pan 1)





