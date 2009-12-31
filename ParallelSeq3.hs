----------------------------------------------------------------------
-- | Parallel sequence of Note, third try.
-- This time, sending trigger to node with using busses and buffers.
-- 

module ParralelSeq3 where

import Sound.SC3
import Sound.OpenSoundControl
import WritingSynthDef

-- | Set groups of scsynth.
initGroups :: IO ()
initGroups = undefined

-- | UGen which makes sound.
paraUGen :: UGen
paraUGen = out 0 (pan2 osc pos 1) where
    pos = Control KR "pos" 0.0
    osc = sinOsc AR freq 0 * dbAmp amp
    freq = Control KR "freq" 440
    amp = Control KR "amp" (-20)

-- | UGen which controls the paraUGen.
playerUGen :: UGen
playerUGen = undefined

type SendUDP a = UDP -> IO a

-- | @flip send@. With flipping the argument, one can write as below:
-- > withSC3 (send' some_osc_message)
send' :: OSC -> SendUDP ()
send' = flip send

-- | @flip async@.
async' :: OSC -> SendUDP OSC
async' = flip async