------------------------------------------------------------------------------
-- |
--
-- Module to play with NoteOn and NoteOff message.
-- Problem is, not suited to play same note in quickly repeated
-- manner.
-- 
-- Could this behavior avoided by using state to hold the (key,value)
-- of node id and MIDI note number ... hmm, try it.  
-- 

module NoteOnOff where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Sound.Alsa.Sequencer
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing.MIDI

-- | Invoke this code and connect with MIDI keyboard.
main :: IO ()
main = withMIDI "NoteOnOff" (worker . ev_data)
-- main = withMIDI "NoteOnOff" (worker2 initialKeyboardstate . ev_data)


-- | Worker of this code.
worker :: EventData -> IO ()
worker (NoteEv NoteOn n) = mkGated (note_note n)
worker (NoteEv NoteOff n) = freeGated (note_note n)
worker _ = return ()

-- | Create new synth node with given MIDI note. Synth's node id would
-- be de determined from MIDI note number.
mkGated :: Integral n => n -> IO ()
mkGated m = do
  now <- utcr
  withSC3 $ \fd -> send fd $ Bundle (UTCr $ now + ltc)
            [s_new "gatedSynth" (fromIntegral m + nodeIdOffset) AddToTail 1
             [("freq", midiCPS . fromIntegral $ m)]]


-- | Free the node related to given MIDI note number.
freeGated :: Integral n => n -> IO ()
freeGated m = do
  now <- utcr
  withSC3 $ \fd -> send fd $ Bundle (UTCr $ now + ltc)
              [n_set (fromIntegral m + nodeIdOffset) [("gate", 0)]]

-- | Latency
ltc :: Double
ltc = 0.05

-- | Setup for scsynth. Sends a synthdef.
setSC :: Transport t => t -> IO OSC
setSC fd = do
  reset fd
  send fd $ d_recv $ synthdef "gatedSynth" gatedSynth
  wait fd "/done"

-- | Synthdef with envelope controlled with @gate@ value.
gatedSynth :: UGen
gatedSynth = out 0 $ mce [sig, sig]
    where
      sig = sinOsc ar f 0 * 0.2 * e
      f = control kr "freq" 440
      e = envGen kr g 1 0 1 RemoveSynth shp
      g = control kr "gate" 1
      shp = env [0,1,0.5,0] [0.02,0.1,0.05] [EnvCub, EnvLin,EnvSin] 1 0

-- | Offset of node Id. MIDI note number would be added to this number
-- and synth node would be created.
nodeIdOffset :: Num a => a
nodeIdOffset = 10000
