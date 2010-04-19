------------------------------------------------------------------------------
-- |
-- 
-- Using withMIDI and making pitched sound with scsynth..
-- 

module NoteOn where

import Sound.Alsa.Sequencer
import Sound.OpenSoundControl
import Sound.SC3

import Sound.SC3.Wing.MIDI (withMIDI)

main :: IO ()
main = withMIDI "NoteOn2" worker

worker :: Event -> IO ()
worker ev = 
    case ev_data ev of
      NoteEv ne n -> worker' ne n
      _ -> return ()

worker' :: NoteEv -> Note -> IO ()
worker' NoteOn n = do
  now <- utcr
  withSC3 $ \fd -> send fd $ Bundle (UTCr $ now + latency) [n2o n]
      where latency = 0.05
            n2o :: Note -> OSC
            n2o n = s_new "pitchedSynth" (-1) AddToTail 1 
                    [("freq", midiCPS $ fromIntegral $ note_note n)]
worker' _ _ = return ()

setupSC :: Transport t => t -> IO OSC
setupSC fd = do
    reset fd
    send fd $ d_recv $ synthdef "pitchedSynth" pitchedSynth 
    wait fd "/done"

pitchedSynth :: UGen
pitchedSynth = out 0 $ mce [sig, sig]
    where
      sig = sinOsc ar f 0 * 0.3 * e
      e = envGen kr 1 1 0 1 RemoveSynth shp 
      shp = envPerc 0.01 1
      f = control kr "freq" 440