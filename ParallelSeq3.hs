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

-- | Control busses
ampBus1, freqBus1, posBus1, trigBus1 :: Num a => a
ampBus1 = 100
freqBus1 = 101
posBus1 = 102
trigBus1 = 103

data Note = Note { notePitch :: Double,
                   noteAmp :: Double,
                   noteDur :: Double }

notes1 :: [Note]
notes1 = 
    -- [Note 80 60 1.0, Note 78 62 1.0, Note 80 64 0.5, Note 76 65 0.5,
    --  Note 81 67 1.5, Note 77 69 1.0, Note 78 71 1.5, Note 80 72 1.0]
    [Note 60 80 0.5, Note 62 78 0.5, Note 64 78 1.0, Note 60 81 1.5,
     Note 62 76 0.5, Note 64 81 2.0, Note 60 80 1.5, Note 62 81 0.5] 

     
-- | UGen which makes sound.
paraUGen :: UGen
paraUGen = out 0 (pan2 osc pos 1) where
    osc = sinOsc AR freq 0 * amp
    pos = Control KR "pos" 0.0
    freq = in' 1 KR (Control KR "freq" freqBus1)
    amp = in' 1 KR (Control KR "amp" ampBus1) * env
    env = envGen KR envTrig 1 0 1 DoNothing envShape
    envTrig = in' 1 KR (Control KR "trig" trigBus1) 
    envShape = envPerc 0.01 0.8

-- | UGen which controls the paraUGen.
simplePlayerUGen :: IO UGen
simplePlayerUGen = do
  let notesToDseq ns f = dseq 1 (mce (map (constant . f) ns))

  durs1 <- notesToDseq notes1 noteDur
  freqs1 <- notesToDseq notes1 (midiCPS . notePitch)
  amps1 <- notesToDseq notes1 (dbAmp . (flip (-) 100) . noteAmp)

  let bpm = 130
      noteTrig1 = tDuty KR (60*durs1/bpm) 0 RemoveSynth 1 0
      freq1 = demand noteTrig1 0 freqs1
      amp1 = demand noteTrig1 0 amps1

  return $ mce [out ampBus1 amp1, 
                out freqBus1 freq1,
                out trigBus1 noteTrig1]

setup :: IO ()
setup = do
  writeSynthdef "para" paraUGen
  writeSynthdef "simplePlayer" =<< simplePlayerUGen
  withSC3 reloadSynthdef

doPlay :: IO ()
doPlay = utcr >>= withSC3 . send' . msg where
    msg time = Bundle (UTCr time) [
           s_new "simplePlayer" 1000 AddToHead 1 [],
           s_new "para" 1001 AddToTail 1 []
          ]

-- | Type synonym for sending osc message to scsynth.
type SendUDP a = UDP -> IO a

-- | @flip send@. With flipping the argument, one can write as below:
-- > withSC3 (send' some_osc_message)
send' :: OSC -> SendUDP ()
send' = flip send

-- | @flip async@.
async' :: OSC -> SendUDP OSC
async' = flip async