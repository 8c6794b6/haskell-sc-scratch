----------------------------------------------------------------------
-- | Parallel sequence of Note, third try.
--
-- This time, sending trigger to node with using busses and buffers.
-- Uses 2 ugens, one for sending trigger and parameter for the other
-- via control bus. The sound making ugen uses @in'@ haskell function
-- to make @In@ ugen in scsynth. Control busses are defined in top
-- level, and could be specifyed by sending parameters to ugen, via
-- @c_set@ message.
--
--

module ParralelSeq3 where


import Sound.SC3
import Sound.OpenSoundControl
import Reusable

-- | Set groups of scsynth.
initGroups :: IO ()
initGroups = undefined

-- | Control busses
ampBus1, freqBus1, trigBus1 :: Num a => a
ampBus1 = 100
freqBus1 = 101
trigBus1 = 102

ampBus2, freqBus2, trigBus2 :: Num a => a
ampBus2 = 200
freqBus2 = 201
trigBus2 = 202

data Note = Note { notePitch :: Double,
                   noteAmp :: Double,
                   noteDur :: Double }
          deriving (Eq, Show)

notes1 :: [Note]
notes1 = concat $ replicate 2
    [
     Note 60 80 0.5, Note 62 78 0.5, Note 64 78 1.0, Note 60 81 1.5,
     Note 62 76 0.5, Note 64 81 2.0, Note 60 80 1.5, Note 62 81 0.5
    ]

notes2 :: [Note]
notes2 =
    [
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 53 81 1.5, Note 48 75 0.5, Note 53 80 1.5, Note 48 75 0.5,
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 55 81 1.5, Note 50 75 0.5, Note 55 80 1.5, Note 50 75 0.5
    ]

-- | UGen making sound.
paraUGen :: UGen
paraUGen = out 0 (pan2 osc pos 1) where
    osc = sinOsc AR freq 0 * amp
    pos = Control KR "pan" 0.0
    freq = in' 1 KR (Control KR "freq" freqBus1)
    amp = in' 1 KR (Control KR "amp" ampBus1) * env
    env = envGen KR envTrig 1 0 1 DoNothing envShape
    envTrig = in' 1 KR (Control KR "trig" trigBus1)
    envShape = envPerc 0.01 0.8

-- | UGen which controls the paraUGen.
-- Apply mapping function before passing to demand ugen. Otherwise,
-- unexpected behavior would happen (... well, in this case, it happened).
simplePlayerUGen :: IO UGen
simplePlayerUGen = do
  let notesToDseq ns f = dseq dinf (mce (map (constant . f) ns))

  durs1 <- notesToDseq notes1 noteDur
  freqs1 <- notesToDseq notes1 (midiCPS . notePitch)
  amps1 <- notesToDseq notes1 (dbAmp . (flip (-) 100) . noteAmp)

  durs2 <- notesToDseq notes2 noteDur
  freqs2 <- notesToDseq notes2 (midiCPS . notePitch)
  amps2 <- notesToDseq notes2 (dbAmp . (flip (-) 100) . noteAmp)

  let bpm = 130
      noteTrig1 = tDuty KR (60*durs1/bpm) 0 RemoveSynth 1 0
      freq1 = demand noteTrig1 0 freqs1
      amp1 = demand noteTrig1 0 amps1

      noteTrig2 = tDuty KR (60*durs2/bpm) 0 RemoveSynth 1 0
      freq2 = demand noteTrig2 0 freqs2
      amp2 = demand noteTrig2 0 amps2

  return $ mce [out ampBus1 amp1,
                out freqBus1 freq1,
                out trigBus1 noteTrig1,
                out ampBus2 amp2,
                out freqBus2 freq2,
                out trigBus2 noteTrig2
               ]

-- | Write ugen to synthdef file and reload synthdefs.
setup :: IO ()
setup = do
  writeSynthdef "para" paraUGen
  writeSynthdef "simplePlayer" =<< simplePlayerUGen
  withSC3 reloadSynthdef

-- | Send synthdefs. 2 sound making ugens and 1 player ugen.
doPlay :: IO ()
doPlay = utcr >>= withSC3 . send' . msg where
    msg time = Bundle (UTCr time) [
           s_new "simplePlayer" 1000 AddToHead 1 [],
           s_new "para" 1001 AddToHead 1
                     [("freq",freqBus1),("amp",ampBus1),
                      ("trig",trigBus1),("pan",0.7)],
           s_new "para" 1002 AddToTail 1
                     [("freq",freqBus2),("amp",ampBus2),
                      ("trig",trigBus2),("pan",-0.5)]
          ]

