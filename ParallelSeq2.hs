----------------------------------------------------------------------
-- | Scratch for parallel sequencing, again.
-- 
-- Note the use of @tDuty@ ugen. This ugen will return next trigger
-- after specified duration, in it's second argument. The list of
-- duration is retrieved from @[Note]@, by extracting @noteDur@. Then
-- the trigger from @tDuty@ will be sent to @demand@ ugens, and the
-- returning values from @demand@ ugens are used for controlling
-- amplitude and frequency.

module ParallelSeq2 where

import Sound.SC3
import Sound.OpenSoundControl

import WritingSynthDef

data Note = Note { notePitch :: Double,
                   noteAmp :: Double,
                   noteDur :: Double }

pattern1 :: IO UGen
pattern1 = do
  let notes1 = [
            Note 60 80 0.5, Note 62 78 0.5, Note 64 78 1.0, Note 60 81 1.5,
            Note 62 76 0.5, Note 64 81 2.0, Note 60 80 1.5, Note 62 81 0.5 
           ]
      notes2 = [
            Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
            Note 53 81 1.5, Note 48 75 0.5, Note 53 80 1.5, Note 48 75 0.5,
            Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
            Note 55 81 1.5, Note 50 75 0.5, Note 55 80 1.5, Note 50 75 0.5
           ]

      noteToParam ns f = dseq dinf (mce (map (constant . f) ns))
      noteToDseqs ns = mapM (noteToParam ns) [noteAmp, notePitch, noteDur]

  [amps1,pitches1,durs1] <- noteToDseqs notes1
  [amps2,pitches2,durs2] <-  noteToDseqs notes2

  let bpm = 130

      noteTrig1 = tDuty KR (durs1*60/bpm) 0 RemoveSynth 1 0
      amp1 = demand noteTrig1 0 amps1
      pitch1 = demand noteTrig1 0 pitches1
      osc1 = sinOsc AR (midiCPS pitch1) 0 * dbAmp (amp1 - 100)

      noteTrig2 = tDuty KR (durs2*60/bpm) 0 RemoveSynth 1 0
      amp2 = demand noteTrig2 0 amps2
      pitch2 = demand noteTrig2 0 pitches2
      osc2 = sinOsc AR (midiCPS pitch2) 0 * dbAmp (amp2 - 100)

  return (out 0 (pan2 osc1 (-0.5) 1 + pan2 osc2 0.5 1))


epattern2 = undefined


