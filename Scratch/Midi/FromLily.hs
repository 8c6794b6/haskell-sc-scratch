{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- | Trying to read midi file created by lilypond and convert it to
-- data which is sequentially playable by haskell.
--
-- TODO:
--
-- * Play each individual track with chosing name of synthdef.
--
-- * Modify to work with gated synthdef, with converting 0 velocity
--   message.
--

module Scratch.Midi.FromLily where

import Control.Applicative
import Control.Arrow
import Control.Monad (join)
import Data.Maybe

import qualified Data.EventList.Relative.TimeBody as TimeBody
import qualified Numeric.NonNegative.Class as NonNegativeClass
import qualified Sound.MIDI.File as MIDIFile
import qualified Sound.MIDI.File.Event as MIDIEvent
import qualified Sound.MIDI.File.Load as MIDIFileLoad
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice

import Sound.SC3
import Sound.OpenSoundControl

import Scratch.Scheduling1

-- | MIDI file created by lilypond.
theLine :: IO MIDIFile.T
theLine = MIDIFileLoad.fromFile
          "/home/atsuro/repos/haskell-sc-scratch/Scratch/Midi/line.midi"

theLine2 :: IO MIDIFile.T
theLine2 = MIDIFileLoad.fromFile
           "/home/atsuro/repos/haskell-sc-scratch/Scratch/Midi/line2.midi"

theChord :: IO MIDIFile.T
theChord = MIDIFileLoad.fromFile
           "/home/atsuro/repos/haskell-sc-scratch/Scratch/Midi/chord.midi"

theTracks :: IO MIDIFile.T
theTracks = MIDIFileLoad.fromFile
           "/home/atsuro/repos/haskell-sc-scratch/Scratch/Midi/threeTracks.midi"

-- | Extracts the track from midi file.
getTracks :: MIDIFile.T -> [MIDIFile.Track]
getTracks (MIDIFile.Cons _ _ t) = t

-- | Returns True if the MIDIEvent.T is MIDIEvent, otherwise False.
isMIDIEvent :: MIDIEvent.T -> Bool
isMIDIEvent (MIDIEvent.MIDIEvent _) = True
isMIDIEvent _ = False

-- | Converts MIDIEvent to Maybe OSC message.
midiToOSC :: MIDIEvent.T -> Maybe OSC
midiToOSC (MIDIEvent.MIDIEvent _) = undefined

collectMIDIEvent :: TimeBody.T MIDIEvent.ElapsedTime MIDIEvent.T
                 -> [(MIDIEvent.ElapsedTime, [MIDIEvent.T])]
collectMIDIEvent = TimeBody.toPairList . TimeBody.collectCoincident
                   . TimeBody.filter isMIDIEvent

-- | Extract pitch and velocity from midi event.
midiEventToTuple :: (Num a, Num b) => MIDIEvent.T -> Maybe (a, b)
midiEventToTuple (MIDIEvent.MIDIEvent (Channel.Cons _ body)) =
    case body of
      Channel.Voice (Voice.NoteOn p v) ->
          Just (fromIntegral . Voice.fromPitch $ p,
                fromIntegral . Voice.fromVelocity $ v)
      Channel.Voice (Voice.NoteOff p v) ->
          Just (fromIntegral . Voice.fromPitch $ p,
                fromIntegral . Voice.fromVelocity $ v)
      _ -> Nothing

timeBodyToTuple :: (Num a, Num b, Num c) =>
                     TimeBody.T MIDIEvent.ElapsedTime MIDIEvent.T ->
                    [(a, [(b, c)])]
timeBodyToTuple = map f . collectMIDIEvent
    where f (a, b)= (fromInteger . MIDIEvent.fromElapsedTime $ a,
                     catMaybes $ map midiEventToTuple b)

shiftTime :: (Num t) => [(t, a)] -> [(t, a)]
shiftTime [] = []
shiftTime ((t1,v1):[]) = [(t1,v1)]
shiftTime ((t1,v1):(t2,v2):es) = (t2,v1): shiftTime ((t2,v2):es)

midiToPV :: (Fractional a, Num b, Num c) => MIDIFile.T -> [(a, [(b, c)])]
midiToPV = map (first (/ 384)) . shiftTime . concat .
           map timeBodyToTuple . tail . getTracks

percSynthNew :: String -> Int -> (Double, Double) -> Maybe OSC
percSynthNew _ _ (p,0) = Nothing
percSynthNew name grp (p,v) = Just osc
    where osc = s_new name (-1) AddToTail grp [("freq", midiCPS p)]

playPercSynth :: FilePath -> BPM -> String -> IO ()
playPercSynth file bpm name =
    runSeqOSC bpm =<<
    map (second (catMaybes . map (percSynthNew name 1))) .
    midiToPV <$> MIDIFileLoad.fromFile file

et :: (Fractional t) =>
      String ->
      TimeBody.T MIDIEvent.ElapsedTime MIDIEvent.T ->
      [(t, [OSC])]
et name = map (second (catMaybes . map (percSynthNew name 1)) .
               first (/ 384)) . shiftTime . timeBodyToTuple

-- | Plays multi track midi file with single percussive synthdef.
playTracks :: FilePath -> BPM -> String -> IO ()
playTracks file bpm name =
    runSeqOSC bpm =<<
    foldr merge [] .
    map (et name) . tail . getTracks <$>
    MIDIFileLoad.fromFile file
