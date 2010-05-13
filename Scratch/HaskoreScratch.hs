------------------------------------------------------------------------------
-- | Scratch to convert haskore's Melody datatype to list of list of OSC
-- messages
-- 

module Scratch.HaskoreScratch where

import Data.Accessor (getVal)
import Haskore.Basic.Duration
import Haskore.Melody
import Haskore.Music
import Haskore.Basic.Pitch (toInt)
import Medium.Controlled.List (parallel, serial, T(..))
import qualified Medium.Controlled.List as MCL

import Sound.OpenSoundControl
import Sound.SC3 (s_new, midiCPS, AddAction(..), withSC3, reset)
import Sound.SC3.Wing.ScheduleSimple

type Melody a = Haskore.Melody.T a

-- | Type synonym for function to convert a Note to list of OSC. 
type FromNote a b = Double -- ^ Duration
                  -> Int -- ^ Pitch, in MIDI
                  -> Maybe a -- ^ Note attribute
                  -> [(Double, [b])]

-- | Apply a function to each note of melody, and returns SeqEvent.
-- 
-- Ignoreing Control type constructors.
noteToSeqEv :: FromNote a b -> Melody a -> SeqEvent b
noteToSeqEv f n@(Primitive x) = f (noteToBeat n) (toInt $ noteToPitch n) (noteToAttr n)
--      [(noteToBeat n, f (noteToBeat n) (toInt $ noteToPitch n) (noteToAttr n))]
noteToSeqEv f n@(Serial xs) = concatMap (noteToSeqEv f) xs
noteToSeqEv f n@(Parallel xs) = foldr (//) [] (map (noteToSeqEv f) xs)
noteToSeqEv f n@(Control x y) = []

-- | Serial composition for note.
seri :: [MCL.T control a] -> MCL.T control a
seri = serial 

-- | Parallel composition for note.
para :: [MCL.T control a] -> MCL.T control a
para = parallel

-- | Extracts attribute from note.
noteToAttr :: Melody a -> Maybe a
noteToAttr (Primitive x) = fmap (getVal noteAttrs) x' where Atom _ x' = x
noteToAttr _ = Nothing

-- | Extracts duration and converts to beat. 
noteToBeat :: Fractional a => Melody b -> a
noteToBeat (Primitive x) = 4 * toNumber d where Atom d _ = x


------------------------------------------------------------------------------
-- 
-- For testing
-- 
------------------------------------------------------------------------------

f1 :: FromNote a OSC
f1 d pch _ = [(d, [s_new "simplePitched" (-1) AddToTail 1 
                   [("freq", midiCPS $ fromIntegral pch)]])]

f2 :: FromNote a OSC
f2 dur pch _ = [(dur, 
                 [s_new "susOrgan01" (-1) AddToTail 1 
                  [("freq", midiCPS $ fromIntegral pch), ("sustain", dur * 0.9)]])]

line1 :: Melody ()
line1 = para [l1, l2, l3]
    where
      l1 = seri [e 7 qn (), d 7 qn (), c 7 qn ()]
      l2 = seri [a 6 qn (), g 6 qn (), g 6 qn ()]
      l3 = seri [c 5 qn (), b 4 qn (), c 5 qn ()]

line2 :: Melody ()
line2 = para [l1, l2, l3]
    where
      l1 = seri [e 6 sn (), d 6 sn (), c 6 sn (), d 6 sn ()]
      l2 = seri [g 5 en (), g 5 en ()]
      l3 = seri [c 4 qn ()]

