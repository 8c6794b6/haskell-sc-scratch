------------------------------------------------------------------------------
-- | Scratch to convert haskore's Melody datatype to list of list of OSC
-- messages
--

module Scratch.HaskoreScratch
    ( Melody,
      FromNote,
      noteToSeqEv,
      noteToSeqEvGated,
      seri,
      para,
      noteToAttr,
      noteToBeat,
      noteToPitch
    ) where

import Control.Monad.State
import Data.IntMap (IntMap)


import Data.Accessor (getVal)
import Haskore.Basic.Duration
import Haskore.Melody
import Haskore.Music
import Haskore.Basic.Pitch (toInt)
import Medium.Controlled.List (parallel, serial, T(..))
import qualified Data.IntMap as IM
import qualified Medium.Controlled.List as MCL


import Sound.OpenSoundControl
import Sound.SC3 (s_new, midiCPS, AddAction(..), withSC3, reset)
import Sound.SC3.Wing.ScheduleSimple

type Melody a = Haskore.Melody.T a

-- | Type synonym for function to convert a Note to list of OSC.
type FromNote a b = Double -- ^ Duration
                  -> Int -- ^ Pitch, in MIDI
                  -> Maybe a -- ^ Note attribute
                  -> (Double, [b])

-- | Apply a function to each note of melody, and returns SeqEvent.
--
-- Ignoreing Control type constructors.
noteToSeqEv :: FromNote a b -> Melody a -> SeqEvent b
noteToSeqEv f n@(Primitive x) =
    [f (noteToBeat n) (toInt $ noteToPitch n) (noteToAttr n)]
noteToSeqEv f n@(Serial xs) = concatMap (noteToSeqEv f) xs
noteToSeqEv f n@(Parallel xs) = foldr (//) [] (map (noteToSeqEv f) xs)
noteToSeqEv f n@(Control x y) = []


noteToSeqEvGated :: FromNote a (String,Double) -> Melody a -> SeqEvent OSC
noteToSeqEvGated  g melody = evalState (ntse g melody) initialGateState

-- noteToSeqEvGated f n@(Primitive x) = undefined
-- noteToSeqEvGated f n@(Serial xs) = concatMap (noteToSeqEvGated f) xs
-- noteToSeqEvGated f n@(Parallel xs) = foldr (//) [] (map (noteToSeqEvGated f) xs)
-- noteToSeqEvGated f n@(Control _ _) = []

data GateState = GateState {
      gsCurrentId :: Int,
      gsNodeMap :: IntMap Int,
      gsLegato :: Double
    } deriving (Eq, Show)

initialGateState :: GateState
initialGateState = GateState 20000 IM.empty 1.0

ntse :: FromNote a (String, Double) -> Melody a -> State GateState [(Double,[OSC])]
ntse f n@(Primitive x) = undefined
ntse f n@(Serial xs) = undefined
ntse f n@(Parallel xs) = undefined
ntse f n@(Control _ _) = return []

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
f1 d pch _ = (d, [s_new "simplePitched" (-1) AddToTail 1
                   [("freq", midiCPS $ fromIntegral pch)]])

f2 :: FromNote a OSC
f2 d pch _ = (d, [s_new "susOrgan01" (-1) AddToTail 1
                   [("freq", midiCPS $ fromIntegral pch),
                    ("sustain", d * 0.9)]])

g1 :: FromNote a (String, Double)
g1 d p _ = (d, [("freq", midiCPS $ fromIntegral p)])

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
