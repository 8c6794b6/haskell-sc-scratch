------------------------------------------------------------------------------
-- | Scratch to convert haskore's Melody datatype to list of list of OSC
-- messages
--

module Scratch.HaskoreScratch
    ( Melody,
      FromNote,
      noteToSeqEv,
      noteToSeqEvNew,
      noteToSeqEvSet,
      noteToSeqEvGated,
      seri,
      para,
      noteToAttr,
      noteToBeat,
      noteToPitch, 
      g1,
      g2
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
import Sound.SC3 (s_new, n_set, midiCPS, AddAction(..), withSC3, reset)
import Sound.SC3.Wing.ScheduleSimple

type Melody a = Haskore.Melody.T a

-- | Type synonym for function to convert a Note to list of OSC.
type FromNote a b = Double -- ^ Duration
                  -> Int -- ^ Just /Pitch in MIDI/ or Nothing for rest.
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

-- | Sends s_new with given list of tuples, adding to tale of target group id
-- with nodeId (-1).
noteToSeqEvNew :: String -> Int -> FromNote a (String, Double) -> Melody a -> SeqEvent OSC
noteToSeqEvNew defname gid f n = noteToSeqEv f' n 
    where f' d p atr = (d', [s_new defname (-1) AddToTail gid ps])
              where (d', ps) = f d p atr

-- | Sends n_set with given list of tuples, sets the node of given node id.
-- 
-- XXX: What should happen with parallel events with single node id specified? 
-- 
noteToSeqEvSet :: Int -> FromNote a (String, Double) -> Melody a -> SeqEvent OSC
noteToSeqEvSet nid f n = noteToSeqEv f' n 
    where f' d p a = (d', [n_set nid ps])
              where (d', ps) = f d p a

-- | Sends s_new messages paired with ("gate", 0) set message after the duration
-- specified by /legato/. Every /legato/ must be @0.0 < legato =< 1.0@.
noteToSeqEvGated :: FromNote a (String,Double) -> Melody a -> SeqEvent OSC
noteToSeqEvGated  g melody = evalState (ntse g melody) initialGateState

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
noteToBead _ = 0

-- | Extracts pitch from melody. Just /midi note number/ or Nothing for rest.
noteToMaybePitch :: Melody a -> Maybe Int
noteToMaybePitch (Primitive (Atom _ y)) = fmap (toInt . notePitch_) y


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

g2 :: FromNote a (String, Double)
g2 d p _ = (d, [("freq", midiCPS $ fromIntegral p), ("duration", d)])

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
