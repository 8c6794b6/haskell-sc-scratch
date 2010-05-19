------------------------------------------------------------------------------
-- | Scratch to convert haskore's Melody datatype to list of list of OSC
-- messages
--

module Scratch.HaskoreScratch
    ( Melody,
      FromNote,
      GateState (..),
      noteToSeqEv,
      noteToSeqEvNew,
      noteToSeqEvSet,
      noteToSeqEvGated,
      defaultGateState,
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
                  -> Maybe Int -- ^ Just /Pitch in MIDI/ or Nothing for rest.
                  -> Maybe a -- ^ Note attribute
                  -> b
--                  -> (Double, [b])

-- | Apply a function to each note of melody, and returns SeqEvent.
--
-- Ignoreing Control type constructors.
noteToSeqEv :: FromNote a [b] -> Melody a -> SeqEvent b
noteToSeqEv f n@(Primitive x) =
   [(noteToBeat n,
     f (noteToBeat n) (noteToMaybePitch n) (noteToAttr n))]
noteToSeqEv f n@(Serial xs) = concatMap (noteToSeqEv f) xs
noteToSeqEv f n@(Parallel xs) = foldr (//) [] (map (noteToSeqEv f) xs)
noteToSeqEv f n@(Control x y) = []

-- | Sends s_new with given list of tuples, adding to tale of target group id
-- with nodeId (-1).
noteToSeqEvNew :: String -> Int -> FromNote a [(String, Double)] -> Melody a -> SeqEvent OSC
noteToSeqEvNew defname gid f n = noteToSeqEv f' n
    where f' d p a = [s_new defname (-1) AddToTail gid (f d p a)]

-- | Sends n_set with given list of tuples, sets the node of given node id.
--
-- XXX: What should happen with parallel events with single node id specified?
--
noteToSeqEvSet :: Int -> FromNote a [(String, Double)] -> Melody a -> SeqEvent OSC
noteToSeqEvSet nid f n = noteToSeqEv f' n
    where f' d p a = [n_set nid (f d p a)]

-- | Sends s_new messages paired with ("gate", 0) set message after the duration
-- specified by /legato/. Every /legato/ must be @0.0 < legato =< 1.0@.
noteToSeqEvGated :: GateState -> FromNote a [(String, Double)] -> Melody a -> SeqEvent OSC
noteToSeqEvGated st f melody = evalState (ntse f melody) st

data GateState = GateState {
      gsSynthdefName :: String,
      gsGroupId :: Int,
      gsCurrentNodeId :: Int,
      gsNodeMap :: IntMap Int,
      gsLegato :: Double -- ^ Between 0 < gsLegato <= 1 
    } deriving (Eq, Show)

defaultGateState :: GateState
defaultGateState = 
    GateState {gsSynthdefName = "default",
               gsGroupId = 1,
               gsCurrentNodeId = 20000,
               gsNodeMap = IM.empty,
               gsLegato = 0.95}

-- ntse :: String -> Int -> FromNote a (String, Double)
--      -> Melody a -> State GateState [(Double,[OSC])]
-- ntse name gid f n@(Primitive x) = do
--   st <- get
--   pitch <- return 0 -- maybe 0 id $ noteToMaybePitch x
--   let cid = gsCurrentNodeId st
--       newMsg = s_new name (-1) AddToTail gid [(f n)]
--       releaseMsg = undefined
--   return [] -- [(0, [newMsg, releaseMsg])]

ntse :: FromNote a [(String, Double)] -> Melody a -> State GateState (SeqEvent OSC)
ntse f n@(Primitive x) = do
  st <- get
  let nodeId = gsCurrentNodeId st
      newMsg = s_new (gsSynthdefName st) nodeId AddToTail
               (gsGroupId st) (fromNote f n)
      rlsMsg = n_set nodeId [("gate", 0)]
  put $ st {gsCurrentNodeId=nodeId+1}
  return [(noteToBeat n * gsLegato st, [newMsg]), 
          (noteToBeat n * (1 - gsLegato st), [rlsMsg])]
ntse f n@(Serial xs)   = fmap concat . sequence $ map (ntse f) xs
ntse f n@(Parallel xs) = fmap (foldr (//) []) . sequence $ map (ntse f) xs
ntse f n@(Control _ _) = return []

fromNote :: FromNote a b -> Melody a -> b
fromNote f = \n -> f (noteToBeat n) (noteToMaybePitch n) (noteToAttr n)

--    [(noteToBeat n, f (noteToBeat n) (toInt $ noteToPitch n) (noteToAttr n))]

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

f1 :: FromNote a [OSC]
f1 d pch _ = [s_new "simplePitched" (-1) AddToTail 1
                   [("freq",
                     maybe 0 (midiCPS . fromIntegral) pch)]]

f2 :: FromNote a [OSC]
f2 d pch _ = [s_new "susOrgan01" (-1) AddToTail 1
                   [("freq",
                     maybe 0 (midiCPS . fromIntegral) pch),
                    ("sustain", d * 0.9)]]

g1 :: FromNote a [(String, Double)]
g1 d p _ = [("freq", maybe 0 (midiCPS . fromIntegral) p)]

g2 :: FromNote a [(String, Double)]
g2 d p _ = [("freq", maybe 0 (midiCPS . fromIntegral) p),
            ("duration", d)]

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
