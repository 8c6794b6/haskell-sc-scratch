------------------------------------------------------------------------------
-- | Playing with midi file created with lilypond.
--
-- Try:
--
-- > runSeqOSC 60 =<< midiToGated "/path/to/midi/file.midi"
--

module Scratch.Midi.PlayWithLily where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Sound.SC3
import Sound.OpenSoundControl
import qualified Sound.MIDI.File.Load as MIDIFileLoad

import Scratch.Midi.FromLily
import Scratch.Scheduling1
import Scratch.UGen

-- | Datatype to hold the synthdef name, current NodeId, and node id map.
data NodeState = NodeState {
      nodeSynth :: String,
      currentId :: Int,
      nodeMap :: IntMap Int
    } deriving (Eq, Show)

initNodeState :: NodeState
initNodeState = NodeState  "simpleGated" 8000 IntMap.empty

-- | Returns a state monad to convert midi note on/off message to osc message.
-- First element in the tuple is midi pitch number, second is velocity.
newOrFree :: (Num a) => (Int, a) -> State NodeState OSC
newOrFree (p, 0) = do
  NodeState _ cur m <- get
  let nid = maybe (-1) id $ IntMap.lookup p m
  return $ n_set nid [("gate", 0)]
newOrFree (p, _) = do
  NodeState name cur m <- get
  let m' = IntMap.insert p cur m
  put $ NodeState name (cur+1) m'
  return $ s_new name cur AddToTail 1 [("freq", midiCPS $ fromIntegral p)]

-- | Returns a state monad to convert list of pair of duration waiting for next
-- event, and midi note on/off messages.
newOrFreeList :: (Num a) => (d, [(Int, a)]) -> State NodeState (d, [OSC])
newOrFreeList (t, ps) = do
  st@(NodeState name cur m) <- get
  let (osc, st') = runState (mapM newOrFree ps) st
  put st'
  return (t, osc)

-- | Takes NodeState and extracted midi event, and convert to sequence of OSC messages.
seqTupToOSC :: (Num a) => NodeState -> [(d, [(Int, a)])] -> [(d, [OSC])]
seqTupToOSC st ts = evalState (mapM newOrFreeList ts) st

-- | Current gated synthdefs written in Scratch/UGen.hs are:
-- 
-- * simpleGated
-- * gtdOrgan01
-- 
playGated :: FilePath -> String -> BPM -> IO ()
playGated midi synthname bpm =
    runSeqOSC bpm =<<
    foldr merge [] .
    zipWith seqTupToOSC sts .
    extractTracks <$> MIDIFileLoad.fromFile midi
    where
      sts = map (\n -> NodeState synthname n IntMap.empty) [8000,9000..]

-- | Plays midifile with simpleGated synthdef.
playSimpleGated :: FilePath -> BPM -> IO () 
playSimpleGated file bpm = playGated file "simpleGated" bpm 

-- | FilePath to schoenberg's study, chapter 07.
ch07 :: FilePath
ch07 = "/home/atsuro/repos/study/schoenberg/section1/chapter07/"