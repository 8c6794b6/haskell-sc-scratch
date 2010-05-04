------------------------------------------------------------------------------
-- | Playing with midi file created with lilypond.

module Scratch.Midi.PlayWithLily where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Sound.SC3
import Sound.OpenSoundControl
import qualified Sound.MIDI.File as MIDIFile
import qualified Sound.MIDI.File.Load as MIDIFileLoad

import Scratch.Midi.FromLily
import Scratch.Scheduling1
import Scratch.UGen

data NodeState = NodeState {
      nodeSynth :: String, 
      currentId :: Int, 
      nodeMap :: IntMap Int
    } deriving (Eq, Show)

initNodeState :: NodeState
initNodeState = NodeState  "simpleGated" 8000 IntMap.empty

-- seqTupToOSC :: (Num a) => [(d, [(Int, a)])] -> [(d, [OSC])]
-- seqTupToOSC ts = evalState (mapM newOrFreeList ts) initNodeState

newOrFree :: (Num a) => (Int, a) -> State NodeState OSC
newOrFree (p, 0) = do
  NodeState _ cur m <- get
  let nid = maybe (-1) id $ IntMap.lookup p m
  return $ n_set nid [("gate", 0)]
newOrFree (p, _) = do
  NodeState name cur m <- get
  let m' = IntMap.insert p cur m
  put $ NodeState name (cur+1) m'
  return $ s_new name cur AddToTail 1
             [("freq", midiCPS $ fromIntegral p)]

newOrFreeList :: (Num a) => (d, [(Int, a)]) -> State NodeState (d, [OSC])
newOrFreeList (t, ps) = do
  st@(NodeState name cur m) <- get
  let (osc, st') = runState (mapM newOrFree ps) st
  put st'
  return (t, osc)

oscTracks :: IO (SeqEvent OSC)
oscTracks = seqTupToOSC initNodeState . foldr merge [] . extractTracks <$> theTracks

seqTupToOSC :: (Num a) => NodeState -> [(d, [(Int, a)])] -> [(d, [OSC])]
seqTupToOSC st ts = evalState (mapM newOrFreeList ts) st

midiToGated :: FilePath -> IO (SeqEvent OSC)
midiToGated file = foldr merge [] . 
                   zipWith seqTupToOSC sts . 
                   extractTracks <$> 
                   MIDIFileLoad.fromFile file
    where
      sts = map (\n -> NodeState "simpleGated" (n*1000) IntMap.empty) [8..]