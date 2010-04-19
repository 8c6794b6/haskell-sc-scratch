------------------------------------------------------------------------------
-- | Another attempt to play with NoteOn, NoteOff message using MVar.
--

module NoteOnOff2 where

import Control.Concurrent
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Sound.Alsa.Sequencer
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Wing.MIDI

-- | Run this either from ghci or compiled program, and connect with
-- MIDI keyboard.
main :: IO ()
main = do
  var <- newMVar initialKeyboardState
  withMIDI "NoteOnOff2" (worker var . ev_data)

-- | Datatype to hold the state of 
data KeyboardState = KeyboardState {kbCurrentNodeId :: Int,
                                    kbCurrentMap :: IntMap Int}
                     deriving (Eq)

instance Show KeyboardState where
    show (KeyboardState i m) =
        "KeyboardState: " ++ "Id:" ++ show i ++ " Map: " ++ show m

-- | Initial KeyboardState, with scsynth node Id offset and empty IntmMap.
initialKeyboardState :: KeyboardState
initialKeyboardState = KeyboardState nodeIdOffset IM.empty

nodeIdOffset :: Int
nodeIdOffset = 10000

-- | Worker that do the task:
--
-- * With NoteOn message, send s_new Message and update the current id
--   and add the (MIDI notenumber, scsynth nodeId) to IntMap in the state
--
-- * With NoteOff message, send n_set Message set @"gate"@ control
--   value to 0, and delte the acoompanied elemente IntMap.
--
-- * With other message than NoteOn nor NoteOff, print the message.
--
worker :: MVar KeyboardState -> EventData -> IO ()
worker var (NoteEv NoteOn n) = goNewNode var n
worker var (NoteEv NoteOff n) = goFreeNode var n
worker _ e = print e

-- | Send s_new message to scsynth and update the state.
goNewNode :: MVar KeyboardState -> Note -> IO ()
goNewNode var n = do
  now <- utcr
  st@(KeyboardState i _) <- takeMVar var
  withSC3 $ \fd -> send fd $ Bundle (UTCr $ now + ltc) [nToO i $ note_note n]
  putMVar var $ addNoteToState st $ note_note n

-- | Add (MIDI note number, sc nodeId) pair into IntMap.
addNoteToState :: Integral n => KeyboardState -> n -> KeyboardState
addNoteToState (KeyboardState i m) n =
    KeyboardState (getNextNodeId i) $ IM.insert (fromIntegral n) i m

-- | Make scsynth node id to fit in range from offSet to (offSet+127).
getNextNodeId :: Int -> Int
getNextNodeId i = ((i+1-nodeIdOffset) `mod` 128) + nodeIdOffset

-- | Returns OSC message with given pitch in MIDI Note.
nToO :: Integral n => Int -> n -> OSC
nToO i n = s_new "gatedSynth" i AddToTail 1
           [("freq", midiCPS . fromIntegral $ n)]

-- | latency
ltc :: Double
ltc = 0.05

-- | Free the scsynth node and update State.
goFreeNode :: MVar KeyboardState -> Note -> IO ()
goFreeNode var n = do
  now <- utcr
  st@(KeyboardState i m) <- takeMVar var
  let nId = IM.lookup n' m
      m' = IM.delete n' m
      n' = fromIntegral $ note_note n
  withSC3 $ \fd -> send fd $ Bundle (UTCr $ now + ltc)
                   [n_set (maybe (-1) id nId) [("gate", 0)]]
  putMVar var $ KeyboardState i m'

-- | Setup for scsynth. Sends a synthdef.
setSC :: Transport t => t -> IO OSC
setSC fd = do
  reset fd
  send fd $ d_recv $ synthdef "gatedSynth" gatedSynth
  wait fd "/done"

-- | Synthdef with released with changing @gate@ control argument.
gatedSynth :: UGen
gatedSynth = out 0 $ mce [sig, sig]
    where
      sig = sinOsc ar mod 0 * 0.2 * e
      mod = sinOsc ar (f*1.5) 0 * f + f * ((modMul+1)*0.005)
      modMul = lfNoise1 'a' ar 4.5
      f = control kr "freq" 440
      e = envGen kr g 1 0 1 RemoveSynth shp
      g = control kr "gate" 1
      shp = env [0,1,0.5,0] [0.02,0.05,0.1] [EnvCub, EnvLin,EnvSin] 1 0
