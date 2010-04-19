------------------------------------------------------------------------------
-- | Module to play with @gate@ message.
--

module SCExamples.Pieces.GateExample where

import Control.Applicative
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3

-- import Reusable
-- import SCQuery
-- import SCSched
-- import SCTree

-- import qualified Scratch.ControlArgs as A

import Sound.SC3.Wing
import qualified Sound.SC3.Wing.UGen.ControlArg as A

playGates :: BPM -> IO ()
playGates bpm = spawn 0 bpm $ mapToE gateNID (M.map cycle gateMap)

setupGates :: Transport t => t -> IO ()
setupGates fd = do
  loadSynthdef "gateSynth" gateSynth fd
  send fd $ s_new "gateSynth" gateNID AddToTail 1 []

gateNID :: Num a => a
gateNID = 2001

setGate :: Transport t => Double -> t -> IO ()
setGate gate fd = do
  send fd $ n_set gateNID [("gate", gate)]

gateSynth :: UGen
gateSynth = out 0 $ mce [sig,sig]
    where
      sig = osc * env * A.amp
      osc = sinOsc ar (A.freq + (env2 * 5 * A.freq * sinOsc ar A.freq 0)) 0
      env = envGen kr A.gate 1 0 1 DoNothing shp
      shp = envCoord' [(0,0), (0.05,1),(0.3,0)] 1 1 EnvSin
      env2 = envGen kr A.gate 0.5 0 0.5 DoNothing shp

gateEvents :: Event OSC
gateEvents = mapToE gateNID gateMap

gateMap :: Phrase
gateMap = M.fromList $
  [("dur",     [0.75,0.25, 0.25,0.50,0.25, 0.50,0.25,0.25, 0.50,0.25,0.25]),
   ("sustain", map (*0.75)
               [0.70,0.20, 0.20,0.40,0.20, 0.40,0.20,0.20, 0.40,0.20,0.20]),
   ("freq",    map midiCPS
               [48,43, 67,55,60, 53,67,55, 65,72,60]),
   ("amp",     map (* 0.3)
               [1,0.5, 0.5,0.5,0.3,  1.0,0.5,0.5, 0.5,0.5,0.3])]

addSustain :: Double -> Phrase -> Phrase
addSustain d phr =
    M.insert "sustain"
         (map (*d) $ maybe [] id $ M.lookup "dur" phr) phr

mapToE :: NodeId -> Phrase -> Event OSC
mapToE nid phr = mappend (listE $ zip dur osc)
                 (listE $ zipWith closeGate dur sus)
    where
      dur = scanl (+) 0 $ maybe [] id $ M.lookup "dur" phr
      osc = mkNSet nid $ M.insert "gate" (repeat 1) $ phr
      sus = maybe [] id . M.lookup "sustain" $ phr
      closeGate d o = ((d+o), n_set nid [("gate",0)])
