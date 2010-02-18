------------------------------------------------------------------------------
-- Etude of additive synthesis.
--

module Etude.Additive01 where

import Control.Applicative
import Control.Concurrent
import Data.List (zipWith4, zipWith5)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3

import SCQuery
import SCTree
import Reusable
import qualified Scratch.ControlArgs as A
import Scratch.Pat.P5

-- | Type synonym for @line@ and @xLine@.
type LineUGen = Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen

updateSynthdefs :: IO ()
updateSynthdefs = 
    mapM_ (uncurry writeSynthdef) synthdefs >> withSC3 reloadSynthdef
    where 
      synthdefs = [("anOsc", anOsc),
                   ("aLine", aLine),
                   ("aLag", aLag),
                   ("anXLine", anXLine)]

numOsc :: Int
numOsc = 128

oscIds :: [NodeId]
oscIds = mkIds 1001

ampIds :: [NodeId]
ampIds = mkIds 2001

freqIds :: [NodeId]
freqIds = mkIds 3001

panIds :: [NodeId]
panIds = mkIds 4001

mkIds :: Int -> [Int]
mkIds str = enumFromTo str (str+numOsc-1)

ampBusses :: [Int]
ampBusses = mkIds 1

freqBusses :: [Int]
freqBusses = mkIds 1001

panBusses :: [Int]
panBusses = mkIds 2001

setAmps :: [Double] -> IO ()
setAmps = setVals ampIds

setFreqs :: [Double] -> IO ()
setFreqs = setVals freqIds

setPans :: [Double] -> IO ()
setPans = setVals panIds

setVals :: [NodeId] -> [Double] -> IO ()
setVals ns ps = 
    mapM_ (\(n, p) -> withSC3 $ \t -> send t $ n_set n [("val", p)])
         (zip ns ps)

setDurs :: [NodeId] -> [Double] -> IO ()
setDurs ns ps = 
    mapM_ (\(n,p) -> withSC3 $ \t -> send t $ n_set n [("dur",p)])
          (zip ns ps)

tree :: SCTree
tree = 
  Group 0 
   [Group 1 
    [Group 10 amps,
     Group 11 freqs,
     Group 12 pans,
     Group 20 oscs,
     Group 30 []]]

amps :: [SCTree]
amps = zipWith4 mkLag
       ampIds ampBusses (repeat 0.02) (repeat 1)

freqs :: [SCTree]
freqs = zipWith4 mkLag
        freqIds freqBusses (repeat 440) (repeat 1)

pans :: [SCTree]
pans = zipWith4 mkLag
       panIds panBusses  (repeat 0) (repeat 1)

oscs :: [SCTree]
oscs = zipWith4 mkAnOsc oscIds ampBusses freqBusses panBusses

mkAnOsc :: NodeId -> BusId -> BusId -> BusId -> SCTree
mkAnOsc nId amp freq pan = 
    Synth nId "anOsc" ["amp":<-amp, "freq":<-freq, "pan":<-pan]

mkLag :: NodeId -> BusId -> Double -> Double -> SCTree
mkLag nId bId val dur = 
    Synth nId "aLag"
          ["out":=fromIntegral bId, "val":=val, "dur":=dur]

anOsc :: UGen
anOsc = out 0 $ pan2 (sinOsc ar A.freq 0 * A.amp) A.pan 1

aLine :: UGen
aLine = out A.out $ line kr A.start A.end A.dur DoNothing

aLag :: UGen
aLag = out A.out $ lag A.val A.dur 

anXLine :: UGen
anXLine = out A.out $ xLine kr A.start A.end A.dur DoNothing
