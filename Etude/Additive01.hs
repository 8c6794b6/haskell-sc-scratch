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
import SCSched
import Reusable
import qualified Scratch.ControlArgs as A

-- | Type synonym for @line@ and @xLine@.
type LineUGen = Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen

updateSynthdefs :: IO ()
updateSynthdefs = do
  mapM_ (\(n,u) -> writeSynthdef n =<< u) synthdefs 
  withSC3 reloadSynthdef
    where 
      synthdefs = 
          [("anOsc", pure anOsc),
           ("aLine", pure aLine),
           ("aLag", pure aLag),
           ("aNoise2", aNoise2)]

numOsc :: Int
numOsc = 128

oscIds :: [NodeId]
oscIds = mkIds 1001

ampGroup, freqGroup, panGroup, oscGroup, effectGroup :: NodeId
ampGroup = 10
freqGroup = 11
panGroup = 12
oscGroup = 20
effectGroup = 30

ampIds :: [NodeId]
ampIds = mkIds 2001

freqIds :: [NodeId]
freqIds = mkIds 3001

panIds :: [NodeId]
panIds = mkIds 4001

ampBusses :: [Int]
ampBusses = mkIds 1

freqBusses :: [Int]
freqBusses = mkIds 1001

panBusses :: [Int]
panBusses = mkIds 2001

mkIds :: Int -> [Int]
mkIds str = enumFromTo str (str+numOsc-1)

setAmps :: [Double] -> IO ()
setAmps = setVals ampIds

setFreqs :: [Double] -> IO ()
setFreqs = setVals freqIds

setPans :: [Double] -> IO ()
setPans = setVals panIds

setVals :: [NodeId] -> [Double] -> IO ()
setVals = setParams "val"

setDurs :: [NodeId] -> [Double] -> IO ()
setDurs = setParams "dur"

setParams :: String -> [NodeId] -> [Double] -> IO ()
setParams name ns ps = 
    mapM_ (\(n,p) -> withSC3 $ \t -> send t $ n_set n [(name,p)])
          (zip ns ps)

setParams' :: String -> [NodeId] -> [Double] -> [OSC]
setParams' name ns ps = zipWith (\n p ->  n_set n [(name,p)]) ns ps

tree :: SCTree
tree = 
  Group 0 
   [Group 1 
    [Group ampGroup amps,
     Group freqGroup freqs,
     Group panGroup pans,
     Group oscGroup oscs,
     Group effectGroup []]]

amps :: [SCTree]
amps = zipWith4 mkLag
       ampIds ampBusses (repeat 0.02) (repeat 1)
-- amps = zipWith5 mkNoise
--        ampIds ampBusses (repeat 0.02) (repeat 0.001) (repeat 0.2)

freqs :: [SCTree]
-- freqs = zipWith5 mkNoise2
 --         freqIds freqBusses (repeat 440) (repeat 20) (repeat 2)
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

mkNoise2 :: NodeId -> BusId -> Double -> Double -> Double -> SCTree
mkNoise2 nId bId val range freq = 
    Synth nId "aNoise2"
          ["out":=fromIntegral bId, 
           "val":=val, "range":=range, "freq":=freq]

anOsc :: UGen
anOsc = out 0 $ pan2 (sinOsc ar A.freq 0 * A.amp) A.pan 1

aLine :: UGen
aLine = out A.out $ line kr A.start A.end A.dur DoNothing

aLag :: UGen
aLag = out A.out $ lag A.val A.dur 

aNoise2 :: IO UGen
aNoise2 = do
  n <- lfNoise2 kr A.freq 
  return $ out A.out $ A.val + (n * A.range / 2)

partialPitches :: [Double] -> [Double]
partialPitches ps = zipWith (*) muls ps'
    where muls = concatMap (replicate (length ps)) [1..]
          ps' = cycle (map midiCPS ps)

-- Note of pitches.
-- [35,47,52,56,59,64] 
-- [36,48,52,55,60,64]

-- Rhythm: 7/8
-- x-x-xxx- x-xxx- 