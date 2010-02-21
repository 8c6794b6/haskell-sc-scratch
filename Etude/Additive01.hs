------------------------------------------------------------------------------
-- Etude of additive synthesis.
--

module Etude.Additive01 where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List (zipWith4, zipWith5)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3

import SCQuery
import SCTree
import SCSched
import Reusable
import qualified Scratch.ControlArgs as A

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

mkChorus :: RandomGen g => [Double] -> g -> [Double]
mkChorus xs g = zipWith (*) (randomRs (0.985,1.015) g) xs

bundleAt :: Double -> [OSC] -> IO OSC
bundleAt t msgs = do
  now <- utcr
  return $ Bundle (UTCr $ now + t) msgs

cycleMM :: [Double] -> [Double]
cycleMM = map midiCPS . cycle

data Send g a = Send Double (g -> [a])

setAmps', setFreqs', setPans' :: [Double] -> [OSC]
setAmps' = setVals' ampIds
setFreqs' = setVals' freqIds
setPans' = setVals' panIds

setVals', setDurs' :: [NodeId] -> [Double] -> [OSC]
setVals' = setParams' "val"
setDurs' = setParams' "dur"

randomAmps :: RandomGen g => (Double,Double) -> g -> [OSC]
randomAmps range = \g -> setAmps' (randomRs range g)

randomFreqs :: RandomGen g => (Double,Double) -> g -> [OSC]
randomFreqs range = \g -> setFreqs' (randomRs range g)

chorusFreqs :: RandomGen g => [Double] -> g -> [OSC]
chorusFreqs ps = setFreqs' . mkChorus (cycleMM ps)

randomPans :: RandomGen g => (Double,Double) -> g -> [OSC]
randomPans range = \g -> setPans' (randomRs range g)

randomDurs :: RandomGen g => [NodeId] -> (Double,Double) -> g -> [OSC]
randomDurs ids range = \g -> setDurs' ids $ randomRs range g

goSend :: Send StdGen OSC -> IO ()
goSend (Send t f) =
    newStdGen >>= return . f >>= bundleAt t >>= withSC3 . flip send

n2 :: [Send StdGen OSC]
n2 =
    [Send 0 $ randomAmps (0.001,0.03),
     Send 0 $ randomDurs freqIds (1,4),
     Send 0 $ randomDurs panIds (0.5,8),
     Send 2 $ randomFreqs (200,8000),
     Send 4 $ randomPans (0.3,0.4),
     Send 8 $ randomFreqs (100,800),
     Send 10 $ randomAmps (0.001,0.03),
     Send 11 $ randomPans (-0.8,-0.81),
     Send 12 $ randomPans (0.3,0.31),
     Send 13 $ randomPans (-0.2,-0.21),
     Send 14 $ chorusFreqs [60,67,72,76],
     Send 14 $ randomPans (-1,1),
     Send 18 $ chorusFreqs [60,69,72,77],
     Send 18 $ randomPans (-1,1),
     Send 22 $ chorusFreqs [60,67,72,76],
     Send 22 $ randomPans (-1,1),
     Send 26 $ chorusFreqs [53,65,69,72],
     Send 26 $ randomPans (-1,1),
     Send 30 $ chorusFreqs [55,67,71,74],
     Send 30 $ randomPans (-1,1),
     Send 34 $ chorusFreqs [60,64,67,72],
     Send 34 $ randomPans (-1,1),
     Send 38 $ randomDurs ampIds (6,12),
     Send 38 $ const $ setAmps' (repeat 0),
     Send 42 $ randomAmps (0.001,0.03),
     Send 42 $ randomFreqs (100,800),
     Send 42 $ randomPans (-1,1),
     Send 46 $ randomFreqs (200,1200),
     Send 46 $ randomPans (-0.5,-0.3),
     Send 52 $ randomFreqs (600,800),
     Send 52 $ randomPans (-1,1),
     Send 54 $ randomFreqs (100,800),
     Send 54 $ randomPans (0.8,1),
     Send 56 $ randomFreqs (1000,12000),
     Send 57 $ randomPans (-1,-1),
     Send 58 $ randomFreqs (100,800),
     Send 59 $ randomPans (0.3,0.5),
     Send 60 $ randomFreqs (100,12000),
     Send 61 $ randomPans (-1,1),
     Send 62 $ randomFreqs (200,400),
     Send 62 $ const $ setAmps' (repeat 0),
     Send 66 $ randomAmps (0.001,0.03),
     Send 70 $ \g -> setVals' (choose freqIds (numOsc `div` 2) g)
                     (randomRs (100,8000) g),
     Send 74 $ \g -> setVals' (choose freqIds (numOsc `div` 2) g)
                     (randomRs (100,8000) g),
     Send 78 $ \g -> setVals' (choose freqIds (numOsc `div` 2) g)
                     (randomRs (100,8000) g),
     Send 82 $ \g -> setVals' (choose freqIds (numOsc `div` 2) g)
                     (randomRs (100,8000) g),
     Send 86 $ const $ setAmps' (repeat 0),
     Send 90 $ randomAmps (0.001,0.03),
     Send 90 $ randomDurs freqIds (0.5,2),
     Send 90 $ \g -> setVals' freqIds
                     (shuffle'
                      (take numOsc $
                            mkChorus (partialPitches
                                      [60,63,65,67,70,72]) g) g),
     Send 94 $ \g -> setVals' freqIds
                     (shuffle'
                      (take numOsc $
                            mkChorus (partialPitches
                                      [58,61,63,65,68,70]) g) g),
     Send 98 $ \g -> setVals' freqIds
                     (shuffle'
                      (take numOsc $
                            mkChorus (partialPitches
                                      [61,64,66,68,71,73]) g) g),
     Send 102 $ \g -> setVals' freqIds
                      (shuffle'
                       (take numOsc $
                             mkChorus (partialPitches
                                       [59,62,64,66,69,71]) g) g),
     Send 90 $ \g -> setVals' freqIds
                     (shuffle'
                      (take numOsc $
                            mkChorus (partialPitches
                                      [60,63,65,67,70,72]) g) g),
     Send 106 $ randomDurs ampIds (8,16),
     Send 106 $ const $ setAmps' (repeat 0)
    ]

-- Note of pitches.
-- [35,47,52,56,59,64]
-- [36,48,52,55,60,64]

-- Rhythm: 7/8
-- x-x-xxx- x-xxx-