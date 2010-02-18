------------------------------------------------------------------------------
-- Etude of additive synthesis.
--

module Etude.Additive01 where

import Control.Applicative
import Control.Concurrent
import Data.List (zipWith4)
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
      synthdefs = [("anOsc", anOsc)]

numOsc :: Int
numOsc = 128

ampBusses :: [Int]
ampBusses = [1..numOsc]

freqBusses :: [Int]
freqBusses = [1001..(1000+numOsc)]

panBusses :: [Int]
panBusses = [2001..(2000+numOsc)]

setAmps :: [Double] -> IO ()
setAmps = setParams ampBusses

setFreqs :: [Double] -> IO ()
setFreqs = setParams freqBusses

setPans :: [Double] -> IO ()
setPans = setParams panBusses

setParams :: [Int] -> [Double] -> IO ()
setParams busIds params = 
    withSC3 $ \t -> send t $ c_set $ zip busIds params

setParamsLinear :: LineUGen -> [Int] -> [Double] -> IO ()
setParamsLinear ug busIds values = do
  valuePairs <- fmap reverse (cValue busIds)
  let ug' = \x y z -> ug kr x y z RemoveSynth
      ug'' = zipWith (\(x,y) z -> 
                          out (constant x) $ 
                          ug' (constant y) (constant z) 1)
             valuePairs values
  mapM_ audition ug''
    
tree :: SCTree
tree = mkOscTree ampBusses freqBusses panBusses

mkOscTree :: [BusId] -> [BusId] -> [BusId] -> SCTree
mkOscTree amps freqs pans = 
  Group 0 
   [Group 1 
    [Group 2 [],
     Group 10 (zipWith4 mkAnOsc [1000..] amps freqs pans),
     Group 20 []]]

mkAnOsc :: NodeId -> BusId -> BusId -> BusId -> SCTree
mkAnOsc nId amp freq pan = 
    Synth nId "anOsc" ["amp":<-amp, "freq":<-freq, "pan":<-pan]

anOsc :: UGen
anOsc = out 0 $ pan2 (sinOsc ar A.freq 0 * A.amp) A.pan 1


-- Getting value from control bus.
cValue :: [Int] -> IO [(Int,Double)]
cValue busIds = f =<< withSC3 (c_get' busIds) 
    where 
      f (Message "/c_set" vs) = return (g [] vs)
      f (Message _ _) = error "Message was not \"/c_set\""
      f _ = error "Not implemented for Bundle returned value."
      g xs [] = xs
      g xs (Int n:Float v:vs) = g ((n,v):xs) vs
