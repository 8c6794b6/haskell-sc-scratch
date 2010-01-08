------------------------------------------------------------------------------
-- | Example from "Introductory Tutorial".
--

module SCHelp.Introductory where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import System.Random

import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.OpenSoundControl

import Reusable
import Instances
import SCTree

--
-- From "Synthesizing sound".
--

oneLineCode :: IO ()
oneLineCode = audition $ out 0 $
              sinOsc ar (mce [400,401]) 0 * 0.1 *
              (saw ar (mce [11,33]) + 1)*
              envGen kr 1 1 0 1 RemoveSynth (envSine 10 1)

moreThanOneLine :: IO ()
moreThanOneLine = do
  freq <- lfNoise0 kr 4 >>=* (* 700) >>=* (+ 1100)
  audition $ out 0 $
           rlpf (saw ar (mce [100,102]) * 0.15)
                (lag freq 0.1) 0.05

anExample :: UGen
anExample = out 0 source
    where
      source = pulse ar (mce [220,221.5] + sinOsc kr (mce [7,8]) 0 * 7)
               0.35 * 0.02

dataForABus :: UGen
dataForABus = out 0 $ saw ar 100 * 0.1

dataFromABus :: UGen
dataFromABus = out 0 $ mce [in' 1 ar 0,
                            sinOsc ar 440 0 * 0.2]

withControls :: UGen
withControls = out 0 $ sinOsc ar (mce [freq,freq+beatFreq]) 0 * mul
    where
      freq = "freq" @= 440
      beatFreq = "beatFreq" @= 1
      mul = "mul" @= 0.22

resetMyControls :: UGen
resetMyControls = out 0 $ sinOsc ar (mce [freq,freq+1]) 0 * mul
    where
      freq = "freq" @= 440
      mul = "mul" @= 0.22

controlsWithLags :: UGen
controlsWithLags = out 0 $ sinOsc ar (mce [freq1,freq2]) 0 * mul
    where
      freq1 = "freq1" @~ 440 $ 4
      freq2 = "freq2" @~ 443 $ 5
      mul = "mul" @= 0.12

ringMod :: UGen
ringMod = out 0 $ (sinOsc ar (mce [440.067,441.03]) 0) * 
          sinOsc ar (mce [111,109]) 0 * 0.2

modifyThisWith :: UGen
modifyThisWith = sinOsc ar 500 0 * 0.5

distortingIt :: UGen
distortingIt = distort modifyThisWith 

cubingIt :: UGen
cubingIt = cubed modifyThisWith

summingUGensWithBinOp :: UGen
summingUGensWithBinOp = sum $ map f [0..2]
    where f n = saw ar (500 + (n * 100)) * 0.05

summingUGensWithMix :: UGen
summingUGensWithMix = mixFill 3 f
    where f n = saw ar (500 + (constant n * 100)) * 0.05

scalingAndMixing :: IO UGen
scalingAndMixing = do
  w <- whiteNoise ar >>=* (* sinOsc kr 1 1)
  b <- brownNoise ar >>=* (* sinOsc kr 2 1)
  return $ w + b

withEnvelope :: UGen
withEnvelope = sinOsc ar 440 0 * 0.1 * 
               envGen kr 1 1 0 1 RemoveSynth (envSine 1 1)

timeScale :: UGen
timeScale = out 0 $ sinOsc ar 440 0 * 0.4 * e
    where 
      e = envGen kr 1 1 0 ("ts" @= 1) RemoveSynth (envSine 1 1)

tsRoutine :: IO ()
tsRoutine = mapM_ f [1..] where
    f _ = do
      ts <- randomRIO (0.01, 0.3)
      withSC3 $ s_new' "timeScale" [("ts",ts)]
      threadDelay (floor $ 0.5 * 1000 * 1000)

addOscs :: IO UGen
addOscs = do
  let f n = do 
            rf <- randomRIO (67.0::Double,2000) >>=* constant
            lf <- randomRIO (67.0::Double,2000) >>=* constant
            return $ sinOsc ar (mce [rf,lf]) 0 * 0.6 * 
                       (constant $ recip (fromIntegral (n+1))) *
                       envGen kr 1 1 0 1 RemoveSynth (envPerc 11 6)
      n = 12
  mixFillM n f

addOscs2 :: IO UGen
addOscs2 = (* envGen kr 1 0.6 0 1 RemoveSynth (envPerc 11 6)) <$> mixFillM n f
    where
      n = 16
      f n = do
        [rf,lf] <- newStdGen >>. randomRs (67.0::Double,2000) >>.
                   take 2 >>. map constant 
        envDur <- constant <$> randomRIO (2.0::Double,17)
        return $ sinOsc ar (mce [rf,lf]) 0 * 
               constant (recip $ fromIntegral (n+1)) * 
               envGen kr 1 1 0 1 DoNothing (envSine envDur 1)

ringModulation :: UGen
ringModulation 
    = out 0 $ sinOsc ar 440 0 * 0.571 * sinOsc kr 880 0 

ringModulationLFO :: UGen
ringModulationLFO 
    = out 0 $ sinOsc ar 440 0 * 0.571 *
      (sinOsc kr 880 0 * 
       sinOsc kr (mce $ map (constant . recip) [6.99,8.01]) 0)

amplitudeModulation :: UGen
amplitudeModulation 
    = out 0 $ sinOsc ar 440 0 * 0.571 * 
      (abs (sinOsc kr 880 0) *
       sinOsc kr (mce $ map (constant . recip) [6.99,8.01]) 0)
                            
compareClassic :: UGen
compareClassic 
    = out 0 $ sinOsc ar 440 0 * 0.571 * (abs $ sinOsc kr 880 0)

compareRing :: UGen
compareRing 
    = out 0 $ sinOsc ar 440 0 * 0.571 * sinOsc kr 880 0