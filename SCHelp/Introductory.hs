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
addOscs2 = mixFillM n f >>. (* envGen kr 1 0.6 0 1 RemoveSynth (envPerc 11 6))
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

subtractive :: UGen
subtractive
    = out 0 $ lpf (pulse ar 440 0.5 * 0.1) (line kr 8000 660 6 DoNothing)

passLowFreq2 :: IO UGen
passLowFreq2 = do
  freqOffset <- lfNoise0 kr 1 >>. (*100) >>. (+200)
  cutFreqL <- lfNoise0 kr 4 >>. (*600) >>. (+2400)
  cutFreqR <- lfNoise0 kr 3 >>. (*600) >>. (+2400)
  return $ out 0 $
       rlpf (saw ar (mce [220,221] + freqOffset))
               (mce [cutFreqL,cutFreqR]) 0.1

noiseToPitch :: IO UGen
noiseToPitch = do
  src <- whiteNoise ar >>. (* "mul" @= 1)
  freq <- lfNoise0 kr 4 >>. (*110) >>. (+660)
  return $ out 0 $ resonz src freq (mce [0.005,0.005])

firstNode_source :: UGen
firstNode_source = out 0 $ saw ar (mce [200,201]) * 0.05

secondNode_filter :: IO UGen
secondNode_filter = do
  n <- lfNoise0 kr (mce [4,4.001]) >>. (*500) >>. (+1000)
  return $ replaceOut 0 $ lpf (in' 2 ar 0) (lag n 0.1)

filterAndSource :: SCTree
filterAndSource =
    Group 0
    [Group 1
     [Synth (-1) "firstNode_source" [],
      Synth (-1) "secondNode_filter" []]]

synthNumber1 :: IO UGen
synthNumber1 = do
  let mul = "mul" @~ 0.2 $ 0.1
  bn <- brownNoise ar >>. (*mul)
  ln <- lfNoise0 kr (mce [1,1.01])
  return $ out 0 $ bn * ln

synthNumber2 :: IO UGen
synthNumber2 = do
  let mul = "mul" @~ 0.2 $ 0.1
  wn <- whiteNoise ar >>. (*mul)
  ln <- lfNoise1 kr (mce [2.99,3])
  return $ out 0 $ wn * ln

synthNumber3 :: IO UGen
synthNumber3 = do
  let mul = "mul" @~ 0.2 $ 0.1
  pn <- pinkNoise ar >>. (* mul)
  ln <- lfNoise2 kr (mce [0.79,0.67])
  return $ out 0 $ pn * ln

groupWithParams :: SCTree
groupWithParams
    = Group 0
      [Group 1
       [Synth (-1) "synthNumber1" [],
        Synth (-1) "synthNumber2" [],
        Synth (-1) "synthNumber3" []]]

aMonoSamplePlayer :: UGen
aMonoSamplePlayer =
    out bus $ playBuf 1 bufnum (bufRateScale kr bufnum * rateScale) 1 0
        NoLoop RemoveSynth * e
    where
      e = envGen kr 1 1 0 1 DoNothing (envSine (bufDur kr bufnum) 1)
      bus = "bus" @= 0
      bufnum = "bufnum" @= 0
      rateScale = "rateScale" @= 1

playMonoFile :: IO ()
playMonoFile = do
  withSC3 $ \fd -> do
      async fd $ b_allocRead 0 "/home/atsuro/audio/wav/a11wlk01.wav" 0 0
      send fd $ s_new "aMonoSamplePlayer" (-1) AddToTail 1 []

simpleComponentReuse :: IO ()
simpleComponentReuse = do
  withSC3 $ \fd -> do
    async fd $ b_allocRead 0 "/home/atsuro/audio/wav/a11wlk01.wav" 0 0
    send fd $ s_new "aMonoSamplePlayer" (-1) AddToTail 1 [("bus",0),("rateScale",0.99)]
    send fd $ s_new "aMonoSamplePlayer" (-1) AddToTail 1 [("bus",1),("rateScale",1.01)]


aLoopingSamplePlayer :: IO UGen
aLoopingSamplePlayer = do
  let outBus = "outBus" @= 0
      bufnum = "bufnum" @= 0
      rateScale = "rateScale" @= 1
      mul = "mul" @= 1
  n <- lfNoise0 kr (constant $ recip 2) >>. (*0.05)
  return $ out ("outBus" @= 0) $
         playBuf 1 bufnum (bufRateScale kr bufnum * rateScale + n) 1 0 
                 Loop DoNothing * mul

ampMod :: UGen
ampMod = out outBus $ mce [in' 1 ar inBus * sinOsc kr modFreq 0,
                           in' 1 ar inBus * sinOsc kr (modFreq - 0.02) 0]
    where
      inBus = "inBus" @= 0
      outBus = "outBus" @= 0
      modFreq = "modFreq" @= 1

aLowPassFilter :: IO UGen
aLowPassFilter = do
  let outBus = "outBus" @= 0
      inBus = "inBus" @= 0 
      freq = "freq" @= 300
      freqDev = "freqDev" @= 50
      boost = "boost" @= 1
  bw <- lfNoise0 kr 1 >>. (*freqDev) >>. (+freq)
  return $ out outBus $ rlpf (in' 2 ar inBus) (lag bw 1) 0.2 * boost *
         lfPulse kr 1 (mce [0.25,0.75]) (mce [0.5,0.45]) +
         in' 2 ar inBus

chainTree01 :: SCTree
chainTree01
 = Group 0
   [Group 1
    [Group 2
     [Synth (-1) "aLoopingSamplePlayer" 
      ["outBus":=PVal 3, "bufNum":=PVal b, 
       "rateScale":=PVal 1, "mul":=PVal 0.051]],
     Group 3
     [Synth (-1) "ampMod"
      ["inBus":=PVal 3, "outBus":=PVal 5, "modFreq":=PVal 1],
      Synth (-1) "aLowPassFilter"
      ["inBus":=PVal 5, "outBus":=PVal 0, "boost":=PVal 5]]]]
    where
      b = 0

setChainTree01Randomly :: IO ()
setChainTree01Randomly = do
  rs <- randomRIO (0.95,1.05)
  mul <- randomRIO (0.051,0.07)
  modFreq <- randomRIO (800,1200)
  freq <- randomRIO (500,700)
  freqDev <- randomRIO (180,210)
  let msg = [n_set 2 [("rateScale",rs),("mul",mul)],
             n_set 3 [("modFreq",modFreq)],
             n_set 3 [("freq",freq),("freqDev",freqDev),("boost",7)]]
  withSC3 $ send' $ Bundle (NTPi 0) msg

    