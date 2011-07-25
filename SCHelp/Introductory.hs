------------------------------------------------------------------------------
-- | Example from "Introductory Tutorial".
--

module SCHelp.Introductory where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
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
      freq1 = "freq1" @@ 440 @~ 4
      freq2 = "freq2" @@ 443 @~ 5
      mul = "mul" @@ 0.12

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
      ["outBus":=3, "bufNum":=b,
       "rateScale":=1, "mul":=0.051]],
     Group 3
     [Synth (-1) "ampMod"
      ["inBus":=3, "outBus":=5, "modFreq":=1],
      Synth (-1) "aLowPassFilter"
      ["inBus":=5, "outBus":=0, "boost":=5]]]]
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

someGrains :: IO UGen
someGrains = do
  let gate = impulse kr ("grainFreq" @= 2) 0
      shape = envSine 0.1 1
  n <- lfNoise0 kr 4 >>. (* "freqDev" @= 200) >>. (+ "centerFreq" @= 777)
  return $ out 0 $ sinOsc ar n 0 * envGen kr gate 1 0 1 DoNothing shape

aDelay :: UGen
aDelay = out 1 delayed
    where
      dt = "delay" @= 0.25
      delayed = delayN (in' 1 ar 0) dt dt

grainDelayed :: SCTree
grainDelayed =
    Group 0
    [Group 1
     [Group 2
      [Synth (-1) "someGrains" []],
      Group 3
      [Synth (-1) "aDelay" []]]]

combExample1 :: IO UGen
combExample1 = do
  freq <- randomRIO (500,1000::Double) >>. constant
  let input = sinOsc ar freq 0 * 0.2 * line kr 1 0 0.1 DoNothing
      combed = combN input 0.3 0.25 6
  return $ out 0 combed

allpassExample1 :: IO UGen
allpassExample1 = do
  freq <- randomRIO (500,1000::Double) >>. constant
  let input = sinOsc ar freq 0 * 0.2 * line kr 1 0 0.1 DoNothing
      allpassed = allpassN input 0.3 0.25 6
  return $ out 0 allpassed

combExample2 :: IO UGen
combExample2 = do
  freq <- randomRIO (500,1000::Double) >>. constant
  let input = sinOsc ar freq 0 * 0.2 * line kr 1 0 0.1 DoNothing
      combed = combN input 0.1 0.025 6
  return $ out 0 combed

allpassExample2 :: IO UGen
allpassExample2 = do
  freq <- randomRIO (500,1000::Double) >>. constant
  let input = sinOsc ar freq 0 * 0.2 * line kr 1 0 0.1 DoNothing
      combed = combN input 0.1 0.025 6
  return $ out 0 combed

whySC :: IO UGen
whySC = do
  let f a b = do
          input <- dust ar 0.2 >>. (*50)
          freqOffset <- randomRIO (0,3000::Double) >>. constant
          return $ a + resonz input (200+freqOffset) 0.003
  s <- foldM f 0 [1..10]
  let z = delayN s 0.048 0.048
      g a b = do
          noiseFreq <- randomRIO (0,0.1::Double) >>. constant
          dct <- lfNoise1 kr noiseFreq >>. (*0.04) >>. (+0.05)
          return $ a + combN z 0.1 dct 15
  y <- foldM g 0 [1..7]
  let g' a b = do
          dtL <- randomRIO (0,0.050::Double) >>. constant
          dtR <- randomRIO (0,0.050::Double) >>. constant
          return $ allpassN a 0.050 (mce [dtL,dtR]) 1
  y' <- foldM g' y [0..4]
  return $ out 0 $ s + (0.2*y')

filteredDust :: IO UGen
filteredDust = do
  let f a b = do
        input <- dust ar 0.2 >>. (* 50)
        freq <- rand 200 3200
        return $ a + resonz input freq 0.003
  output <- foldM f 0 [1..10]
  return $ out 2 output

preDelay :: IO UGen
preDelay = return $ replaceOut 4 $ delayN (in' 1 ar inBus) 0.048 0.048
    where
      inBus = "inBus" @= 2

combs :: IO UGen
combs = do
  let f a _ = do
          nFreq <- rand 0 0.1
          dt <- lfNoise1 kr  nFreq >>. (* 0.04) >>. (+0.05)
          return $ a + combN (in' 1 ar 4) 0.1 dt 15
  output <- foldM f 0 [1..7]
  return $ replaceOut 6 output

allpasses :: IO UGen
allpasses = do
  let source = in' 1 ar 6
      gain = "gain" @= 0.2
      f a _ = do
              dtL <- rand 0 0.05
              dtR <- rand 0 0.05
              return $ allpassN a 0.050 (mce [dtL,dtR]) 1
  output <- foldM f source [1..8]
  return $ replaceOut 8 $ source * gain

theMixer :: IO UGen
theMixer = return $ replaceOut 0 $
           (in' 1 ar 2 + in' 2 ar 8) * ("gain" @= 1)

loadJMCs :: IO ()
loadJMCs
    = mapM_ (\(n, u) -> withSC3 . loadSynthdef n =<< u)
      [("filteredDust",filteredDust),
       ("preDelay", preDelay),
       ("combs",combs),
       ("allpasses",allpasses),
       ("theMixer",theMixer)]

whySCTree :: SCTree
whySCTree =
    Group 0
    [Group 1
     [Group 2
        [Synth (-1) "filteredDust" []],
      Group 3
        [Synth (-1) "preDelay" [],
         Synth (-1) "combs" [],
         Synth (-1) "allpasses" []],
      Group 4
        [Synth (-1) "theMixer" []]]]

fm1 :: IO UGen
fm1 = do
  idxOffset <- lfNoise1 kr (constant $ recip 5) >>. abs
  let bus = "bus" @= 0
      freq = "freq" @= 440
      carPartial = "carPartial" @= 1
      modPartial = "modPartial" @= 1
      index = "index" @= 3
      mul = "mul" @= 0.05
      mod = sinOsc ar (freq * modPartial) 0 * freq * index * idxOffset
      car = sinOsc ar ((freq * carPartial) + mod) 0 * mul
  return $ out bus car

fmNodes01 :: SCTree
fmNodes01 =
    Group 0
    [Group 1
     [Synth (-1) "fm1" ["bus" := 0, "freq" := 440,
                        "carPartial" := 1, "modPartial" := 2.4],
      Synth (-1) "fm1" ["bus" := 1, "freq" := 442,
                        "carPartial" := 1, "modPartial" := 2.401]]]

fmReverbNodes :: SCTree
fmReverbNodes =
    Group 0
    [Group 1
     [Group 2
      [Synth (-1) "fm1" ["bus" := 2, "freq" := 440, "carPartial" := 1,
                         "modPartial" := 1.99, "mul" := 0.071],
       Synth (-1) "fm1" ["bus" := 2, "freq" := 442, "carPartial" := 1,
                         "modPartial" := 2.401, "mul" := 0.071]],
      Group 3
      [Synth (-1) "preDelay" [],
       Synth (-1) "combs" [],
       Synth (-1) "allpass" [],
       Synth (-1) "theMixer" ["gain" := 0.64]]]]


carrier :: UGen
carrier = out outBus $ sinOsc ar ((freq * carPartial) + mod) 0 * mul
    where
      outBus = "outBus" @= 0
      inBus = "inBus" @= 2
      mod = in' 1 ar inBus
      freq = "freq" @= 440
      carPartial = "carPartial" @= 1
      index = "index" @= 3
      mul = "mul" @= 0.2

modulator :: IO UGen
modulator = do
  nf <- rand 3 6 >>. recip >>. abs
  n <- lfNoise1 kr nf
  let outBus = "outBus" @= 2
      freq = "freq" @= 440
      modPartial = "modPartial" @= 1
      index = "index" @= 3
  return $ out outBus $ sinOsc ar (freq * modPartial) 0 * freq * n * index

loadFmComponents :: IO ()
loadFmComponents = do
  withSC3 $ loadSynthdef "carrier" carrier
  withSC3 . loadSynthdef "modulator" =<< modulator
  return ()

fmComponentTree :: SCTree
fmComponentTree =
  Group 0
  [Group 1
   [Group 2
    [Synth (-1) "modulator"
     ["outbus" := 2, "freq" := freq, "modPartial" := 0.649, "index" := 2],
     Synth (-1) "modulator"
     ["outbus" := 2, "freq" := freq, "modPartial" := 1.683, "index" := 2.31],
     Synth (-1) "modulator"
     ["outbus" := 4, "freq" := freq, "modPartial" := 0.729, "index" := 1.43],
     Synth (-1) "modulator"
     ["outbus" := 4, "freq" := freq, "modPartial" := 2.19, "index" := 1.76]],
    Group 3
    [Synth (-1) "carrier"
     ["inBus" := 2, "outBus" := 0, "freq" := freq, "carPartial" := 1],
     Synth (-1) "carrier"
     ["inBus" := 4, "outBus" := 1, "freq" := freq, "carPartial" := 0.97]]]]
    where
      freq = 440

reverbedFmComponentTree :: IO SCTree
reverbedFmComponentTree = do
  freq <- randomRIO (330,500)
  let tree =
       Group 0
       [Group 1
        [Group 2
         [Synth (-1) "modulator"
          ["outbus" := 60, "freq" := freq, "modPartial" := 0.649, "index" := 2],
          Synth (-1) "modulator"
          ["outbus" := 60, "freq" := freq, "modPartial" := 1.683, "index" := 2.31],
         Synth (-1) "modulator"
          ["outbus" := 62, "freq" := freq, "modPartial" := 1.11, "index" := 1.43],
          Synth (-1) "modulator"
          ["outbus" := 62, "freq" := freq, "modPartial" := 0.729, "index" := 1.76]],
         Group 3
         [Synth (-1) "carrier"
          ["inBus" := 60, "outBus" := 100, "freq" := freq, "carPartial" := 1],
          Synth (-1) "carrier"
          ["inBus" := 62, "outBus" := 100, "freq" := freq+1, "carPartial" := 2.91]],
         Group 4
         [Synth (-1) "preDelay" ["inBus" := 100],
          Synth (-1) "combs" [],
          Synth (-1) "allpasses" [],
          Synth (-1) "theMixer" ["gain" := 0.2]]]]
  return tree

