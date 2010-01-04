------------------------------------------------------------------------------
-- | Example shown in "Getting Started with SC".
--

module SCHelp.GettingStarted where

import Reusable
import SCTree
import Instances

import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.OpenSoundControl

import Control.Applicative
import Control.Monad
import System.Random

data Chan a = Mono a
            | Stereo a a

nowConsiderThis :: IO ()
nowConsiderThis = do
  let freqs = [Stereo 660 880, Stereo 440 660, Mono 1320, Mono 880]
      getFreq (Mono a) = a
      getFreq (Stereo a b) = mce2 a b
  freq <- (getFreq . (freqs !!)) `fmap` randomRIO (0,length freqs - 1)
  audition $ out 0 $ sinOsc ar freq 0 * 0.2

panningPinkNoise :: IO ()
panningPinkNoise = do
  n <- pinkNoise ar
  let pos = sinOsc kr 0.5 0
  audition $ out 0 $ pan2 n pos 1

slightlyLeftPinkNoise :: IO ()
slightlyLeftPinkNoise = do
  n <- (* 0.2) <$> pinkNoise ar
  audition $ out 0 $ pan2 n (-0.3) 1

noiseAndSinAndSaw :: IO ()
noiseAndSinAndSaw = do
  n <- (* 0.2) <$> pinkNoise ar
  let s = sinOsc ar 440 0 * 0.2
      t = saw ar 660 * 0.2
  audition $ out 0 $ n + s + t

oneChannelMix :: IO ()
oneChannelMix = audition $ out 0 sound
    where sound = mix $ mce [sinOsc ar 440 0 * 0.2, saw ar 660 * 0.2]

twoChannelMix :: IO ()
twoChannelMix = audition $ out 0 $ mix $ mce2 a b
    where a = mce [sinOsc ar 440 0 * 0.2, saw ar 662 * 0.2]
          b = mce [sinOsc ar 442 0 * 0.2, saw ar 660 * 0.2]

mixFillEx :: IO UGen
mixFillEx = fmap (out 0) (mixFillM 8 f)
    where f n = randomRIO (0,500::Double) >>= \offset ->
                return $ sinOsc ar (500 + constant offset) 0 *
                           (1/constant (n+1)) * 0.3


-- | To write ugen to synthdef, invoke:
--
-- > writeSynthdef "tutorial_SinOsc" tutorial_SinOsc
--
-- And then to reload synthdefs, invoke:
--
-- > withSC3 reloadSynthdef
--
-- or to send the synthdef without writing to file:
--
-- > withSC3 $ sendSynthdef "tutorial_SinOsc" tutorial_SinOsc
--
tutorial_SinOsc :: UGen
tutorial_SinOsc = out 0 $ sinOsc ar 440 0 * 0.2

tutorial_SinOsc_stereo :: UGen
tutorial_SinOsc_stereo = out 0 $ outArray
    where outArray = mce [sinOsc ar 440 0*0.2,sinOsc ar 442 0*0.2]

tutorial_PinkNoise :: IO UGen
tutorial_PinkNoise = out 0 . (* 0.3) <$> pinkNoise ar

tutorial_NoRand :: IO UGen
tutorial_NoRand = do
  offset <- randomRIO (0,200::Double)
  return $ out 0 $ sinOsc ar (440 + constant offset) 0 * 0.2

tutorial_Rand :: IO UGen
tutorial_Rand = do
  freq <- rand 440 660
  return $ out 0 $ sinOsc ar freq 0 * 0.2

tutorial_args :: UGen
tutorial_args = out out' $ sinOsc ar freq 0 * 0.2
    where out' = control kr "out" 0
          freq = control kr "freq" 440

tutorial_Infreq :: UGen
tutorial_Infreq = out 0 $ sinOsc ar freq 0 * 0.5
    where
      freq = in' 1 kr (control kr "bus" 1) + freqOffset
      freqOffset = control kr "freqOffset" 0

tutorial_Outfreq :: UGen
tutorial_Outfreq = out bus $ sinOsc kr 1 0 * (freq/40) + freq
    where 
      bus = control kr "bus" 1
      freq = control kr "freq" 400

tutorial_DecayPink :: IO UGen
tutorial_DecayPink = do
  n <- pinkNoise ar 
  let source = decay2 (impulse ar 1 0.25) 0.01 0.2 * n
      outBus = control kr "outBus" 0
      effectBus = control kr "effectBus" 1
      direct = control kr "direct" 0.5
  return $ mrg [out outBus $ source * direct, 
                out effectBus $ source * (1-direct)]

tutorial_DecaySin :: UGen
tutorial_DecaySin = mrg [out 0 $ source * direct,
                         out effectBus $ source * (1-direct)]
    where
      outBus = control kr "outBus" 0
      effectBus = control kr "effectBus" 0
      direct = control kr "direct" 0.5
      source = decay2 (impulse ar 0.3 0.25) 0.3 1 * s
      s = sinOsc ar (sinOsc kr 0.2 0 * 110 + 440) 0

tutorial_Reverb :: IO UGen
tutorial_Reverb = do
  let inBus = control kr "inBus" 1
      outBus = control kr "outBus" 0
      input = in' 1 ar inBus
      f x _ = do 
        [r,l] <- fmap (take 2 . randomRs (0.001,0.04::Double)) newStdGen
        return $ allpassC x 0.04 (mce $ map constant [r,l]) 3
  reverbed <- foldM f input [1..16]
  return $ out outBus reverbed

busExTree :: SCTree
busExTree 
    = Group 0 
      [Group 1
       [Synth (-1) "tutorial_DecayPink" 
        ["effectBus" := b],
        Synth (-1) "tutorial_DecaySin" 
        ["effectBus" := b, "outBus" := PVal 1],
        Synth (-1) "tutorial_Reverb"
        ["inBus" := b]]]
    where b = PVal 1

tutorial_map :: UGen
tutorial_map = out 0 $ sinOsc ar (mce [freq1,freq2]) 0 * 0.1
    where 
      freq1 = "freq1" @= 440
      freq2 = "freq2" @= 440

tutorial_map_control :: UGen
tutorial_map_control = out ("bus" @= 1) $ sinOsc kr 1 0 * 50 + 880


-- | From in' help file.
-- connect something to system's input in jackd.
inEx01 :: IO ()
inEx01 = audition (out 0 (i+d)) 
    where i = in' 2 ar numOutputBuses
          d = mixFill 8 $ \n -> combN i 0.5 (1/constant n) 0.3

-- From help file of hsc3-dot, hsc3-dot.help.lhs

drawHelp1 :: IO ()
drawHelp1 = draw (out 0 (sinOsc ar 440 0 * 0.1))

drawHelp2 :: IO ()
drawHelp2 = draw $ out 0 $ sinOsc ar f 0 * 0.1
    where f = lfSaw kr 1 0 * 220 + 440

drawHelp3 :: IO UGen
drawHelp3 = do
  l <- rand 200 400
  m <- rand l 600
  a <- rand 500 900
  let f = lfSaw kr 1 0 * m + a
  return $ out 0 $ sinOsc ar f 0 * 0.1

drawHelp4 :: UGen
drawHelp4 = out 0 $ sinOsc ar f 0 * 0.1
    where f = control kr "freq" 440

drawHelp5 :: UGen
drawHelp5 = out 0 $ sinOsc ar f 0 * 0.1
    where f = mce2 440 220

-- | With multiple root graph
drawHelp6 :: UGen
drawHelp6 = mrg [out 0 o1, out 0 o2]
    where f = mce2 440 220 + in' 2 kr 0
          o1 = sinOsc ar f 0 * 0.1
          o2 = sinOsc kr (mce2 0.25 0.35) 0 * mce2 10 15
