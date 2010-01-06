{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
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


-- | Sends and updates synthdefs
updateSynthdefs :: IO ()
updateSynthdefs = mapM_ f synthdefList
    where
      f (n,u) = withSC3 . sendSynthdef n =<< u
-- -- updateSynthdefs :: Transport t => IO (t -> IO ())
-- updateSynthdefs = withSC3 =<< foldM f v synthdefList
--     where f _ (n,u) = fmap (sendSynthdef n) u
--           v = const (return ())

synthdefList :: [(String,IO UGen)]
synthdefList =
    [("tutorial_SinOsc",return tutorial_SinOsc),
     ("tutorial_SinOsc_stereo",return tutorial_SinOsc_stereo),
     ("tutorial_PinkNoise",tutorial_PinkNoise),
     ("tutorial_NoRand",tutorial_NoRand),
     ("tutorial_Rand",tutorial_Rand),
     ("tutorial_args",return tutorial_args),
     ("tutorial_Infreq",return tutorial_Infreq),
     ("tutorial_Outfreq",return tutorial_Outfreq),
     ("tutorial_DecayPink",tutorial_DecayPink),
     ("tutorial_DecaySin",return tutorial_DecaySin),
     ("tutorial_Reverb",tutorial_Reverb),
     ("tutorial_map",return tutorial_map),
     ("tutorial_DecaySin2",tutorial_DecaySin2),
     ("tutorial_Reverb2", tutorial_Reverb2),
     ("tutorial_PlayBuf", return tutorial_PlayBuf),
     ("tutorial_Buffer_cue", return tutorial_Buffer_cue),
     ("tutorial_RecordBuf", tutorial_RecordBuf),
     ("tutorial_playback", return tutorial_playback)]


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

groupTree01 :: SCTree
groupTree01 =
    Group 0
    [Group 1
     [Group 2 [],
      Group 3 []]]

-- | Stereo version of "tutorial_DecaySin"
tutorial_DecaySin2 :: IO UGen
tutorial_DecaySin2 = do
  pulseFreq <- rand 0.3 1
  pos <- rand (-1) 1
  let source = pan2 (decay2 (impulse ar pulseFreq 0.125) 0.3 1) pos 1 * s
      s = sinOsc ar (sinOsc kr 0.2 0 * 110 + ("freq" @= 440)) 0
      direct = "direct" @= 0.5
  return $ mrg [out ("outBus" @= 0) (source * direct),
                out ("effectBus" @= 1) (source * (1-direct))]

tutorial_Reverb2 :: IO UGen
tutorial_Reverb2 = do
  let input = in' 2 ar $ mce [("inBus1" @= 0),("inBus2" @= 1)]
      f i _ = do
        dt <- rand 0.001 0.04
        return $ allpassN i 0.04 dt 3
  output <- foldM f input [1..16]
  return $ out ("outBus" @= 0) output

groupTree02 :: SCTree
groupTree02 =
    Group 0
    [Group 1
     [Group sources
      [Synth (-1) "tutorial_DecaySin2"
       ["effectBus" := PVal bus1, "outBus" := PVal 0],
       Synth (-1) "tutorial_DecaySin2"
       ["effectBus" := PVal bus2, "outBus" := PVal 0,"freq" := PVal 660]]],
     Group effects
      [Synth (-1) "tutorial_Reverb2"
       ["inBus1" := PVal bus1,"inBus2" := PVal bus2]]]
    where
      bus1 = 1
      bus2 = 2
      sources = 2
      effects = 3

-- groupAsGroup :: SCTree
groupAsGroup = do
  let f n = do
        freqOffset <- constant <$> randomRIO (0,110::Double)
        pan <- constant <$> randomRIO (0,1::Double)
        let ug = out 0 $ pan2 (sinOsc ar (440 + freqOffset) 0 * 0.1) pan 1
        return $ sendSynthdef ("temp_" ++ show n) ug
  mapM_ (\n -> f n >>= withSC3) [1..4]
  let tree =
       Group 0
       [Group 1
        [Group 2
         (map (\n -> Synth (-1) ("temp_" ++ show n) []) [1..4])]]
  withSC3 $ mkTree tree

tutorial_PlayBuf :: UGen
tutorial_PlayBuf = out ("out" @= 0) $
                   playBuf 1 bufnum (bufRateScale kr bufnum) 1 0 NoLoop RemoveSynth
    where
      bufnum = "bufnum" @= 0

tutorial_Buffer_cue :: UGen
tutorial_Buffer_cue = out ("out" @= 0) $ diskIn 1 ("bufnum" @= 0) NoLoop

cueSound :: IO Int
cueSound =
    withSC3 $ do
      async' $ b_alloc bufnum 8192 1
      async' $ b_read bufnum "/home/atsuro/audio/wav/shot_shot_mono.wav" 0 (-1) 0 1
      s_new' "tutorial_Buffer_cue" [("bufnum",fromRational $ toRational $ bufnum)]
    where
      bufnum = 1

tutorial_RecordBuf :: IO UGen
tutorial_RecordBuf = do
  n <- pinkNoise ar >>=* (* 0.3)
  return $ recordBuf ("bufnum" @= 1) 0 1 0 1 NoLoop 1 n

goRecordBuf :: IO Int
goRecordBuf = do
  let bufnum = 1
  withSC3 $ \fd -> do
    async fd $ b_alloc bufnum (48000 * 5) 1
    s_new' "tutorial_RecordBuf" [("bufnum", fromIntegral bufnum)] $ fd

stopRecordBuf :: IO ()
stopRecordBuf = do
  let bufnum = 1
  withSC3 $ async' $ b_close bufnum
  withSC3 $ send' $ b_free bufnum

tutorial_playback :: UGen
tutorial_playback = mrg [out ("out" @= 0) pb, remove]
    where
      pb = playBuf 1 ("bufnum" @= 1) 1 1 0 NoLoop RemoveSynth
      remove = freeSelfWhenDone pb


diskInHelp =
    withSC3 $ \fd -> do
      async fd $ b_alloc 0 8192 n
      async fd $ b_read 0 f 0 (-1) 0 1
      play fd g
    where
      f = "/home/atsuro/audio/wav/shot_shot_mono.wav"
      n = 1
      g = out 0 $ diskIn n 0 NoLoop


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
