------------------------------------------------------------------------------
-- | From "Tutorial" help file.
--

module SCHelp.Tutorial where

import Control.Applicative
import Control.Concurrent (forkIO, killThread,ThreadId)
import Control.Monad
import Data.List (transpose)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import System.Random

import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.OpenSoundControl
import FRP.Reactive

import Reusable
import Instances
import SCTree
import SCQuery
import SCSched
import qualified Scratch.ControlArgs as Arg

tish :: IO UGen
tish = do
  let trg = decay2 (impulse ar Arg.rate 0 * 0.3) 0.01 0.3
  osc <- clone 2 $ whiteNoise ar >>. (* trg)
  return $ out 0 osc

echo :: UGen
echo = replaceOut 0 $ (combN input 0.5 Arg.delay Arg.decay) + input
    where input = in' 2 ar 0

echoTree :: SCTree
echoTree =
  Group 0
  [Group 1
   [Synth (-1) "tish" ["freq":=200, "rate":=1.2],
    Synth (-1) "echo" ["delay":=0.1, "decay":=4]]]

tutorial_sine :: UGen
tutorial_sine =
    out 0 $ sinOsc ar Arg.freq 0 * 0.2

tutorial_line :: UGen
tutorial_line =
    replaceOut Arg.i_bus $
    line kr Arg.i_start Arg.i_end Arg.i_time RemoveSynth

mappingTree :: SCTree
mappingTree =
    Group 0
    [Group 1
     [Synth 1000 "tutorial_sine" ["freq":<-10],
      Synth (-1) "tutorial_line" []]]

tutorial_saw :: UGen
tutorial_saw = out Arg.out $ pan2 source Arg.pan 1
    where
      source = rlpf (saw ar (mce [Arg.freq,Arg.freq*2]) * Arg.amp)
               Arg.cutoff Arg.rezz

tutorial_envsaw :: UGen
tutorial_envsaw = out Arg.out $ pan2 source Arg.pan 1
    where
      source = rlpf (saw ar (lag Arg.freq 0.1) * env) Arg.cutoff Arg.rezz
               * Arg.amp
      env = envGen kr Arg.amp 1 0 1 DoNothing (envPerc 0.01 Arg.sustain)

tutorial_delay :: UGen
tutorial_delay = out Arg.out $ combN input 0.5 Arg.delay Arg.decay + input
    where
      input = in' 2 ar Arg.out

tutorial_sampler :: UGen
tutorial_sampler = out Arg.out $ pan2 source Arg.pan 1
    where
      source = playBuf 1 Arg.bufnum Arg.rate (inTrig 1 Arg.trig)
               0 NoLoop RemoveSynth * e * Arg.amp
      e = envGen kr 1 1 0 1 RemoveSynth (envPerc 0.01 1)

sawBufnum :: Num a => a
sawBufnum = 1

sawTree :: SCTree
sawTree =
    Group 0
     [Group 1
      [Synth 1000 "tutorial_saw"
        [],
       Synth 1002 "tutorial_delay"
        ["out":=0, "delay":=0.4, "decay":=14]]]

sawSetup :: Query ()
sawSetup = do
  freeAll
  mapM (uncurry load)
          [("tutorial_saw",tutorial_saw),
           ("tutorial_envsaw",tutorial_envsaw),
           ("tutorial_delay",tutorial_delay),
           ("tutorial_sampler",tutorial_sampler)]
  msg $ b_read sawBufnum "/home/atsuro/audio/wav/a11wlk01.wav" 0 0 0 0
  add 0 sawTree

sawEvent01 :: IO (Event OSC)
sawEvent01 = do
  gen <- newStdGen
  let waits = scanl (+) 0 $ repeat 2
      freqs = cycle [30,40,42,40]
      cutoffs = randomRs (500,1500) gen
      g freq cutoff = n_set 1000
                      [("freq",midiCPS freq),("cutoff",cutoff),
                       ("rezz",0.5),("amp",0.1),("out",0)]
  return $ listE $ zip waits $ zipWith g freqs cutoffs

sawEvent02 :: IO (Event OSC)
sawEvent02 = do
  gen <- newStdGen
  let waits = scanl (+) 0 $ repeat 2
      rates = randomRs (0, 0.5) gen
      g rate wait = s_new "tutorial_sampler" (-1) AddBefore 1002
                    [("bufnum",sawBufnum),("trig",1),
                     ("amp",0.1),("rate",rate),
                     ("sustain", wait),("pan",0)]
  return $ listE $ zip waits $ zipWith g rates waits

runSawEx :: IO ()
runSawEx = do
  query sawSetup s
  pure mappend <*> sawEvent01 <*> sawEvent02 >>= spawn 0 240

--
-- From "sappy emo electronica example".
--
patternefx_Ex :: UGen
patternefx_Ex = out (mrg [0,1]) $ audio + efx
    where
      audio = in' 2 ar (mce [20,21])
      efx = combN audio 0.5 (mce [0.24,0.4]) 2

pattern_Ex :: UGen
pattern_Ex = out Arg.out $ pan2 rezzed Arg.pan 1
    where
      rezzed = rlpf (pulse ar Arg.freq 0.05) Arg.cut Arg.rez * env
      env = envGen kr Arg.gate Arg.amp 0 1 RemoveSynth shape
      shape = envLinen 0.01 1 0.3 1 [EnvLin, EnvLin, EnvLin]

bass_Ex :: UGen
bass_Ex = out Arg.out $ pan2 source Arg.pan 1
    where
      source = rlpf (sinOsc ar Arg.freq 0.05) Arg.cut Arg.rez * env
      env = envGen kr Arg.gate Arg.amp 0 1 RemoveSynth shape
      shape = envLinen 0.01 1 0.3 1 [EnvLin, EnvLin, EnvLin]

updateSappyUGens :: Query ()
updateSappyUGens = do
  mapM_ (uncurry load)
        [("patternefx_Ex", patternefx_Ex),
         ("pattern_Ex", pattern_Ex),
         ("bass_Ex",bass_Ex)]

sappyTree :: SCTree
sappyTree =
    Group 0
     [Group 1
      [Group 10
       [],
       Group 11
       [Synth (-1) "patternefx_Ex" []]]]

sappyMap :: IO (Map String [Double])
sappyMap = do
  gen <- newStdGen
  return $ M.fromList
         [("out", repeat 20),
          ("gate", repeat 1),
          ("pan", repeat 1),
          ("amp", repeat 0.12),
          ("freq", cycle [0,4,7,11,14,17,7,2]),
          ("cut", randomRs (300,2000) gen),
          ("rez", randomRs (0.3,1) gen)]

-- sappyEv00 :: Double -> IO (Event OSC)
-- sappyEv00 offset = do
--   gen <- newStdGen
--   let times = scanl (+) 0 $ repeat 2
--       pitches = cycle [0,4,7,11,14,17,7,2]
--       cuts = randomRs (300,2000) gen
--       rezz = randomRs (0.3,1) gen
--       g p c r = s_new "pattern_Ex" (-1) AddToTail 10
--                 [("out",20),
--                  ("gate",1),
--                  ("pan",1),
--                  ("amp",0.12),
--                  ("freq",midiCPS $ p + offset),
--                  ("cut",c),
--                  ("rez",r)]
--   return $ listE $ zip times (zipWith3 g pitches cuts rezz)

-- sappyEv00' :: Double -> IO (Event OSC)
-- sappyEv00' offset = do
--     gen <- newStdGen
--     let t = [0,2..]
--         m = M.fromList
--             [("out", repeat 20),
--              ("gate", repeat 1),
--              ("pan", repeat 1),
--              ("amp", repeat 0.12),
--              ("freq", map (midiCPS . (+ offset)) $ cycle [0,4,7,11,14,17,7,2]),
--              ("cut", randomRs (300,2000) gen),
--              ("rez", randomRs (0.3,1) gen)]
--     return $ listE $ zip t $ mkSNew' "pattern_Ex" 10 m

-- sappyEv01 :: IO (Event OSC)
-- sappyEv01 = sappyEv00' 36

-- sappyEv015 :: IO (Event OSC)
-- sappyEv015 = sappyEv00' 43

-- sappyEv02 :: Event OSC
-- sappyEv02 = listE $ zip times oscs
--     where
--       times = fmap (+ 0.5) $ scanl (+) 0 $ repeat 2
--       pitches = cycle [0,4,7,11,14,17,7,2]
--       oscs = map g pitches
--       g p = s_new "pattern_Ex" (-1) AddToTail 10
--             [("out",20),
--              ("gate",1),
--              ("freq", midiCPS $ p + 48),
--              ("pan",-1),
--              ("cut",2000),
--              ("rez",0.6),
--              ("amp",0.1)]

sappyEv01 :: IO (Event OSC)
sappyEv01 = do
  m' <- M.update (pure . (map (midiCPS . (+36)))) "freq" <$> sappyMap
  return $ listE $ zip [0,2..] (mkSNew' "pattern_Ex" 10 m')

sappyEv015 :: IO (Event OSC)
sappyEv015 = do
  m' <- M.update (pure . (map (midiCPS . (+43)))) "freq" <$> sappyMap
  return $ listE $ zip [0,2..] (mkSNew' "pattern_Ex" 10 m')

sappyEv02 :: IO (Event OSC)
sappyEv02 = do
  m' <- M.insert "cut" (repeat 2000) .
        M.insert "rez" (repeat 0.6) .
        M.insert "pan" (repeat (-1)) .
        M.update (pure . map (midiCPS . (+48))) "freq" <$> sappyMap
  return $ listE $ zip [0.5,2.5..] $ mkSNew' "pattern_Ex" 10 m'

sappyEv03 :: IO (Event OSC)
sappyEv03 = do
  gen <- newStdGen
  let times = fmap (+32) $ scanl (+) 0 $ repeat 1
      g = s_new "bass_Ex" (-1) AddToTail 10
            [("freq",midiCPS 36),
             ("gate",1),
             ("pan",0),
             ("cut",128),
             ("amp",0.12),
             ("rez",0.1)]
      oscs = repeat g
  return $ listE $ zip times oscs


runSappy :: IO ()
runSappy = do
  query (updateSappyUGens >> freeAll >> add 0 sappyTree) s
  spawn 0 120 . mconcat =<<
           sequence [sappyEv01, sappyEv015, sappyEv02, sappyEv03]
