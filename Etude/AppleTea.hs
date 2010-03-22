------------------------------------------------------------------------------
-- |
-- Etude.
-- 
-- Synthdef of kick, snare, and hihat is inspired from acid otophilia
-- example.
--

module AppleTea where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import System.Random
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing
import qualified Sound.SC3.Wing.ControlArg as A

runAppleTea :: IO ()
runAppleTea = undefined

setAppleTea :: Transport t => t -> IO ()
setAppleTea fd = do
  updateAppleSynthdefs fd
  mkTree appleTeaTree fd

-- | Update synthdefs.
updateAppleSynthdefs :: Transport t => t -> IO OSC
updateAppleSynthdefs fd = do
  loadSynthdef "simplePan" simplePan fd
  loadSynthdef "simpleReverb" simpleReverb fd
  loadSynthdef "simpleReverb2" simpleReverb2 fd
  lsd "appleKick" appleKick fd
  lsd "appleSnare" appleSnare fd
  lsd "appleHat" appleHat fd
    where
      lsd name ug fd = ug >>= \ug' -> loadSynthdef name ug' fd

-- | Synth node mapping.
appleTeaTree :: SCTree
appleTeaTree = 
  Group 0
   [Group 1
    [Group rhythmGroup [],
     Group fxGroup 
     [Synth 2102 "simpleReverb"
      ["outBus":=rhythmBus, "inBus":=rhythmBus, 
       "mix":=0.3, "damp":=0.3, "room":=0.2],
      Synth 2101 "simplePan" 
      ["outBus":=0, "inBus":=rhythmBus, "pan":=0]
     ]]]

testPerc :: String -> IO ()
testPerc name = withSC3 $ \fd -> do
  send fd $ s_new name (-1) AddToTail rhythmGroup [("out",rhythmBus)] 

testKick :: IO ()
testKick = testPerc "appleKick"

testSnare :: IO ()
testSnare = testPerc "appleSnare"

testHat :: IO ()
testHat = testPerc "appleHat"

rhythmGroup, fxGroup :: Num a => a
rhythmGroup = 11
fxGroup = 21

rhythmBus :: Num a => a
rhythmBus = 101

-- |  For kick synthdef.
appleKick :: IO UGen
appleKick = out A.out <$> (* A.amp {controlDefault=0.8}) <$>
            (clp . (+osc) . flt .  (+pls) <$> whiteNoise ar)
    where
      clp :: UGen -> UGen
      clp ug = clip2 (ug * 1.2) 1

      flt :: UGen -> UGen
      flt ug = lpf ug (env1m*1.5) * env0

      osc, pls, env1m, env0, env1 :: UGen
      osc = sinOsc ar env1m 0.5 * env0
      pls = lfPulse ar env1m 0 0.5 * 1 - 0.5
      env1m = midiCPS env1
      env0 = envGen kr 1 1 0 1 RemoveSynth 
             (env [0.5, 1.0, 0.5, 0] [0.005, 0.06, 0.26]
              (map EnvNum [-4, -2, -4]) (-1) (-1))
      env1 = envGen kr 1 1 0 1 RemoveSynth
             (env [110, 55, 19] [0.005, 0.29]
              (map EnvNum [-4, -5]) (-1) (-1))

-- | For snare synthdef.
appleSnare :: IO UGen
appleSnare = out A.out <$> (clp . (+osc) <$> nz)
    where
      clp :: UGen -> UGen
      clp ug = clip2 ug 1 * (A.amp {controlDefault=0.8})

      flt :: UGen -> UGen
      flt ug = (bpf ug 6900 0.6 * 3) + ug

      flt' :: UGen -> UGen
      flt' ug = hpf ug 200 * 2

      nz :: IO UGen
      nz = (*env2) . flt . flt' . (*0.2) <$> whiteNoise ar

      osc, pls, env0, env1m, env1, env2 :: UGen
      osc = (lpf pls (env1m*1.2) * env0) + (sinOsc ar env1m 0.8 * env0)
      pls = (lfPulse ar env1m 0 0.5 * 1 - 0.5) +
            (lfPulse ar (env1m*1.6) 0 0.5 * 0.5 - 0.25)
      env0 = envGen kr 1 1 0 1 DoNothing 
             (env [0.5, 1, 0.5, 0] [0.005, 0.03, 0.10]
              (map EnvNum [-4,-2,-4]) 1 1)
      env1m = midiCPS env1
      env1 = envGen kr 1 1 0 1 DoNothing
             (env [110, 60, 44] [0.005, 0.1]
              (map EnvNum [-4,-5]) 1 1)
      env2 = envGen kr 1 1 0 1 RemoveSynth 
             (env [1, 0.4, 0] [0.05, 0.13]
              (map EnvNum [-2, -2]) 1 1)

-- | For hihat synthdef.
appleHat :: IO UGen
appleHat = out A.out <$> sig
    where
      sig, nz, osc :: IO UGen
      sig = clp <$> ((+) <$> nz <*> osc)
      nz = flt2 . flt3 . flt4 <$> (mkNz [1..8] =<< whiteNoise ar)
      osc = flt1 <$> mkOsc 0 [1..5]

      mkOsc :: UGen -> [UGen] -> IO UGen
      mkOsc v xs = foldM f v xs 
        where 
          f a b = do
            f1 <- getStdRandom (randomR (0,4))
            f2 <- getStdRandom (randomR (0,4))
            return $ sinOsc ar 
                  (midiCPS $ linLin b 0 (n'-1) 42 74 + f1)
                  (sinOsc ar
                   (midiCPS $ linLin b 0 (n'-1) 78 80 + f2) 0 * 12)
                  * (1/n')
          n' = constant 5

      mkNz :: [UGen] -> UGen -> IO UGen
      mkNz xs v = foldM f v xs
          where
            f a b = do
              fAdd <- getStdRandom (randomR (0,4))
              let frq = linLin b 0 (n2'-1) 40 50 + fAdd
              return $ combN a 0.04  frq 0.1 * (1/n2') + a
            n2' = 8
                  
      clp :: UGen -> UGen
      clp ug = softClip ug * (A.amp {controlDefault=0.3})
               
      flt1, flt2, flt3, flt4 :: UGen -> UGen
      flt1 ug = bHiPass ug 1000 2 * env1
      flt2 ug = bHiPass ug 1000 1.5 * env2
      flt3 ug = bLowShelf ug 3000 0.5 (-6)
      flt4 ug = bpf ug 6000 0.9 * 0.5 + ug
 
      env1, env2  :: UGen
      env1 = envGen kr 1 1 0 1 DoNothing $ 
             env [0, 1.0, 0] [0.001, 0.2] (map EnvNum [0, -12]) 1 1
      env2 = envGen kr 1 1 0 1 DoNothing $
             env [0, 1.0, 0.05, 0] [0.002, 0.05, 0.03]
             (map EnvNum [0, -4, -4]) 1 1

-- | Simple panner.
simplePan :: UGen
simplePan = replaceOut A.outBus (pan2 sig A.pan 1)
    where sig = in' 1 ar A.inBus

-- | Simple reverb using freeverb.
simpleReverb :: UGen
simpleReverb = mkSimpleReverb 1

-- | Stereo version of simpleReverb.
simpleReverb2 :: UGen
simpleReverb2 = mkSimpleReverb 2

-- | Makes simple reverb with given number of input channel.
mkSimpleReverb :: Int -> UGen
mkSimpleReverb n = replaceOut A.outBus (rvb sig)
    where
      sig = in' n ar A.inBus
      rvb ug = freeVerb ug mix room damp
      mix = A.mix {controlDefault=0.5}
      room = A.room {controlDefault=0.5}
      damp = A.room {controlDefault=0.5}

rhythm01 :: Phrase
rhythm01 = undefined

melody01 :: Phrase
melody01 = undefined