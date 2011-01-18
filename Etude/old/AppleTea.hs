------------------------------------------------------------------------------
-- |
-- Etude for using percussive synthdefs and phrases.
--
-- Synthdef of kick, snare, and hihat is inspired from acid otophilia
-- example.
--
-- TODO:
--
-- * Add effects to rhythm track.
-- * Add sound-effect-ish tracks.
-- * Add gui for controlling parameters.
-- * Make sequences, verses and sections.
-- * Record the sound.
--

module AppleTea where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import System.Random
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Monadic
import Sound.SC3.Wing
import qualified Sound.SC3.Wing.UGen.ControlArg as A

runAppleTea :: IO ()
runAppleTea = undefined

setAppleTea :: Transport t => t -> IO ()
setAppleTea fd = do
  updateAppleSynthdefs fd
  addNode 0 appleTeaTree fd

-- | Update synthdefs.
updateAppleSynthdefs :: Transport t => t -> IO OSC
updateAppleSynthdefs fd = do
  loadSynthdef "simplePan" simplePan fd
  loadSynthdef "simpleReverb" simpleReverb fd
  loadSynthdef "simpleReverb2" simpleReverb2 fd
  loadSynthdef "simpleGain" simpleGain fd
  lsd "appleKick" appleKick
  lsd "appleSnare" appleSnare
  lsd "appleHat" appleHat
  lsd "appleLead" appleLead
  lsd "appleChorus" appleChorus
  lsd "appleBass" appleBass
  lsd "simpleLfNoise2" simpleLfNoise2
    where
      lsd name ug = ug >>= \ug' -> loadSynthdef name ug' fd

-- |  For kick synthdef.
appleKick :: IO UGen
appleKick = out A.out <$>
            (* A.amp {controlDefault=0.8}) <$>
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
      env2 = envGen kr 1 1 0 1 RemoveSynth $
             env [0, 1.0, 0.05, 0] [0.002, 0.05, 0.03]
             (map EnvNum [0, -4, -4]) 1 1

-- | For lead synthdef
appleLead :: IO UGen
appleLead = out A.out . (*A.amp) <$> sig
    where
      sig :: IO UGen
      sig = (*env0) . (+osc0) . flt0 (pulse ar A.freq 0.3) <$> nz0

      nz0 :: IO UGen
      nz0 = (*env1) . (*8000) . (+1) . (*0.5) <$> lfNoise1 ar 4

      flt0 :: UGen -> UGen -> UGen
      flt0 ug pass = rlpf ug pass 0.5

      osc0, env0, env1 :: UGen
      osc0 = sinOsc ar car0 0
      car0 = A.freq + (A.freq * saw ar (A.freq * 1.5))
      env0 = envGen kr 1 1 0 1 RemoveSynth $
             env [0, 1, 0.8, 0] [0.001, 0.01, 1.0]
             (map EnvNum [-4, -4, -4]) 1 1
      env1 = envGen kr 1 1 0 1 DoNothing $
             env [1, 0.5, 1, 0.2, 0.1] [0.001, 0.01, 0.02, 0.3]
             (map EnvNum [-4, -4, -3, -4]) 1 1

-- | For chorus synthdef.
appleChorus :: IO UGen
appleChorus = out A.out . (*A.amp) <$> sig
    where
      sig :: IO UGen
      sig = (\x -> sinOsc ar x 0) <$> (car <$> lfNoise2 ar 4)

      car :: UGen -> UGen
      car x = frq + (sinOsc ar (0.5 * frq) 0 * x * frq)

      frq :: UGen
      frq = lag A.freq 0.1

-- | For bass synthdef
appleBass :: IO UGen
appleBass = out A.out . (*A.amp) . (*env0) <$> sig
    where
      sig, nz0 :: IO UGen
      sig = car <$> nz0
      nz0 = lfNoise1 kr (0.12)

      car :: UGen -> UGen
      car ug = sinOsc ar (A.freq + mod ug) 0

      mod :: UGen -> UGen
      mod ug = sinOsc ar (A.freq * env1) 0 * A.freq * ug

      env0, env1 :: UGen
      env0 = envGen kr A.gate 1 0 1 RemoveSynth $
             env [0,1,0.8,0.2,0] [0.03,0.05,1.2]
             (map EnvNum [-4,-5,-4]) 1 (-1)
      env1 = envGen kr 1 1 0 1 DoNothing $
             env [1,0.5] [0.2]
             (map EnvNum [-4]) (-1) (-1)

-- | Simple panner.
simplePan :: UGen
simplePan = out A.outBus (pan2 sig A.pan 1)
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

-- | Simple gain controlling synthdef.
simpleGain :: UGen
simpleGain = replaceOut A.outBus sig
    where
      sig = clip2 (in' 1 ar A.inBus * A.preamp) 1 * A.amp

-- | Simple low frequency noise for control. Uging lfNoise2.
simpleLfNoise2 :: IO UGen
simpleLfNoise2 = out (A.out) <$> sig
    where
      sig :: IO UGen
      sig = (*A.mul) . (+A.add) <$> lfNoise2 kr A.freq

playRhythm :: BPM -> Double -> Phrase -> IO ()
playRhythm bpm t0 = spawn t0 bpm .
                 mkRhythms rhythmDefault rhythmGroup (scanl (+) 0 rhythmDur)

-- | Synth node mapping.
appleTeaTree :: SCTree
appleTeaTree =
  Group 0
   [Group 1
    [Group controlGroup
     [Synth preampId "simpleLfNoise2"
      ["out":=preampBus, "mul":=5, "add":=1, "freq":=2.17],
      Synth panId "simpleLfNoise2"
      ["out":=panBus, "mul":=0.5, "add":=0, "freq":=0.75]],
     Group sourceGroup
     [Synth chorus1 "appleChorus" ["out":=chorusBus, "amp":=0],
      Synth chorus2 "appleChorus" ["out":=chorusBus, "amp":=0],
      Synth chorus3 "appleChorus" ["out":=chorusBus, "amp":=0],
      Synth bass1 "appleBass" ["out":=bassBus, "amp":=0],
      Group rhythmGroup []],
     Group fxGroup
     [Synth (inFxGrp 11) "simpleReverb"
      ["outBus":=hatBus, "inBus":=hatBus,
       "mix":=0.08, "damp":=0.8, "room":=0.2],
      Synth (inFxGrp 12) "simpleGain"
      ["outBus":=hatBus, "inBus":=hatBus, "preamp":=1, "amp":=1],
      Synth (inFxGrp 13) "simplePan"
      ["outBus":=0, "inBus":=hatBus, "pan":=0.11],

      Synth (inFxGrp 21) "simpleReverb"
      ["outBus":=snareBus, "inBus":=snareBus,
       "mix":=0.19, "damp":=0.3, "room":=0.4],
      Synth (inFxGrp 22) "simpleGain"
      ["outBus":=snareBus, "inBus":=snareBus, "preamp":=1, "amp":=0.5],
      Synth (inFxGrp 23) "simplePan"
      ["outBus":=0, "inBus":=snareBus, "pan":=(-0.19)],

      Synth (inFxGrp 31) "simpleReverb"
      ["outBus":=kickBus, "inBus":=kickBus,
       "mix":=0.02, "damp":=0.7, "room":=0.19],
      Synth (inFxGrp 32) "simpleGain"
      ["outBus":=kickBus, "inBus":=kickBus, "preamp":=1, "amp":=1],
      Synth (inFxGrp 33) "simplePan"
      ["outBus":=0, "inBus":=kickBus, "pan":=0.03],

      Synth (inFxGrp 41) "simpleReverb"
      ["outBus":=leadBus, "inBus":=leadBus,
       "mix":=0.9, "damp":=0.9, "room":=0.9],
      Synth (inFxGrp 42) "simpleGain"
      ["outBus":=leadBus, "inBus":=leadBus, "preamp":=1, "amp":=1],
      Synth (inFxGrp 43) "simplePan"
      ["outBus":=0, "inBus":=leadBus, "pan":=(-0.1)],

      Synth (inFxGrp 51) "simpleGain"
      ["outBus":=chorusBus, "inBus":=chorusBus,
       "preamp":<-preampBus, "amp":=0.2],
      Synth (inFxGrp 52) "simpleReverb"
      ["outBus":=chorusBus, "inBus":=chorusBus,
       "mix":=0.3, "damp":=0.2, "room":=0.7],
      Synth (inFxGrp 53) "simplePan"
      ["outBus":=0, "inBus":=chorusBus, "pan":<-panBus],

      Synth (inFxGrp 61) "simpleGain"
      ["outBus":=bassBus, "inBus":=bassBus, "preamp":=1, "amp":=1],
      Synth (inFxGrp 62) "simpleReverb"
      ["outBus":=bassBus, "inBus":=bassBus,
       "mix":=0.2, "damp":=0.2, "room":=0.4],
      Synth (inFxGrp 63) "simplePan"
      ["outBus":=0, "inBus":=bassBus, "pan":=0.3]
     ]]]

sourceGroup, rhythmGroup, fxGroup, controlGroup :: Num a => a
rhythmGroup = 11
fxGroup = 21
sourceGroup = 31
controlGroup = 41

rhythmBus, kickBus, snareBus, hatBus, leadBus, chorusBus :: Num a => a
rhythmBus = 101
kickBus = 102
snareBus = 103
hatBus = 104
leadBus = 105
chorusBus = 106

bassBus :: Num a => a
bassBus = 107

preampBus, panBus :: Num a => a
preampBus = 1001
panBus = 1002

chorus1, chorus2, chorus3 :: Num a => a
chorus1 = 3101
chorus2 = 3102
chorus3 = 3103

bass1 :: Num a => a
bass1 = 3104

preampId, panId :: Num a => a
preampId = 4101
panId = 4102

inFxGrp :: Num a => a -> a
inFxGrp a = fxGroup * 100 + a

normalizeR :: Phrase -> Phrase
normalizeR = M.map (fmap (*0.1))

rhythmDur :: [Double]
rhythmDur = cycle [0.29, 0.21, 0.27, 0.23]

rhythmDefault :: Map String [(String,Double)]
rhythmDefault = M.fromList
  [("appleHat",   [("out", hatBus)]),
   ("appleSnare", [("out", snareBus)]),
   ("appleKick",  [("out", kickBus)])]

rhythm05 :: Phrase
rhythm05 = normalizeR $ M.fromList
  [("appleHat",   [1,2,4,2, 1,2,4,2, 1,2,4,2, 1,2,4,2]),
   ("appleSnare", [0,3,0,0, 4,0,0,3, 0,3,0,0, 4,0,0,3]),
   ("appleKick",  [5,0,0,4, 0,0,5,0, 0,4,0,0, 4,0,5,0])]

rhythm06 :: Phrase
rhythm06 = normalizeR $ M.fromList
  [("appleHat",   [1,2,4,2, 1,2,4,2, 1,2,4,2, 1,2,4,2]),
   ("appleSnare", [0,0,0,0, 0,0,0,0, 4,0,0,0, 0,0,0,0]),
   ("appleKick",  [5,0,0,4, 0,0,5,0, 0,0,0,0, 0,0,0,4])]

rhythm07 :: Phrase
rhythm07 = normalizeR $ M.fromList
  [("appleHat",   [1,2,4,1, 1,2,4,1, 1,0,4,0, 1,2,4,2]),
   ("appleSnare", [0,0,0,0, 0,0,0,0, 4,0,0,0, 0,0,0,0]),
   ("appleKick",  [5,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,4])]

rhythm08 :: Phrase
rhythm08 = normalizeR $ M.fromList
  [("appleHat",   [1,2,4,2, 1,2,4,2, 1,2,4,2, 1,2,4,2]),
   ("appleSnare", [0,3,0,0, 3,0,0,3, 0,0,3,0, 0,5,0,4]),
   ("appleKick",  [5,0,0,4, 0,0,5,0, 0,4,0,0, 4,0,5,0])]

rhythm09 :: Phrase
rhythm09 = normalizeR $ M.fromList
  [("appleHat",   [1,2,4,2, 1,2,4,2, 1,2,4,2, 1,2,4,2]),
   ("appleSnare", [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]),
   ("appleKick",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])]

rhythmVerse01 :: Phrase
rhythmVerse01 = M.unionsWith (++)
  [rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm06, rhythm05, rhythm05, rhythm08,

   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm06, rhythm06, rhythm06, rhythm07,
   rhythm06, rhythm07, rhythm05, rhythm08,

   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm05, rhythm05, rhythm05, rhythm05,
   rhythm06, rhythm05, rhythm05, rhythm08,

   rhythm05, rhythm05, rhythm05, rhythm06,
   rhythm09, rhythm07, rhythm09, rhythm06,
   rhythm09, rhythm09, rhythm09, rhythm09,
   rhythm09, rhythm09, rhythm05, rhythm08]

rhythmG :: Int -> StdGen -> Phrase
rhythmG n g0 = normalizeR $ M.fromList
  [("appleHat",   take n $ choices [1..4] g0),
   ("appleSnare", take n $ choices [0..5] g1),
   ("appleKick",  take n $ choices [0..5] g2)]
    where
      [g1,g2] = take 2 $ iterate (snd . next) g0

rhythmS :: StdGen -> Phrase
rhythmS g = normalizeR $ M.fromList [("appleSnare", choices [0,0,1,2,4] g)]

melody01 :: Phrase
melody01 = M.fromList
  [("freq", map midiCPS [60,60,60, 62,62,62]),
   ("amp",  [0.5,0.4,0.3, 0.5,0.4,0.4]),
   ("dur",  [0.75,0.75,2.5, 0.75,0.75,2.5]),
   ("out",  take 6 $ repeat leadBus)]

melody02 :: Phrase
melody02 = M.update g "freq" melody01
    where g = (\_ -> return $ map midiCPS [67,67,67, 67,67,67])

chorus01 :: Phrase
chorus01 = M.fromList
  [("freq", map (midiCPS . (+24)) [48,50,48,47]),
   ("amp",  take 2 (repeat 0.5)),
   ("dur",  take 2 (repeat 1)),
   ("out",  take 2 (repeat chorusBus))]

chorus02 :: Phrase
chorus02 = M.update g "freq" chorus01
    where g m = return $ map (* 0.5) m

chorus03 :: Phrase
chorus03 = M.update g "freq" chorus01
    where g _ = return (map midiCPS [55,53,53,55])

mkChorus :: Event OSC
mkChorus = mconcat [e1,e2,e3]
    where
      [e1,e2,e3] = zipWith mkE [chorus1, chorus2, chorus3]
                   $ map (M.map cycle) [chorus01, chorus02, chorus03]
      mkE gid p = listE $ zip durs (mkNSet gid p)
      durs = scanl (+) 0 $ repeat 16

bassG :: StdGen -> Phrase
bassG g0 = M.fromList
  [("freq", map midiCPS [38,36,43,38,40,36]),
   ("dur",  [0.75,0.75,1.0,0.5,0.5,0.5]),
   ("sustain", [0.70,0.70,0.80, 0.45,0.45,0.45]),
   ("amp",  choices [0.2,0.3,0.4] g0),
   ("out",  repeat bassBus)]

-- | Plays each events with forking threads. CPU friendly than playing testE
-- in single thread.
testy :: IO [ThreadId]
testy = do
  let bpm = 130
      e = mkSNewWithDur "appleLead" sourceGroup (M.map cycle melody01)
          `mappend`
          mkSNewWithDur "appleLead" sourceGroup (M.map cycle melody02)
  t1 <- forkIO . spawn 4 bpm $ e
  t2 <- forkIO . spawn 4 bpm . mkRhythms rhythmDefault rhythmGroup
        (scanl (+) 0 rhythmDur) . M.map cycle $ rhythmVerse01
  t3 <- forkIO . playRhythm bpm 4 . rhythmS =<< newStdGen
  t4 <- forkIO . spawn 4 bpm $ mkChorus
  t5 <- forkIO . spawn 4 bpm . setMonoEvent bass1 .
        M.map cycle . bassG =<< newStdGen
  return [t1,t2,t3,t4,t5]

-- | Noticed that spawning merged events eats cpu more thatn forking each
-- events in separate thread.
testE :: IO (Event OSC)
testE = do
  let mkMelody = mkSNewWithDur "appleLead" sourceGroup . M.map cycle
      e1 = mkMelody melody01
      e2 = mkMelody melody02
      e3 = mkRhythms rhythmDefault rhythmGroup (scanl (+) 0 rhythmDur) .
           M.map cycle $ rhythmVerse01
      e4 = mkChorus
  e5 <- mkRhythms rhythmDefault rhythmGroup (scanl (+) 0 rhythmDur) .
        rhythmS <$> newStdGen
  e6 <- setMonoEvent bass1 . M.map cycle . bassG <$> newStdGen
  return $ mconcat [e1,e2,e3,e4,e5,e6]