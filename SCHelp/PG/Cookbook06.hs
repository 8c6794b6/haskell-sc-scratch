------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook06
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook06_Phrase_Network/.
--
-- TODO:
-- 
-- * Synthdef differs from sclang version.
--
-- * Free the node when sustain is shorter than duration, and create
--   new synth node for next event.
-- 

module SCHelp.PG.Cookbook06 where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import System.Random
import qualified Data.Map as M

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCSched
import SCTree
import SCQuery
import qualified Scratch.ControlArgs as A

-- | UGen for pharase network example.
--
-- @
-- SynthDef(\sawpulse, { |out, freq = 440, gate = 0.5, plfofreq = 6, mw = 0, ffreq = 2000, rq = 0.3, freqlag = 0.05, amp = 1|
-- 	var sig, plfo, fcurve;
-- 	plfo = SinOsc.kr(plfofreq, mul:mw, add:1);
-- 	freq = Lag.kr(freq, freqlag) * plfo;
-- 	fcurve = EnvGen.kr(Env.adsr(0, 0.3, 0.1, 20), gate);
-- 	fcurve = (fcurve - 1).madd(0.7, 1) * ffreq;
-- 	sig = Mix.ar([Pulse.ar(freq, 0.9), Saw.ar(freq*1.007)]);
-- 	sig = RLPF.ar(sig, fcurve, rq)
-- 	* EnvGen.kr(Env.adsr(0.04, 0.2, 0.6, 0.1), gate, doneAction:2)
-- 	* amp;
-- 	Out.ar(out, sig ! 2)
-- }).memStore;
-- @

sawpulse :: UGen
sawpulse = out A.out $ mce [sig, sig]
    where
      sig = rlpf sig' fcurve rq *
            envGen kr gate 1 0 1 RemoveSynth sh *
            A.amp
      sh = envCoord' [(0, 0), (0.04, 1), (0.2, 0.6), (0.3, 0)] 1 1
           EnvLin
      sig' = mix $ mce [pulse ar freq 0.9, saw ar (freq * 1.007)]
      freq = lag A.freq freqlag * plfo
      freqlag = A.freqlag {controlDefault = 0.05}
      plfo = (sinOsc kr plfofreq 0 * A.mw) + 1
      fcurve = mulAdd (fcurve' - 1) 0.7 1 * ffreq
      fcurve' = envGen kr gate 1 0 1 DoNothing shape 
      -- shape = envLinen 0 0.3 20 0.1 [EnvCub]
      shape = envCoord' [(0,1), (0.3, 0.1), (20,0)] 1 1 EnvSin
      rq = A.rq {controlDefault = 0.3}
      plfofreq = A.plfofreq {controlDefault = 6}
      ffreq = A.ffreq {controlDefault = 2000}
      gate = A.gate {controlDefault = 0.5}

testSawPulse :: Transport t => t -> IO ()
testSawPulse = \fd -> do
  loadSynthdef "sawpulse" sawpulse fd
  send fd $ sync 1
  wait fd "/synced"
  send fd $ s_new "sawpulse" (-1) AddToTail 1 []

testEnv1 :: IO ()
testEnv1 = envTest $ envLinen 0 0.3 20 0.1 [EnvCub]

testEnv2 :: IO ()
testEnv2 = envTest $ envLinen 0.04 0.2 0.6 0.1 [EnvCub]

-- | BPM tempo.
phraseTempo :: Num a => a
phraseTempo = 128

-- | Type synonym for phrase.
type Phrase = Map String [Double]

type PhraseIndex = Int

type PhraseMatrix = [(PhraseIndex, [PhraseIndex])]

data PhraseState = PhraseState
    { phraseSeed :: StdGen,
      phraseIndex :: PhraseIndex }

-- | Run phrases.
--
-- Current implementation does not send @gate@ information to node,
-- and is not using @sustain@ values at all.
--
-- Rest is not expressed in performance.
--
-- Try:
--
-- > > setPhrase
-- > > runPhrase
-- > > cleanPhrase
-- 
runPhrase :: IO ()
runPhrase = playPhrases =<< genPhrases <$> initPhraseState

setPhrase :: IO ()
setPhrase = withSC3 $ \fd -> do
  loadSynthdef "sawpulse" sawpulse fd
  send fd (sync 1)
  wait fd "/synced"            
  send fd $ s_new "sawpulse" phraseNodeId AddToTail 1 [("amp",0)]

cleanPhrase :: IO ()
cleanPhrase = withSC3 $ \fd -> do
  send fd $ n_free [phraseNodeId]

genPhrases :: PhraseState -> [Phrase]
genPhrases = evalState (sequence $ repeat phraseDelta)

initPhraseState :: IO PhraseState
initPhraseState = fmap (\g -> PhraseState g 0) newStdGen

phraseDelta :: State PhraseState Phrase
phraseDelta = do
  st <- get
  let gen = phraseSeed st
      canditates = maybe [] id $ lookup (phraseIndex st) phraseMatrix
      (idx, gen') = chooseOne canditates gen
  put $ PhraseState gen' idx
  return $ phrases !! idx

phraseNodeId :: NodeId
phraseNodeId = 1001

playPhrases :: [Phrase] -> IO ()
playPhrases = mapM_ playPhrase

playPhrase :: Phrase -> IO ()
playPhrase p = do
  let p' = M.update (return . map midiCPS) "freq" .
           M.update (return . map (*0.4)) "amp" $ p
      dur = scanl (+) 0 . maybe [] id . M.lookup "dur" $ p
      sus = scanl (+) 0 . maybe [] id . M.lookup "sustain" $ p
      ev = listE $ zip dur $ mkNSet phraseNodeId p'
  spawn 0 phraseTempo ev

phrasesToE :: [Phrase] -> Event OSC
phrasesToE ps = undefined

phraseMatrix :: PhraseMatrix
phraseMatrix =
    [(0, [0,1,3]),
     (1, [1,2,3,4,7]),
     (2, [1,1,1,1,2,2,3,3,4,4,5]),
     (3, [0,1,1,1,1,3,3,3,3,5]),
     (4, [1,1,1,1,3,3,4,4,4]),
     (5, [0,1,1,1,1,3,3,5]),
     (6, [6,6,6,8,9,10,10,10,10,11,11,13,13]),
     (7, [0,7,7,7,7,7,3,3,3,3]),
     (8, [3,3,3,4,4,5]),
     (9, [10,10,10,11,11,11,11,12,12,12]),
     (10, [11,11,11,11,11,12,12]),
     (11, [0,9,9,11,11,12,12,12,12,12]),
     (12, [6,6,8,9,9,9,10,10,10,10,13,13,13]),
     (13, [8,13,13])]
-- phraseMatrix = 
--     [(0, [0,1,2]),
--      (1, [0,1,2]),
--      (2, [0,2])]

phrases :: [Phrase]
phrases = [phr01, phr02, phr03, phr04, phr05,
           phr06, phr07, phr08, phr09, phr10,
           phr11, phr12, phr13, phr14]
-- phrases = [p01, p02, p03]

boolToBin :: Num a => Bool -> a
boolToBin True  = 1
boolToBin False = 0

pp :: IO ()
pp = playPhrase =<< 
     M.update (return . map (*0.4)) "amp" <$>
     M.unionsWith (++) <$> 
     take 2000 <$> genPhrases <$> initPhraseState

-- | Phrase for testing.
p01 :: Phrase
p01 = M.fromList $ 
      [("freq", [60, 64, 67, 64, 60]),
       ("dur",  0.5 : replicate 4 0.25),
       ("sustain", [1.0, 1.0, 0.5, 0.5, 1.0]),
       ("amp", [1, 1, 0.5, 0.5, 0.7]),
       ("mw", replicate 5 0)]

-- | Another phrase for testing.
p02 :: Phrase 
p02 = M.fromList $ 
      [("freq", [60, 65, 69, 72, 69, 65]),
       ("dur", replicate 6 0.25),
       ("sustain", [0.5, 0.5, 0.25, 0.25, 0.25, 0.25]),
       ("amp", [1, 0.7, 0.7, 1, 0.7, 0.5]),
       ("mw", replicate 6 0)]

p03 :: Phrase
p03 = M.fromList $
      [("freq", [62, 65, 67, 71, 67, 65]),
       ("dur", replicate 6 0.25),
       ("sustain", replicate 6 0.3),
       ("amp", [1, 0.7, 0.7, 1, 0.7, 0.5]),
       ("mw", replicate 6 0)] 

phr01 :: Phrase
phr01 = M.fromList $
           [("freq",[78,81,78,76,78,76,72,71,69,66]),
            ("dur",[0.25,1.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25]),
            ("sustain", [0.3,1.2,0.3,0.2,0.3,0.2,0.3,0.2,0.3,0.2]),
            ("amp", [1,0.5,0.75,0.5,0.75,0.5,0.75,0.5,0.75,0.5]),
            ("mw", [0,0.03] ++ replicate 8 0)]

phr02 :: Phrase
phr02 = M.fromList $
        [("freq", [64, 66, 69, 71, 72, 73]),
         ("dur", replicate 6 0.25),
         ("sustain", [0.3, 0.2, 0.3, 0.3, 0.3, 0.2]),
         ("amp", [1, 0.5, 0.5, 0.5, 0.5, 0.5]),
         ("mw", replicate 6 0)]

phr03 :: Phrase
phr03 = M.fromList $
        [("freq", [69, 71, 69, 66, 64, 69, 71, 69]),
         ("dur", [0.125, 0.625, 0.25, 0.25, 0.25, 0.25, 0.25, 0.75]),
         ("sustain", [0.2, 0.64, 0.2, 0.2, 0.2, 0.3, 0.3, 0.75]),
         ("amp", [0.5, 0.75, 0.5, 0.5, 0.5, 1, 0.5, 0.5]),
         ("mw", replicate 8 0)]

phr04 :: Phrase
phr04 = M.fromList $
        [("freq", [72, 73, 76, 72, 71, 69, 66, 71, 69]),
         ("dur", [0.25, 0.25, 0.25, 0.083, 0.083, 0.084, 0.25, 0.25, 0.25]),
         ("sustain", [0.2, 0.64, 0.2, 0.2, 0.2, 0.3, 0.3, 0.75, 0.75]),
         ("amp", [1, 0.5, 0.5, 1, 0.3, 0.3, 0.75, 0.75, 0.5]),
         ("mw", replicate 9 0)]

phr05 :: Phrase
phr05 = M.fromList $
        [("freq", [64, 66, 69, 71, 72, 73, 71, 69,
                   66, 71, 69, 66, 64, 69]),
         ("dur", [0.25, 0.25, 0.25, 0.25, 0.125, 0.375, 0.166, 0.166, 0.168,
                  0.5, 0.166, 0.166, 0.168, 0.5]),
         ("sustain", [0.3, 0.2, 0.2, 0.2, 0.14, 0.4, 0.2, 0.2, 0.2,
                      0.6, 0.2, 0.2, 0.2, 0.5]),
         ("amp", [0.5, 0.5, 0.6, 0.8, 1.0, 0.5, 0.5, 0.5,
                  0.5, 1.0, 0.5, 0.5, 0.5, 0.45]),
         ("mw", replicate 14 0)]

phr06 :: Phrase
phr06 = M.fromList $
        [("freq", [72, 73, 76, 78, 81, 78, 83, 81,
                   84, 85]),
         ("dur",[0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5,
                 0.125, 1.125]),
         ("sustain", [0.3, 0.2, 0.2, 0.2, 0.95, 0.25, 0.9, 0.25,
                      0.2, 1.13]),
         ("amp", [0.7, 0.5, 0.5, 0.5, 0.7, 0.5, 0.8, 0.5,
                  1, 0.5]),
         ("mw", replicate 9 0 ++ [0.03])]

phr07 :: Phrase
phr07 = M.fromList $
        [("freq", [83, 81, 78, 83, 81, 78, 76, 72,
                   73, 78, 72, 72, 71]),
         ("dur", [0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                  0.25, 0.25, 0.25, 0.25, 2]),
         ("sustain", [0.2, 0.3, 0.2, 0.3, 0.3, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3, 2]),
         ("amp", [0.5, 0.5, 0.5, 0.8, 0.5, 0.5, 0.5, 0.8, 0.5, 0.8, 0.5, 1, 0.4]),
         ("mw", replicate 12 0 ++ [0.03])]

phr08 :: Phrase
phr08 = M.fromList $
        [("freq", [69, 71, 72, 71, 69, 66, 64, 69, 71]),
         ("dur", [0.25, 0.25, 0.25, 0.25, 0.166, 0.167, 0.167, 0.25, 0.25]),
         ("sustain", [0.2, 0.2, 0.3, 0.2, 0.2, 0.2, 0.14, 0.3, 0.3]),
         ("amp", [0.5, 0.5, 0.8, 0.5, 0.5, 0.5, 0.5, 0.8, 0.5]),
         ("mw", replicate 8 0 ++ [0.03])]

phr09 :: Phrase
phr09 = M.fromList $
        [("freq", [83, 85, 84, 84, 88, 84, 83, 81, 83, 81, 78, 76, 81, 83]),
         ("dur", [0.125, 0.535, 0.67, 1.92, 0.25, 0.166, 0.167, 0.167,
                  0.25, 0.25, 0.25, 0.25, 0.25, 0.25]),
         ("sustain", [0.2, 3.12, 0.2, 0.2, 0.2, 0.2, 0.2, 0.15, 0.3,
                      0.2, 0.2, 0.2, 0.3, 0.2]),
         ("amp", [1, 0.8, 0.8, 0.8, 1, 1, 0.8, 0.8, 1, 0.8, 0.8, 0.8,
                  1, 0.8]),
         ("mw", [0, 0.005, 0.005, 0.06] ++ replicate 10 0)]

phr10 :: Phrase
phr10 = M.update (return . map (+12)) "freq" phr04

phr11 :: Phrase
phr11 = M.fromList $
        [("freq", [81, 84, 83, 81, 78, 76, 81, 83]),
         ("dur", replicate 8 0.25),
         ("sustain", [0.2, 0.3, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2]),
         ("amp", [0.5, 1, 0.5, 0.5, 0.6, 0.5, 0.8, 0.5]),
         ("mw", replicate 8 0)]

phr12 :: Phrase
phr12 = M.update (return . map (+12)) "freq" phr01

phr13 :: Phrase
phr13 = M.fromList $
        [("freq", [78, 81, 83, 78, 83, 84, 78, 84, 85]),
         ("dur", [0.25, 0.25, 0.5, 0.25, 0.25, 0.5, 0.25, 0.25,
                  1.75]),
         ("sustain", [0.2, 0.3, 0.2, 0.2, 0.3, 0.2, 0.2, 0.3, 1.75]),
         ("amp", [0.4, 0.8, 0.5, 0.4, 0.8, 0.5, 0.4, 1, 0.8]),
         ("mw", replicate 8 0 ++ [0.03])]

phr14 :: Phrase
phr14 = M.fromList $
        [("freq", [88, 84, 83, 81, 83, 81, 78, 76, 81, 83]),
         ("dur", [0.25, 0.166, 0.167, 0.167, 0.25, 0.25, 0.25, 0.25,
                  0.25, 0.25]),
         ("sustain", [0.2, 0.2, 0.2, 0.15, 0.3, 0.2, 0.2, 0.2, 0.3,
                      0.2]),
         ("amp", [1, 1, 0.8, 0.8, 1, 0.8, 0.8, 0.8, 1, 0.8]),
         ("mw", replicate 10 0)]
