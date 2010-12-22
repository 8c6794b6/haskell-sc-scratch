------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- DTMF dialing tones.
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/DTMF>
--
-- /Example/:
--
-- > > withSC3 reset
-- > > n <- audit "dtmf" dtmf
-- > > nfree n
-- > > pat1
-- > > pat2
-- > > pat3 "01203339026"
-- > > go
--
module DesigningSound.DTMF where

import Control.Applicative (ZipList(..), (<*>), (<$>))
import Control.Concurrent (threadDelay)
import Sound.SC3
import System.Random (newStdGen, randomRs)
import qualified Data.Map as M

import DesigningSound.Util

-- | Lookup table for number to frequency.
tbl :: M.Map Char [Double]
tbl = M.fromList
 [ ('1', [697, 1209])
 , ('2', [770, 1209])
 , ('3', [852, 1209])
 , ('4', [697, 1336])
 , ('5', [770, 1336])
 , ('6', [852, 1336])
 , ('7', [697, 1477])
 , ('8', [770, 1477])
 , ('9', [852, 1477])
 , ('*', [697, 1633])
 , ('0', [770, 1633])
 , ('#', [852, 1633])
 , ('A', [941, 1209])
 , ('B', [941, 1336])
 , ('C', [941, 1477])
 , ('D', [941, 1633]) ]

-- | Synthdef playing a single number at a time.
--
-- > SynthDef(\dtmf, {|freq=#[770, 1633], out=0, amp=0.2, gate=1|
-- >   var son, env;
-- >   son = SinOsc.ar(freq, 0, amp).sum;
-- >   env = EnvGen.ar(Env.asr(0.001, 1, 0.001), gate, doneAction: 2);
-- >   Out.ar(out, Pan2.ar(son * env * amp));
-- > }).memStore;
--
dtmf :: UGen
dtmf = out 0 $ pan2 (son * ev * amp) 0 1
  where
    son = mix $ sinOsc ar (mce [freq1, freq2]) 0 * amp
    ev = envGen kr g 1 0 1 RemoveSynth (envASR 0.001 1 0.001 EnvCub)
    amp = control kr "amp" 0.2
    freq1 = control kr "freq1" 770
    freq2 = control kr "freq2" 1633
    g = control kr "gate" 1

-- | Pattern generating random phone number and dial it.
--
-- This pattern has is with /human timing/ mentioned below.
--
-- > Pbind(
-- >   \instrument, \dtmf,
-- >   \dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
-- >   \sustain, 0.15,
-- >   \amp, 0.3,
-- >   \freq, Prand(~tbl.asArray, 13)
-- > ).play;
--
pat1 :: IO ()
pat1 = sequence_ =<< acts
  where
    acts = do
      let idx = M.keys tbl
          dur = repeat 0.15
      cs <- map (idx !!) . take 13 . randomRs (0, length idx - 1) <$> newStdGen
      sus <- randomRs (0.05, 0.45) <$> newStdGen
      return $ getZipList $ pressNumber <$> z cs <*> z dur <*> z sus

-- | Dialing to '0128-27-743-866'.
--
-- > Pbind(
-- >   \instrument, \dtmf,
-- >   \dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
-- >   \sustain, 0.15,
-- >   \amp, 0.3,
-- >   \freq, Pseq("012827743866".collectAs({|digit| ~tbl[digit] }, Array))
-- > ).play;
--
pat2 :: IO ()
pat2 = pat3 "012827743866"

-- | Dial to given char sequence.
pat3 :: [Char] -> IO ()
pat3 cs = sequence_ =<< acts
  where
    acts = do
      let dur = repeat 0.15
      sus <- randomRs (0.05, 0.45) <$> newStdGen
      return $ getZipList $ pressNumber <$> z cs <*> z dur <*> z sus

-- | Short cut for ZipList
z :: [a] -> ZipList a
z = ZipList

-- | Press a number.
pressNumber :: Char   -- ^ number
            -> Double -- ^ duration
            -> Double -- ^ sustain
            -> IO ()
pressNumber c d s = do
  let [f1, f2] = maybe [0, 0] id $ M.lookup c tbl
  n <- snew "dtmf" [("freq1", f1), ("freq2", f2)]
  threadDelay $ floor $ d * 1000 * 1000
  nfree n
  threadDelay $ floor $ s * 1000 * 1000

-- | Get number from stdin and dial.
go :: IO ()
go = drecv "dtmf" dtmf >> go'
  where
    go' = do
      c <- getChar
      pressNumber c 0.1 0.1
      go'
