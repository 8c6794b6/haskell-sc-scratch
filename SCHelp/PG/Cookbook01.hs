------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook01
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing sequences shown in
-- /PG_Cookbook01_Basic_Sequencing/.
--

module SCHelp.PG.Cookbook01 (

     -- * Plaing a predefined note sequence
     -- $predefined

     fuguePitch,
     degreeToFreq,
     degrees,
     sandwitch,
     sandwitchedDegrees,
     fugue,
     prepareListE,
     runFugue,
     simpleSynth,

     -- * Multichannel expansion
     -- $multichannelExpansion

     mceChords,
     mceDurs,
     runMCEchords,

     -- * Using custom Synthdefs
     -- $customSynthdefs
     stretchedFragments,
     psTest,
     prepareStretched,
     stretchedBufnum,
     stretchedMessages,
     mapToEventOSC,
     runStretched
     ) where

import Control.Applicative
import Control.Monad
import System.Random
import Data.List (transpose)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as M

import FRP.Reactive
import Sound.SC3
import Sound.SC3.Lang.Math
import Sound.OpenSoundControl

import Reusable
import SCQuery
import SCTree
import SCSched
import Scratch.Pat.P5
import qualified Scratch.ControlArgs as A

-- $predefined
-- Plays a sequence of notes which is predefined as a list.

fuguePitch :: Pitch Double
fuguePitch = defaultPitch { scale = [0, 2, 3, 5, 7, 8, 10], root = 2}

degreeToFreq d = toPv $ freq $ fuguePitch {degree=d}

degrees = [3,2,1,0,-0.9,0,1,2,-3,-1.9,-0.9,0,-0.9,0,1,2]

sandwitch :: a -> [a] -> [a]
sandwitch _ [] = []
sandwitch a (x1:xs) = x1 : a : sandwitch a xs

sandwitchedDegrees :: [Double]
sandwitchedDegrees = 4 : sandwitch 4 degrees

fugue :: Pattern PValue
fugue =
    Pbind
    [("freq",Pseq 1 $ map (Pid . degreeToFreq) sandwitchedDegrees),
     ("dur", Pseq 1 $ take (length sandwitchedDegrees) $
           repeat (Pid (PNum 0.25)))]

prepareListE :: String -> [[(String,Double)]] -> [(Double,OSC)]
prepareListE name xs = fst $ foldr f v xs
    where f xs (xs',s) = (xs' ++ [(s',osc)], s')
              where
                osc = s_new name (-1) AddToTail 1 xs
                s' = s + (maybe 0 id $ lookup "dur" xs)
          v = ([],0)

-- | Runs fugue. Try:
--
-- > > loadSynthdef "simpleSynth" simpleSynth
-- > > spawn 0 84 =<< runFugue
-- 
runFugue :: IO (Event OSC)
runFugue = do
  params <- map fromPv <$> runPIO fugue
  return $ listE $ prepareListE "simpleSynth" params

-- | Simple synth for playing fugue sequence.
simpleSynth :: UGen
simpleSynth = out 0 $ pan2 (sinOsc ar A.freq 0 * env) 0 1 * 0.3
    where env = envGen kr 1 1 0 1 RemoveSynth (envPerc 0.1 0.5)


-- $multichannelExpansion
-- Choose degrees randomly from one of:
-- [[7, 4, 2], [7, 5, 2], [7, 5, 3], [7, 6, 3]]
-- and play as a triad chord. Try:
--
-- > > spawn 0 60 =<< runMCEchords
-- 

mceChords :: [[Double]]
mceChords = [[0,-2,-4],[0,-3,-5],[0,-2,-5],[0,-1,-4]]

mceDurs :: [Double]
mceDurs = [1,0.5]

runMCEchords :: IO (Event OSC)
runMCEchords = do
  let f xs _ = do
        g <- newStdGen
        let chords = fst $ chooseOne mceChords g
            dur = fst $ chooseOne mceDurs g
        return ((dur,chords):xs)
  ps <- foldM f [] [1..20]
  let durs = map fst ps
      chords = transpose $ map (map (toFreq . (+7)) . snd) ps
      durs' = scanl (+) 0 durs
      es = map (listE . zip durs' . map toOSC . zip durs) chords
  return $ mconcat es
    where 
      toFreq x = freq $ defaultPitch {degree=x+7}
      toOSC (d,f) = s_new "simpleSynth" (-1) AddToTail 1 [("dur",d),("freq",f)]


-- $customSynthdefs
-- 
-- Playing custom synthdef. This way of use of Pattern seems to be
-- suited to be rewritten by using Data.Map in haskell.
-- Try:
--
-- > > prepareStretched
-- > > spawn 0 60 =<< runStretched
-- 

-- | Scratch test for pitchShift ugen.
psTest :: IO ()
psTest = audition $ out 0 $ pitchShift (sinOsc ar 440 0) 0.2 r d 0
    where
      r = mouseX kr 0.5 2.0 Linear 0.1
      d = mouseY kr 0.0 0.1 Linear 0.1

-- | Synth to controlled by pattern, in cookbook.
stretchedFragments :: UGen
stretchedFragments = out A.out $ mce [sig, sig]
    where
      sig = pitchShift src 0.2 A.stretch 1 1
      src = playBuf 1 A.bufnum (recip A.stretch) 1 A.start NoLoop
            RemoveSynth * e
      e = linen 1 A.attack A.time A.decay RemoveSynth
      -- e = envGen kr 1 1 0 1 RemoveSynth shape
      -- shape = linen 1 A.attack A.time A.decay RemoveSynth

stretchedBufnum :: Int
stretchedBufnum = 1

prepareStretched :: IO ()
prepareStretched = do
  withSC3 (\fd -> 
     loadSynthdef "stretchedFragments" stretchedFragments fd >>
     send fd (b_allocRead stretchedBufnum 
              "/home/atsuro/audio/wav/a11wlk01.wav" 0 0) >>
     wait fd "/done")
  return ()

cleanUpStretched :: IO ()
cleanUpStretched = do
  withSC3 (\fd -> send fd (b_free stretchedBufnum) >> 
                  wait fd "/done")
  return ()

runStretched :: IO (Event OSC)
runStretched = mapToEventOSC "stretchedFragments" 1 <$> stretchedMessages

stretchedMessages :: IO (Map String [Double])
stretchedMessages = do
  g <- newStdGen
  b <- getBufInfo stretchedBufnum
  let times = randomRs (0.2, 1.5) g
  let msg = M.fromList $ 
        [("bufnum", repeat $ fromIntegral stretchedBufnum),
         ("start", randomRs (0, fromIntegral (bufNumFrames b) * 0.7) g),
         ("delta", times),
         ("time", times),
         ("stretch", randomRs (1.0, 4.0) g),
         ("amp", repeat 0.5),
         ("attack", repeat 0.1),
         ("decay", repeat 0.2)]
  return msg

mapToEventOSC :: String -> Int -> Map String [Double] -> Event OSC
mapToEventOSC name gId m = listE $ zip durs (mkSNew' name gId m)
    where durs = scanl (+) 0 (maybe [] id $ M.lookup "time" m)