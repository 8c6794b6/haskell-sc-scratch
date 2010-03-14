------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook07
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook06_Phrase_Network/.
--
--

module SCHelp.PG.Cookbook07 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid (mappend, mconcat)
import System.Random
import qualified Data.IntMap as IM
import qualified Data.Map as M

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCSched
import SCTree
import SCQuery
import qualified Scratch.ControlArgs as A

runRhythmicVariations :: IO ()
runRhythmicVariations = undefined

setRhythmicVariations :: IO OSC
setRhythmicVariations = withSC3 $ \fd -> do
  loadSynthdef "kik" kik fd
  join (loadSynthdef "kraftySnr" <$> kraftySnr <*> pure fd)


-- | Synth for kick sound.
kik :: UGen
kik = out A.out $ mce [sig, sig]
    where
      sig = distort (sinOsc ar fcurve (0.5 * pi) * preamp) * e *
            A.amp {controlDefault = 1}
      e = envGen kr 1 1 0 1 RemoveSynth $
          envCoord [(0,decay1), (decay1L,decay2), (0,0)] 1 1 EnvCub
      fcurve = envGen kr 1 1 0 1 DoNothing $
               envCoord [(0, basefreq * ratio), (sweeptime, basefreq)]
                            1 1 EnvCub
      preamp = A.preamp {controlDefault=1}
      decay1 = "decay1" @= 0.3
      decay2 = "decay2" @= 0.15
      decay1L = "decay1L" @= 0.8
      basefreq = "basefreq" @= 50
      ratio = "ratio" @= 7
      sweeptime = "sweeptime" @= 0.05


-- | Synth for snare and hihat sound.
kraftySnr :: IO UGen
kraftySnr = do
  sig <- pinkNoise ar >>. (* A.amp)
  let env = envGen kr 1 1 0 1 RemoveSynth $
            envPerc 0.01 (A.decay {controlDefault=0.3})
      sig' = bpf sig (A.freq {controlDefault=2000})
             (A.rq {controlDefault=3}) * env
  return $ out A.out (pan2 sig' A.pan 1)



type RhythmPhrase = Map String [Double]

type RhythmDelta = RhythmPhrase -> SynthName -> Int -> StdGen -> Event OSC

data RhythmStatus = RhythmStatus
                  { rhythmCurrentBar :: Int,
                    rhythmInstrument :: SynthName,
                    rhythmPhrase :: RhythmPhrase,
                    rhythmDelta :: RhythmDelta,
                    rhythmSeed :: StdGen }

hhInit :: IO RhythmStatus
hhInit = RhythmStatus 0 "kraftySnr" hhBase hhDelta <$> newStdGen

hhBase :: RhythmPhrase
hhBase = M.fromList $
         [("amp", replicate 16 1),
          ("dur", replicate 16 0.25),
          ("rq", repeat 0.06),
          ("decay", repeat 0.04),
          ("freq", repeat 12000)]

hhDelta :: RhythmDelta
hhDelta phr name t0 g = listE $ zip dur osc
    where

      inLast4 :: Bool
      inLast4 = 12 <= tR && tR < 16 where tR = t0 `mod` 16

      indices :: Int -> [Int]
      indices num = take num $ fst $ shuffle [0..len - 1] g
         where len = length (maybe [] id (M.lookup "dur" phr))

      phr' :: RhythmPhrase
      phr' | inLast4 = scramble 5
           | otherwise = scramble 1
          where scramble n = M.update (updateDurs (indices n)) "dur" .
                             M.update (updateAmps (indices n)) "amp" $
                             phr

      dur :: [Double]
      dur = scanl (+) (fromIntegral t0) .
            maybe [] id .
            M.lookup "dur" $ phr'

      osc :: [OSC]
      osc = mkSNew' name 1 phr'

      updateDurs :: [Int] -> [Double] -> Maybe [Double]
      updateDurs is xs = return xs'
          where
            xs' :: [Double]
            xs' = concat $ IM.elems $ foldr f v is

            v :: IntMap [Double]
            v = IM.fromList $ zip [0..] (map return xs)

            f :: IM.Key -> IntMap [Double] -> IntMap [Double]
            f = IM.update (\_ -> return [0.125,0.125])

      updateAmps :: [Int] -> [Double] -> Maybe [Double]
      updateAmps is xs = return xs'
          where
            xs' :: [Double]
            xs' = concat $ IM.elems $ foldr f v is
            
            v :: IntMap [Double]
            v = IM.fromList $ zip [0..] (map return xs)
            
            f :: IM.Key -> IntMap [Double] -> IntMap [Double]
            f = IM.update (\_ -> return [15,10])

hhPhrase :: State RhythmStatus (Event OSC)
hhPhrase = do
  RhythmStatus current name phr f g <- get
  let (_, g') = next g
  put $ RhythmStatus (current + 4) name phr f g'
  return $ f phr name current g

hhEvent :: Int -> IO (Event OSC)
hhEvent n = evalState (mconcat <$> replicateM n hhPhrase) <$> hhInit