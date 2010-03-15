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
-- Current problem for this implementation is performance.
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
runRhythmicVariations = do
  let nb = 8 
      bpm = 128
  e1 <- mkEvent nb <$> hhInit
  e2 <- mkEvent nb <$> snrInit
  e3 <- mkEvent nb <$> kikInit
  spawn 0 bpm $ mconcat [e1, e2, e3]

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

-- $helper
--
-- Helper datatype, type synonyms, and functions.

type RhythmPhrase = Map String [Double]

type RhythmDelta = RhythmPhrase -> SynthName -> Int -> StdGen -> Event OSC

data RhythmStatus = RhythmStatus
                  { rhythmCurrentBar :: Int,
                    rhythmInstrument :: SynthName,
                    rhythmPhrase :: RhythmPhrase,
                    rhythmDelta :: RhythmDelta,
                    rhythmSeed :: StdGen }

getDurs :: RhythmPhrase -> Int -> [Double]
getDurs phr t0 = scanl (+) (fromIntegral t0) .
                 maybe [] id . 
                 M.lookup "dur" $ phr

getOSCs :: RhythmPhrase -> SynthName -> [OSC]
getOSCs phr name = mkSNew' name 1 phr

chooseIndices :: RandomGen g => Int -> Int -> g -> [Int]
chooseIndices num len g = take num $ fst $ shuffle [0..len] g

inLast4 :: Int -> Bool
inLast4 n = 12 <= n' && n' < 16 where n' = n `mod` 16

runRhythm :: State RhythmStatus (Event OSC)
runRhythm = do
  RhythmStatus current name phr f g <- get
  let (_, g') = next g
  put $ RhythmStatus (current + 4) name phr f g'
  return $ f phr name current g

-- | Current implementation increases cpu usage when repetation
-- increases. Fix this.
mkEvent :: Int -> RhythmStatus -> Event OSC
mkEvent n st = evalState (mconcat <$> replicateM n runRhythm) st

-- $hihat 
--
-- Functions for hihat rhythm.

hhInit :: IO RhythmStatus
hhInit = RhythmStatus 0 "kraftySnr" hhBase hhDelta <$> newStdGen

-- | Base pattern for hihat.
-- 
-- Keys other than @amp@ nor @dur@ are repeated infinitly.
hhBase :: RhythmPhrase
hhBase = M.fromList $
         [("amp", replicate 16 1),
          ("dur", replicate 16 0.25),
          ("rq", repeat 0.06),
          ("decay", repeat 0.04),
          ("freq", repeat 12000)]

hhDelta :: RhythmDelta
hhDelta phr name t0 g = listE $ zip durs oscs 
    where

      durs :: [TimeT]
      durs = getDurs phr' t0 
             
      oscs :: [OSC]
      oscs = getOSCs phr' name

      indices :: Int -> [Int]
      indices n = chooseIndices n 15 g

      phr' :: RhythmPhrase
      phr' | inLast4 t0 = scramble 5
           | otherwise = scramble 1
          where scramble n = M.update (updateDurs is) "dur" .
                             M.update (updateAmps is) "amp" $
                             phr
                    where is = indices n

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

-- $snare
-- 
-- Functions for snare rhythms.

getRestIndices :: RhythmPhrase -> [Int]
getRestIndices phr = map fst $ 
                     filter (\(a,b) -> b == 0) $ 
                     zip [0..] $ 
                     maybe [] id $ M.lookup "amp" phr

snrBase :: RhythmPhrase
snrBase = M.fromList $
          [("amp", [0, 0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 0, 1, 0, 0, 0]),
           ("decay", [0, 0, 0, 0, 0.7, 0, 0, 0, 
                      0, 0, 0, 0, 0.4, 0, 0, 0]),
           ("freq", repeat 5000),
           ("dur", replicate 16 0.25)]

snrInit :: IO RhythmStatus
snrInit = RhythmStatus 0 "kraftySnr" snrBase snrDelta <$> newStdGen

snrDelta :: RhythmDelta
snrDelta phr name t0 g = listE $ zip durs oscs 
    where 
      durs :: [TimeT]
      durs = getDurs phr' t0

      oscs :: [OSC]
      oscs = getOSCs phr' name

      phr' :: RhythmPhrase
      phr' | inLast4 t0 = let is = idx (5, 9) in
               M.update (updateAmps g is) "amp" . 
               M.update (updateDecays is) "decay" $ phr

           | otherwise = let is = idx (1, 3) in
               M.update (updateAmps g is) "amp" . 
               M.update (updateDecays is) "decay" $ phr

      idx :: (Int,Int) -> [Int]
      idx range = take (fst $ randomR range g) $ fst $ shuffle xs g
          where xs = getRestIndices phr

      updateDecays :: [Int] -> [Double] -> Maybe [Double]
      updateDecays is xs = return xs'
          where
            xs' = concat $ IM.elems $ fst $ foldr f v is
            f i (m,gen) = (m', gen')
                where m' = IM.update (\_ -> return $ return $ fst $ 
                                            randomR (0.2, 0.4) gen)
                           i m
                      gen' = snd $ next gen
            v = (IM.fromList $ zip [0..] (map return xs), g)

      updateAmps :: StdGen -> [Int] -> [Double] -> Maybe [Double]
      updateAmps g is xs = return xs'
        where
          xs' :: [Double]
          xs' = concat $ IM.elems $ fst $ foldr f v is

          f :: Int 
            -> (IntMap [Double], StdGen) 
            -> (IntMap [Double], StdGen)
          f i (m, gen) = (m', gen')
            where m' = IM.update (\_ -> return $ return $ fst $ 
                                        randomR (0.15, 0.3) gen) i m
                  gen' = snd $ next gen

          v :: (IntMap [Double], StdGen)
          v = (IM.fromList $ zip [0..] (map return xs), g)

-- $kick
-- 
-- Functions for kick rhythms

kikBase :: RhythmPhrase
kikBase = M.fromList $
          [("amp", [1,0,0,0, 0,0,0.7,0, 
                    0,1,0,0, 0,0,0,0]),
           ("decay", [0.15,0,0,0, 0,0,0.15,0,
                      0,0.15,0,0, 0,0,0,0]),
           ("preamp", replicate 16 0.4),
           ("dur", replicate 16  0.25)]

kikInit :: IO RhythmStatus
kikInit = RhythmStatus 0 "kik" kikBase kikDelta <$> newStdGen

kikDelta :: RhythmDelta
kikDelta phr name t0 g = listE $ zip durs oscs
    where
      durs :: [Double]
      durs = getDurs phr' t0

      oscs :: [OSC]
      oscs = getOSCs phr' name

      phr' :: RhythmPhrase
      phr' | inLast4 t0 = let is = idx (5, 10) in
                          M.update (updateAmps g is) "amp" .
                          M.update (updateDecays is) "decay" $ phr
           | otherwise = let is = idx (0, 2) in
                         M.update (updateAmps g is) "amp" .
                         M.update (updateDecays is) "decay" $ phr

      idx :: (Int,Int) -> [Int]
      idx range = take (fst $ randomR range g) $ 
                  (fst $ shuffle (getRestIndices phr) g)

      updateDecays :: [Int] -> [Double] -> Maybe [Double]
      updateDecays is xs = return xs'
          where
            xs' = concat $ IM.elems $ fst $ foldr f v is
            f i (m,gen) = (m', gen')
                where m' = IM.update (\_ -> return $ return $ fst $ 
                                            randomR (0.2, 0.5) gen)
                           i m
                      gen' = snd $ next gen
            v = (IM.fromList $ zip [0..] (map return xs), g)

      updateAmps :: StdGen -> [Int] -> [Double] -> Maybe [Double]
      updateAmps g is xs = return xs'
        where
          xs' :: [Double]
          xs' = concat $ IM.elems $ fst $ foldr f v is

          f :: Int 
            -> (IntMap [Double], StdGen) 
            -> (IntMap [Double], StdGen)
          f i (m, gen) = (m', gen')
            where m' = IM.update (\_ -> return $ return $ fst $ 
                                        randomR (0.05, 0.10) gen) i m
                  gen' = snd $ next gen

          v :: (IntMap [Double], StdGen)
          v = (IM.fromList $ zip [0..] (map return xs), g)
