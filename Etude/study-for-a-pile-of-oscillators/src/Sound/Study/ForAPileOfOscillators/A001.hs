------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a pile of oscillators, take 1.
--
module Sound.Study.ForAPileOfOscillators.A001 where

import Data.List (zipWith4)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Sound.Study.ForAPileOfOscillators.Common

-- | For compiling GUI.
main :: IO ()
main = withSC3 $ \fd -> do
  treeToGui (Group 0 [Group 1 afp]) hints fd

-- | Write synthdefs to file, reload them, then make synth nodes.
setup :: (Transport t) => t -> IO ()
setup fd = do
  mapM_ (\(n,u) -> writeSynthdef n u) [("piece1",piece1)]
  reloadSynthdef fd
  mkTree a001Tree fd

-- | Main synth node tree.
a001Tree :: SCNode
a001Tree =
  Group 0
    [Group 1
      [Group 10 [Synth 1000 "piece1" ["bpm":=424]]
      ,Group 11 afp  
      ,Group 20 oscs]]

-- | Controller synth nodes.
afp :: [SCNode]
afp = [Synth 1001 "ac1"
         ["vc":=18e-3,"vd":=0.125,
          "mix":<-104,"nfreq":=0.68,
          "edgey":<-103,"edur":<-106,"crv":=(-12),
          "del":<-108,"chaos":<-107,
          "tfreq":<-110,"tmul":<-109,"tos":=0.9,
          "t_trig":=1]
      ,Synth 1002 "fc1"
         ["vc":=8000,"vd":=0.9999,
          "fc":<-112,"fd":=0.9999,
          "mix":<-102,
          "ptc":<-100,"vib":=0,"t_trig":<-101]
      ,Synth 1003 "pc1"
         ["fc":=1,"fd":=0.5,
          "vc":=0,"vd":=0.995,
          "t_trig":<-105]]

-- | Write osc data for non-realtime synthesis.
writeA001Score :: FilePath -> IO ()
writeA001Score path = do
  let os = zipWith (\t m -> Bundle (NTPr t) [m]) (repeat 0) (treeToOSC a001Tree)
      end = Bundle (NTPr 342) []
  writeNRT path $ os ++ [end]

piece1 :: UGen
piece1 = mrg outs
  where
    outs = [out pitchTrigBus pitchTrig
           ,out sweepTrigBus sweep
           ,out ftrigBus ftrig
           ,out ffcBus ffc
           ,out edgeyBus edgey
           ,out edurBus edur
           ,out tmulBus tmul
           ,out tfreqBus tfreq
           ,out aMixBus aMix
           ,out chaosBus chaos
           ,out delBus del
           ,out panBus panChange
           ,out rTrigBus hitRandTrig
           ,freeTheSound ]
           ++ hits

    hits = map hitFunc aBusses
    hitFunc k
      | r 0 = replaceOut (fromIntegral k) (decay2 pitchTrig 5e-3 300e-3 * 30e-3 * wholeEnv)
      | r 1 = replaceOut (fromIntegral k) (decay2 metro 5e-3 150e-3 * 20e-3 * wholeEnv)
      | otherwise = 0
      where
        r l = k `mod` 8 == l

    edgey = clip (lfdNoise3 'b' kr (1/(beat*128)) * 30) 0 1
    aMix = cubed (cubed (lfTri kr (1/(beat*512)) 0))
    edur = (950e-3 * (1.001 - eInvertedNoise)) * wholeEnv + 1e-3
    eInvertedNoise = squared $ lfdNoise3 'c' kr (1/(64*beat))
    chaos = gate chaosVal startChaos
    chaosVal = tRand 'd' 0 0.8 (lfNoise0 'e' kr (1/(beat*256)))
    startChaos = tDelay (512 <* pulseCount metro 0) 0
    del = gate delVal startDel
    delVal = tRand 'f' 1e-3 100e-3 (lfNoise0 'g' kr (1/(beat*256)))
    startDel = tDelay (756 <* pulseCount metro 0) 0
    tmul = tmulTransit initialTmul (tmulVal+0.5)
    tfreq = tmulTransit initialTfreq (tfreqVal+0.5)
    initialTmul = 5
    initialTfreq = 2.74
    tmulVal = squared $ squared $ lfdNoise3 'h' kr (1/(beat*128)) * 1.8
    tfreqVal = squared $ lfdNoise3 'i' kr (1/(beat*256)) * 1.4
    tmulTransit a b = a * (1-tmulTransitEnv) + b * tmulTransitEnv
    tmulTransitEnv = envGen kr startTmul 1 0 1 DoNothing $
                     env [0,0,1] [0,1] [EnvLin] (-1) 0
    startTmul = tDelay (1024 <* pulseCount metro 0) 0

    -- freqs
    pitchTrig = tDuty kr (dseq 'j' dinf (mce $ map (*beat) bars))
                0 DoNothing 1 0
    sweep = clip (randSweep + periodicSweep) 0 1
    randSweep = envGen kr sweepTrigR 1 0 1 DoNothing $
                env [0,0,1,1,0] [0,sweepAtk,sweepSus,sweepRel]
                [EnvSin] (-1) 0
    sweepAtk = tExpRand 'k' (1*beat) (12*beat) sweepTrigR
    sweepSus = tExpRand 'l' (9*beat) (36*beat) sweepTrigR
    sweepRel = tExpRand 'm' (1*beat) (12*beat) sweepTrigR
    sweepTrigR = dust 'n' kr (1/(beat*96))
    periodicSweep = shortSweep + longSweep
    shortSweep = 0
    longSweep = envGen kr sweepTrigPLong 1 0 1 DoNothing $
                env [0,0,1,0] [0,beat*3,beat*3] [EnvSin] (-1) 0
    sweepTrigPLong = pulseDivider metro (mce [32]) 2
    ftrig = select (stepper stepTrig 0 0 11 1 0) (mce pary)
    stepTrig = pulseDivider metro 64 1 +
               pulseDivider metro 256 44 +
               pulseDivider metro 256 45 +
               pulseDivider metro 256 48 +
               pulseDivider metro 256 52 +
               pulseDivider metro 256 56 +
               pulseDivider metro 256 60
    ffc = (squared (lfdNoise3 'p' kr (1/(beat*128))) * 3) + 1

    -- pan
    panChange = dust 'o' kr (1/(beat*128))

    -- common
    metro = impulse kr (bpm/60) 0
    hitRandTrig = pulseDivider metro 128 127
    freeTheSound = free (2048 <* pulseCount metro 0) 20
    beat = 60/bpm
    wholeEnv = squared $ envGen kr t_trig 1 0 1 DoNothing $
               env [0,0,1,1,0] [0,128*beat,1664*beat,256*beat] [EnvSin] (-1) 0

    pary = [0,  4, 11,  3, 10,  2,  9,  1,  8,  0,  7, 11,
            6, 10,  5,  9,  4,  8,  3,  7,  2,  6,  1,  5]
    bars = [3,3,2, 6,1,1, 3,3,2, 3,5]

    -- controls
    bpm = ctrl "bpm" 300
    pitchTrigBus = ctrl "pitch" 101
    sweepTrigBus = ctrl "sweep" 102
    ftrigBus = ctrl "ftrig" 100
    ffcBus = ctrl "ffc" 112
    edgeyBus = ctrl "edgey" 103
    edurBus = ctrl "envDur" 106
    tmulBus = ctrl "tmul" 109
    tfreqBus = ctrl "tfreq" 110
    aMixBus = ctrl "aMix" 104
    chaosBus = ctrl "chaos" 107
    delBus = ctrl "del" 108
    panBus = ctrl "pan" 105
    rTrigBus = ctrl "rtrig" 111
    t_trig = ctrl "t_trig" 1

