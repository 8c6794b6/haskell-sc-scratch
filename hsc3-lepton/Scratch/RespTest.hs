{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch, take 2.

-}
module Scratch.RespTest where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket, bracket_)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (setup)

import qualified Sound.SC3.Lepton.Pattern.Play as Play

-- | Load synth def and play the pattern.
gospe :: (Transport t) => t -> IO ()
gospe fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  mapPIO_ f pspe
  where
    f v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ 0.13 * 1e6)

gospe' :: Transport t => t -> IO ()
gospe' fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  play fd $ psnew "speSynth" Nothing AddToTail 1
    [("dur", pforever 0.13),("freq", midiCPS (pspe :: R Double))]

gospe'p p fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  play fd $ psnew "speSynth" Nothing AddToTail 1
    [("dur", pforever 0.13),("freq", midiCPS p)]


-- | Synthdef for spe example.
speSynth :: IO UGen
speSynth = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ mkSig dl dr
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen KR 1 ("amp"@@0.3) 0 1 RemoveSynth shp
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
    freq = control KR "freq" 440

pspe =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

pspe' = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13), ("freq", midiCPS pspe)]

-- ---------------------------------------------------------------------------
-- Parallel tests

type E = R (ToOSC Double)

-- m1,m2,m3,m4,m5,m6,m7,m8 :: E

silence n = psnew "silence" Nothing AddToTail 1 [("dur",n)]

m1 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", plist [1/4,3/4])
  ,("freq",midiCPS $ pcycle [67,69])
  ,("pan", pforever 0.3)]

m2 = psnew "rspdef1" Nothing AddToTail 1
  [("dur",  pseq 2 [3/4,1/4,2/4,2/4])
  ,("freq", midiCPS $ pseq 3 [60,62,63,65,67,68,70,72])]

m3 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 2 [2/8,2/8,3/8,1/8])
  ,("freq", midiCPS $ pcycle $ [60,67,74,81])]

m4 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 32 [1/32])
  ,("freq", midiCPS $ pcycle [115,103])]

m5 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 8 [1/4])
  ,("pan", prepeat 0.75)
  ,("freq", midiCPS $ pcycle [60,64,67,72])]

m6 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", plist [7/8, 1/8, 6/13, 7/13])
  ,("pan", prepeat (-0.75))
  ,("freq", midiCPS $ plist [84,86,89,91])]

m7 = silence 2

m8 =
  ppar
  [pforever
   (pchoose 1 [m2,m3,m4,m5,m6,m7])
  ,pcycle [m1,m1]
   -- [madjust "freq" (*2) m1
   -- ,madjust "freq" (*1.5) m1]
  ,pforever m1]
  -- ,pforever
  --  (madjust "freq" (*0.5) $
  --   madjust "pan" (const (-0.8)) $
  --   madjust "dur" (*2) $
  --   madjust "atk" (const 1) $
  --   m1)]

------------------------------------------------------------------------------
-- Sine and whitenoises

gosw :: Transport t => t -> IO ()
gosw fd =
  bracket_
    (Play.setup fd >> setup fd)
    (send fd $ n_free [1003])
    (play fd ps)
  where
    ps = psw :: E

gosw2 :: IO ()
gosw2 =
  bracket
    (mapM (forkIO . audition) [loop02,loop03 :: E])
    (mapM_ killThread)
    (const $ audition (loop01 :: E))

setup :: Transport t => t -> IO OSC
setup fd = do
  Play.setup fd
  async fd $ bundle immediately
    [d_recv $ synthdef "rspdef1" rspdef1
    ,d_recv $ synthdef "rspdef2" rspdef2
    ,d_recv $ synthdef "rspdef3" rspdef3
    ,d_recv $ synthdef "rspdef4" rspdef4
    ,d_recv $ synthdef "rspdef5" rspdef5]

rspdef1 :: UGen
rspdef1 =
  let phase = rand 'k' 0 pi in
  out 0 $ pan2
  (fSinOsc AR ("freq"@@440 * ("fmul"@@1 `lag2` 3.5)) phase * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef2 :: UGen
rspdef2 =
  out 0 $ pan2
  (resonz (whiteNoise 'd' AR)
   ("freq"@@1320)
   (clip ("q"@@0.8 * mouseY KR 0.125 4 Exponential 0.1) 1e-5 9999e-4) * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvSin] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef3 :: UGen
rspdef3 = out ("out"@@100) (tExpRand 'f' 0.25 4 ("t_trig"@@1))

rspdef4 :: UGen
rspdef4 = out 0 $
  lfPar AR ("freq"@@440 `lag` 0.25) 0 * ("amp"@@0.3 `lag3` 0.3)

-- | Variant of 'rspdef1', using 'in\'' ugen to map frequency factor.
rspdef5 :: UGen
rspdef5 =
  out 0 $ pan2
  (sinOsc AR ("freq"@@440 * (in' 1 KR ("fmul"@@100) `lag2` 3.5)) 0 *
   envGen KR ("t_trig"@@1) 0.3 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [preplicate 1024 (1/41)
                  ,preplicate 512 (2/41)
                  ,preplicate 256 (4/41)
                  ,preplicate 128 (8/41)])
  ,("freq", midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  ,("n_map/fmul", pforever 100)]

loop02 = psnew "rspdef2" Nothing AddToTail 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp $ prange (log 110) (log 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]

loop04 = psnew "rspdef1" Nothing AddToTail 1
  [("dur",  pforever $ prange 1e-3 7.5e-2)
  ,("freq", pforever $ exp $ prange (log 80) (log 12000))
  ,("atk",  let xs = take 1024 $ iterate (*1.006) 0.002
            in  pforever $ plist (xs ++ reverse xs))
  ,("dcy",  pforever $ prange 1e-4 2e-1)
  ,("amp",  pforever $ prange 1e-1 3e-1)
  ,("pan",  pforever $ prange (-1) 1)]

msg01 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 20e-3)
  ,("freq", preplicate 20 (prange 20 20000))
  ,("atk", pforever 1e-3)
  ,("dcy", pforever 5e-3)
  ,("amp", pforever 0.3)]

main = withSC3 gosw
