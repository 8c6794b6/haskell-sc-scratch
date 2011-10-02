{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Translation of /on-the-fly synchronization (concurrent)/ chuck example
by Perry and Ge, from: <http://chuck.cs.princeton.edu/doc/examples/> .

Synchronizations are done with calculating offset time of OSC bundle
timestamps. Each events starts from multiple of unit time.

Start pattern scsynth and pttern server, then load this module in ghci
and execute followings:

> > setup'otf
> > addKik
> > addHat
> > addHato
> > addSnr
> > addSin1
> > addSin2

patterns could be stopped:

> delPat 0 "snr"

added again:

> > addPat 0 "snr" snrP

after having enough:

> > resetAll

-}
module Scratch.ChuckOTF where

import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

------------------------------------------------------------------------------
-- Synth defs

otfperc = out ("out"@@0) sig where
  sig = pan2 (pb * ("amp"@@0.3)) ("pan"@@0) 1
  pb = playBuf 1 ("bnum"@@0) 1 1 0 NoLoop RemoveSynth

otfsine = out ("out"@@0) sig where
  sig = pan2 (sinOsc AR ("freq"@@0) 0 * e) ("pan"@@0) 1
  e = envGen KR 1 ("amp"@@0) 0 ("dur"@@1) RemoveSynth $
      env [0,1,1,0] [1e-3,998e-3,1e-3] [EnvLin] 0 0

otfrev1 = replaceOut ("out"@@0) sig where
  sig = freeVerb2 inl inr 0.5 0.5 0.5
  inl = (in' 1 AR ("inl"@@0))
  inr = (in' 1 AR ("inr"@@1))

otfrev2 = out ("out"@@0) sig where
  sig = ins * m + foldr f ins [1..8::Int] * (1-m)
  m = "mix"@@0.6
  ins = in' 2 AR ("in"@@0)
  f a b = allpassC b 0.8 (rand a 1e-3 8e-2) (rand a 4e-2 4)

------------------------------------------------------------------------------
-- Patterns

kikP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever t)
  ,("bnum", prepeat 0)
  ,("pan", prepeat (-0.1))
  ,("amp", pforever (prange 0.7 0.9))]

snrP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (prepeat t * prand 1 [2, plist [0.75,1.25]]))
  ,("bnum", prepeat 1)
  ,("pan", prepeat 0.3)]

hatP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (prepeat t * prand 1 [0.5, plist [0.25,0.25]]))
  ,("bnum", prepeat 2)
  ,("pan", prepeat (-0.3))
  ,("amp", pforever (prange 0.3 0.4))]

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever t)
  ,("bnum", prepeat 3)
  ,("pan", prepeat (-0.3))
  ,("amp", pforever (prange 0.2 0.4))]

sin1P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (t * 0.25))
  ,("out", prepeat 2)
  ,("freq",
    pforever $ midiCPS (21 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
  ,("amp", pforever 0.65)
  ,("pan", pforever 0.1)]

sin2P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (t * prand 1 [0.5,0.25]))
  ,("out", prepeat 2)
  ,("freq",
    pforever $ midiCPS (69 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
  ,("amp", pforever 0.2)
  ,("pan", pforever (-0.2))]

t = 0.5

sin1N = 0xbaca

sin2N = 0xcaba

------------------------------------------------------------------------------
-- Sends synthdefs, allocate buffers, add new synth nodes.

setup'otf = withSC3 $ \fd -> do
  reset fd
  async fd $ bundle immediately $
    (map (d_recv . uncurry synthdef))
     [("otfperc",otfperc),("otfsine", otfsine)
     ,("otfrev1", otfrev1),("otfrev2",otfrev2)] ++
    (zipWith (\i file -> b_allocRead i (soundsDir </> file) 0 0)
     [0..]
     ["kick.wav", "snare-hop.wav", "hihat.wav", "hihat-open.wav"])
  patchNode otfNodes fd

soundsDir = "/home/atsuro/Downloads/chuck-1.2.1.3/examples/data"

otfNodes =
  g 0
    [g 1 []
    ,g 2
      [s 1000 "otfrev1" []
      ,s 1001 "otfrev2" ["in":=2,"mix":=0.5]]]
  where
    g = Group; s = Synth

------------------------------------------------------------------------------
-- Actions for patterns

addKik = addPat 0 "kik" kikP
addHat = addPat 0 "hat" hatP
addHato = addPat 0 "hat-open" hatoP
addSnr = addPat t "snr" snrP
addSin1 = addPat 0 "sin-lo" sin1P
addSin2 = addPat 0 "sin-hi" sin2P

dumpPat = withLept . flip send $ l_dump

addAll = sequence_ [addKik, addSnr, addHat, addHato, addSin1, addSin2]

addAllButSnare' = bundle' (t*2) 0
  [ l_new "kik" kikP, l_new "hat" hatP, l_new "hat-open" hatoP
  , l_new "sin-lo" sin1P, l_new "sin-hi" sin2P ]

resetAll = withLept (flip send l_freeAll)
addPat d n e = withLept . flip send =<< bundle' (t*2) d [l_new n e]
delPat n = withLept . flip send =<< bundle' (t*2) 0 [l_free n]

pausePat n = leptseq =<< bundle' (t*2) 0 [l_pause n]
runPat n = leptseq =<< bundle' (t*2) 0 [l_run n]
