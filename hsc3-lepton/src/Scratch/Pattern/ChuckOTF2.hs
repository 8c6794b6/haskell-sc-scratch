{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Rewrite of ChuckOTF using L.

-}
module Scratch.Pattern.ChuckOTF2 where

import System.FilePath

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

setup'otf = withSC3 $ do
  reset
  mapM_ async $
    (map (d_recv . uncurry synthdef))
     [("otfperc",otfperc),("otfsine", otfsine)
     ,("otfrev1", otfrev1),("otfrev2",otfrev2)] ++
    (zipWith (\i file -> b_allocRead i (soundsDir </> file) 0 0)
     [0..]
     ["kick.wav", "snare-hop.wav", "hihat.wav", "hihat-open.wav"])
  patchNode otfNodes

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
-- Synth defs

otfperc = out ("out"@@0) sig where
  sig = pan2 (pb * ("amp"@@0.3)) ("pan"@@0) 1
  pb = playBuf 1 AR ("bnum"@@0) 1 1 0 NoLoop RemoveSynth

otfsine = out ("out"@@0) sig where
  sig = pan2 (sinOsc AR ("freq"@@0) 0 * e) ("pan"@@0) 1
  e = envGen KR 1 ("amp"@@0) 0 ("dur"@@1) RemoveSynth $
      Envelope [0,1,1,0] [1e-3,998e-3,1e-3] [EnvLin] Nothing Nothing

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
  [("dur", pforever (d t))
  ,("bnum", pforever (d 0))
  ,("pan", pforever (d (-0.1)))
  ,("amp", pforever (pdrange (d 0.7) (d 0.9)))]

snrP = psnew "otfperc" Nothing AddToHead 1
  [("dur",
    (pforever (d t) *@ pforever (prnd [d 2, pconcat (ds [0.75,1.25])])))
  ,("bnum", pforever (d 1))
  ,("pan", pforever (d 0.3))]

hatP = psnew "otfperc" Nothing AddToHead 1
  [("dur",
    pforever (d t) *@ pforever (prnd [d 0.5, pconcat (ds [0.25,0.25])]))
  ,("bnum", pforever (d 2))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.3) (d 0.4)))]

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (d t))
  ,("bnum", pforever (d 3))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.2) (d 0.4)))]

sin1P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (d (0.25*t)))
  ,("out", pforever (d 2))
  ,("freq",
    let fs = [midiCPS (21+x+y)|x<-[0,12,24,36],y<-[0,2,4,7,9]] in
    pforever (prnd (ds fs)))
  ,("amp", pforever (d 0.65))
  ,("pan", pforever (d 0.1))]

sin2P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (prnd (ds [0.5*t,0.25*t])))
  ,("out", pforever (d 2))
  ,("freq",
    let fs = [midiCPS (69+x+y)|x<-[0,12,24,36],y<-[0,2,4,7,9]] in
    pforever (prnd (ds fs)))
  ,("amp", pforever (d 0.2))
  ,("pan", pforever (d (-0.2)))]

-- t = 0.5
-- t = 0.52
t = 60 / 115

d = pdouble
ds = map pdouble
i = pint
prnd = prand (i 1)

------------------------------------------------------------------------------
-- Actions for patterns

addKik, addHat, addHato, addSnr, addSin1, addSin2 :: IO ()
addKik = addPat 0 "kik" kikP
addHat = addPat 0 "hat" hatP
addHato = addPat 0 "hato" hatoP
addSnr = addPat t "snr" snrP
addSin1 = addPat 0 "sin1" sin1P
addSin2 = addPat 0 "sin2" sin2P
addPat d n e = withLept . sendOSC =<< bundle' (t*2) d [l_new n e]

resetAll = withLept (send l_freeAll)

dumpPat = withLept . send $ l_dump

addAll = sequence_ [addKik, addSnr, addHat, addHato, addSin1, addSin2]

addAllButSnare' = bundle' (t*2) 0
  [ l_new "kik" kikP, l_new "hat" hatP, l_new "hato" hatoP
  , l_new "sin1" sin1P, l_new "sin1" sin2P ]

delPat n = withLept . sendOSC =<< bundle' (t*2) 0 [l_free n]

pausePat n = leptseq =<< bundle' (t*2) 0 [l_pause n]
runPat n = leptseq =<< bundle' (t*2) 0 [l_run n]

{-

-- Workspace

setup'otf
addAll
leptseq l_dump
leptseq l_freeAll

mapM_ delPat ["sin2", "sin2"]

delPat "sin2" >> addSin1
addHat >> addHato >> addSnr >> addSin2 >> delPat "kik" >> delPat "sin1"
addHat >> addKik >> addSnr >> addSin2
addKik >> addSnr >> addHato >> addSin1

addHato
delPat "hat"
addHat
delPat "hato"
delPat "sin1"
delPat "snr"

sequence_ [addSin1, addSin2]
sequence_ [delPat "hat", delPat "hato"]

sequence_ [delPat "snr", delPat "kik"] >> sequence_ [addHat, addHato]
sequence_ [delPat "hat", delPat "hato"] >> sequence_ [addSnr, addKik]
sequence_ [delPat "snr", addHat, addSnr]

mapM_ delPat ["snr", "sin2", "hat", "hato"]
delPat "kik"
sequence_ [addKik, addSnr, addSin2, addHat, addHato]

delPat "sin2"
sequence_ [addSin2, addSnr, delPat "kik", delPat "hat", delPat "hato"]

mapM_ delPat ["hat", "hato"]
sequence_ [addHat, addHato]

addKik
addHat >> addHato
addSnr

addSin1 >> delPat "kik"
addHat
delPat "kik"
addSin1

sequence_ [addHat, addHato]

addSin1 >> addSnr >> addKik

-}
