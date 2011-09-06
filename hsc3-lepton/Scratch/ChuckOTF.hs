{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Translation of /on-the-fly synchronization (concurrent)/ chuck example
by Perry and Ge, from: <http://chuck.cs.princeton.edu/doc/examples/> .

Synchronizations are done with calculating offset time of OSC bundle
timestamps. Each events starts from multiple of unit time.

-}
module Scratch.ChuckOTF where

import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Scratch.Client

-- ---------------------------------------------------------------------------
-- Synth defs

otfperc = out ("out"@@0) sig where
  sig = pan2 (pb * ("amp"@@0.3)) ("pan"@@0) 1
  pb = fvb $ playBuf 1 ("bnum"@@0) 1 1 0 NoLoop RemoveSynth
  fvb k = freeVerb k 0.5 0.5 0.5

otfsine = out ("out"@@0) sig where
  sig = pan2 (rev $ sinOsc AR ("freq"@@0) 0 * ("amp"@@0)) ("pan"@@0) 1
  rev k = k * 0.6 + foldr f k [1..8::Int] * 0.4
  f a b = allpassC b 0.8 (rand a 1e-3 8e-2) (rand a 4e-2 4)

-- ---------------------------------------------------------------------------
-- Patterns

kikP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever t)
  ,("bnum", prepeat 0)
  ,("pan", prepeat (-0.1))
  ,("amp", pforever (prange 0.5 0.7))]
  
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

sin1P = pnset sin1N
  [("dur", pforever (t * 0.25))
  ,("freq",
    pforever $ midiCPS (21 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
  ,("amp", pforever 0.4)
  ,("pan", pforever 0.2)]

sin2P = pnset sin2N
  [("dur", pforever (t * prand 1 [0.5,0.25]))
  ,("freq",
    pforever $ midiCPS (69 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
  ,("amp", pforever 0.2)
  ,("pan", pforever (-0.2))]
  
t = 0.5

sin1N = 0xbaca

sin2N = 0xcaba

-- ---------------------------------------------------------------------------
-- Sends synthdefs, allocate buffers, add new synth nodes.

setup'otf = withSC3 $ \fd -> do
  reset fd
  async fd $ bundle immediately $
    d_recv (synthdef "otfperc" otfperc) :
    d_recv (synthdef "otfsine" otfsine) :
    (zipWith (\i file -> b_allocRead i (soundsDir </> file) 0 0)
     [0..]
     ["kick.wav", "snare-hop.wav", "hihat.wav", "hihat-open.wav"])
  send fd $ bundle immediately $
    [s_new "otfsine" sin1N AddToHead 1 []
    ,s_new "otfsine" sin2N AddToHead 1 [] ]
    
soundsDir = "/home/atsuro/Downloads/chuck-1.2.1.3/examples/data"

-- ---------------------------------------------------------------------------
-- After above setup, execute below actions each by each.
-- Or, add all at once.

addKik = addPat 0 "kik" kikP
addHat = addPat 0 "hat" hatP
addHato = addPat 0 "hat-open" hatoP
addSnr = addPat t "snr" snrP
addSin1 = addPat 0 "sin-lo" sin1P
addSin2 = addPat 0 "sin-hi" sin2P

addAll = sequence_ [addKik, addSnr, addHat, addHato, addSin1, addSin2]

addPat d n e = withLept $ \fd -> send fd =<< bundle' (t*2) d [l_new n e]

delPat d n = withLept $ \fd -> send fd =<< bundle' (t*2) d [l_free n]

-- --------------------------------------------------------------------------
-- Cleaning up

resetAll = withLept (flip send l_freeAll)