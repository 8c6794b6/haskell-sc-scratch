------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_rain.html>
--
-- Try:
--
-- > > audition rain
--
module Pssd.Earth.Rain where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

mkDroplet :: UGen -- ^ Noise source
          -> UGen -- ^ freq mul
          -> UGen -- ^ freq add
          -> UGen
mkDroplet nz fmul fadd = lpf (hpf (hpf sig 500) 500 * 0.01) 10000
  where
    sig = sig1 * sig2 * 0.3
    sig1 = cubed $ cubed $ nz * 3.2
    sig2 = sinOsc ar ((nz * 3.2 * fmul) + fadd) 0

mkRumble :: UGen -- ^ Noise source 1
         -> UGen -- ^ Noise source 2
         -> UGen
mkRumble n1 n2 = bpf (n1*n2) 333 2 * 0.4

mkDrips :: UGen -- ^ Noise source 1
        -> UGen -- ^ Noise source 2
        -> UGen -- ^ Noise source 3
        -> UGen
mkDrips n1 n2 n3 = bpf input freq 0.2 * 0.7
  where
    input = clip (cubed $ hpf (n1'/n2') 10000 * 0.03) 0 0.9
    freq = clip (lpf n3 16 * 300000 + 1000) 1000 10000
    n1' = lpf n1 1
    n2' = lpf n2 1900

mkAmbience :: UGen -- ^ Noise source
           -> UGen
mkAmbience = (*0.007)

dl :: UGen
dl = out 0 $ mce2 sig sig
  where
    sig = sum dls
    dls = [ mkDroplet (bpf (whiteNoise 'a' ar) 2.064 300) 344 3000
          , mkDroplet (bpf (whiteNoise 'b' ar) 2.064 300) 3000 2000
          , mkDroplet (bpf (whiteNoise 'c' ar) 2.064 300) 5000 2000 ]

rbl :: UGen
rbl = out 0 $ lpf (mkRumble n1 n2) 12000
  where
    n1 = whiteNoise 'a' ar
    n2 = whiteNoise 'b' ar

drp :: UGen
drp = out 0 $ mce2 sig sig
  where
    sig = lpf (mkDrips n1 n2 n3) 12000
    n1 = whiteNoise 'a' ar
    n2 = whiteNoise 'b' ar
    n3 = whiteNoise 'c' ar

rain :: UGen
rain = out2 $ sig * ctrl "amp" 1.0
  where
    sig = lpf noises 12000
    noises = sum [ droplet1
                 , droplet2
                 , droplet3
                 , rumble
                 , drips
                 , ambience ]
    droplet1 = mkDroplet (bp n1) 344 3000
    droplet2 = mkDroplet (bp n2) 3000 2000
    droplet3 = mkDroplet (bp n3) 5000 2000
    bp n = bpf n 2.064 300
    rumble = mkRumble n1 n2 * 0.1
    drips = mkDrips n1 n2 n3
    ambience = mkAmbience n1
    n1 = whiteNoise 'a' ar
    n2 = whiteNoise 'b' ar
    n3 = whiteNoise 'c' ar
