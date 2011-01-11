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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_fire.html>
--
-- Try:
--
-- > > audition $ out 0 $ hissing $ whiteNoise 'a' ar
-- > > audition $ out 0 $ crackling $ whiteNoise 'a' ar
-- > > audition $ out 0 $ flaming $ whiteNoise 'a' ar
-- > > audition $ out 0 $ mkFire $ whiteNoise 'a' ar
-- > > audition fourFires
--
module Pssd.Earth.Fire where

import Sound.SC3
import Sound.SC3.ID

-- | Hissing sound.
--
-- Using @onePole@ as filter for white noise to emulate pure data's
-- implementation.
--
hissing :: UGen -- ^ input noise
        -> UGen
hissing noise = mce2 sig sig
  where
    sig = noise1 * noise2
    noise1 = hpf noise 1000
    noise2 = cubed (onePole noise 0.998 * 4) * 12

-- | Crackling sound.
--
-- Using @dust@ for triggering, squared @linen@ for amplitude envelope, and
-- @tRand@ for random values.
--
crackling :: UGen -- ^ input noise
          -> UGen
crackling noise = mce2 sig sig
  where
    sig = bpf noise freq 1 * amp
    freq = randVal * 500 + 1500
    amp = squared $ linen trg 1e-4 1 (randVal/1e3) DoNothing
    randVal = tRand 'r' 1 30 trg
    trg = dust 't' kr 10

-- | Flaming sound.
--
-- Again, using @onePole@ for high pass filter.
--
flames :: UGen -- ^ input noise
       -> UGen
flames noise = mce2 sig sig
  where
    sig = hp sig1 * 0.6
    sig1 = clip sig2 (-0.9) 0.9
    sig2 = hp sig3
    sig3 = bpf noise 30 5 * 100
    hp x = onePole x 0.996

-- | Make fire sound from single noise.
mkFire :: UGen -- ^ input noise
       -> UGen
mkFire nz = crackling nz * 0.2 + hissing nz * 0.3 + flames nz * 0.6

-- | Ensemble of four fires.
fourFires :: UGen
fourFires = out 0 $ sum [f1, f2, f3, f4]
  where
    f1 = bpf (mkFire $ whiteNoise 'a' ar) 600 0.2
    f2 = bpf (mkFire $ whiteNoise 'b' ar) 1200 0.6
    f3 = bpf (mkFire $ whiteNoise 'c' ar) 2600 0.4
    f4 = onePole (mkFire $ whiteNoise 'd' ar) 0.9
