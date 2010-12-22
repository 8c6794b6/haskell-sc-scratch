------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Fire
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Fire>
--
-- /Example/:
--
-- > > audition $ out 0 simplest
-- > > audition $ out 0 fireGen
-- > > audition $ out 0 polyFire
--
module DesigningSound.Fire where

import Sound.SC3
import Sound.SC3.ID

-- | Simplest random hissing sound
--
-- > {WhiteNoise.ar(LFNoise2.kr(1))}.play
--
simplest :: UGen
simplest = whiteNoise 'a' ar * lfNoise2 'b' kr 1

-- | Square it
--
-- > {WhiteNoise.ar(LFNoise2.kr(1).squared)}.play
--
squaredSimplest :: UGen
squaredSimplest = whiteNoise 'a' ar * squared (lfNoise2 'b' kr 1)

-- | Add another squared for even stronger effect on the dynamics, and use a
-- high-pass filter to make the hiss more hissy
--
-- > {WhiteNoise.ar * Line.ar(1, 0, 0.02, doneAction: 2)}.play
--
moreSquared :: UGen
moreSquared = hpf (whiteNoise 'a' ar) 1000 * squared (squared (lfNoise2 'b' kr 1))

-- | Many crackles
--
-- If we use a proper envelope generator instead of a simple line, we can
-- retrigger it randomly with Dust to give random crackles of a controllable
-- density
--
-- > {WhiteNoise.ar * EnvGen.ar(Env.perc(0, 0.02, curve: 0), Dust.kr(1))}.play
--
manyCrackles :: UGen
manyCrackles = whiteNoise 'a' ar *
               envGen ar (dust 'b' kr 1) 1 0 1 DoNothing (envPerc 0 0.02)

-- | More variation
--
-- > {
-- >   var trigs, durscale, son, resfreq;
-- >   trigs = Dust.kr(1);
-- >   durscale = TRand.kr(1, 1.5, trigs); // vary duration between default 20ms and 30ms
-- >   resfreq = TExpRand.kr(100, 1000, trigs); // different resonant frequency for each one
-- >   son = WhiteNoise.ar * EnvGen.ar(Env.perc(0, 0.02, curve: 0), trigs, timeScale: durscale);
-- >   son = son + BPF.ar(son, resfreq, 20);
-- > }.play
--
moreVariation :: UGen
moreVariation = son
  where
    son = son' + bpf son' resFreq 20
    son' = whiteNoise 'a' ar * envGen ar trigs 1 0 durScale DoNothing shape
    shape = envPerc 0 0.02
    durScale = tRand 'b' 1 1.5 trigs
    resFreq = tExpRand 'c' 100 1000 trigs
    trigs = dust 'd' kr 1

-- | Simple attempt at the low "woofing" noise made by the flames themselves
--
-- > {LPF.ar(WhiteNoise.ar, 30) * 100}.play
--
woof :: UGen
woof = lpf (whiteNoise 'a' ar) 30 * 100

-- | Another component we could use to build up a flamey sound
--
-- > {BPF.ar(WhiteNoise.ar, 30, 0.2) * 20}.play
--
woosh :: UGen
woosh = bpf (whiteNoise 'a' ar) 30 0.2 * 20

-- | Shaping the dynamic range and discarding some of the lower frequencies,
-- allowing a little clipping to give a less static sound.
--
-- > {LeakDC.ar(LeakDC.ar(BPF.ar(WhiteNoise.ar, 30, 0.2) * 50).clip2(0.9)) * 0.5}.play
--
shaping :: UGen
shaping = leakDC (leakDC
                  (clip2 (bpf (whiteNoise 'a' ar) 30 0.2 * 50) 0.9) 0.995)
          0.995 * 0.5

-- | Putting it all together
--
-- > ~firegen = {
-- >    var trigs, durscale, resfreq;
-- >    var noise, hissing, crackles, lapping;
-- >    // A common noise source
-- >    noise = WhiteNoise.ar;
-- >    // Hissing
-- >    hissing = HPF.ar(noise, 1000) * LFNoise2.kr(1).squared.squared;
-- >    // Crackle
-- >    trigs = Dust.kr(1);
-- >    durscale = TRand.kr(1, 1.5, trigs); // vary duration between default 20ms and 30ms
-- >    resfreq = TExpRand.kr(100, 1000, trigs); // different resonant frequency for each one
-- >    crackles = noise * EnvGen.ar(Env.perc(0, 0.02, curve: 0), trigs, timeScale: durscale);
-- >    crackles = crackles + BPF.ar(crackles, resfreq, 20);
-- >    // Flame
-- >    lapping = LeakDC.ar(LeakDC.ar(BPF.ar(noise, 30, 0.2) * 50).clip2(0.9)) * 0.5;
-- >    // Combine them:
-- >    ([crackles, hissing, lapping] * [0.1, 0.3, 0.6]).sum * 3
-- > };
--
fireGen :: UGen
fireGen = sum (zipWith (*) [crackles, hissing, lapping] [0.1, 0.3, 0.6]) * 3
  where
    crackles = crackles' + bpf crackles' resFreq 20
    crackles' = noise * envGen ar trigs 1 0 durScale DoNothing shape
    shape = envPerc 0 0.02  -- curve?
    resFreq = tExpRand 'a' 100 1000 trigs
    trigs = dust 'b' kr 1
    hissing = hpf noise 100 * squared (squared (lfNoise2 'c' kr 1))
    noise = whiteNoise 'd' ar
    durScale = tRand 'e' 1 1.5 trigs
    lapping = leakDC (leakDC (clip2 (bpf noise 30 0.2 * 50) 0.9) 0.995) 0.995 *
              0.5

-- | Lets have four of the above, each filtered differently, for a composite
-- effect
--
-- > {
-- >   BPF.ar(~firegen,  600, 1/0.2) +
-- >   BPF.ar(~firegen, 1200, 1/0.6) +
-- >   BPF.ar(~firegen, 2600, 1/0.4) +
-- >   HPF.ar(~firegen, 1000)
-- > }.play
--
polyFire :: UGen
polyFire = sum [ bpf fireGen 600 (1/0.2)
               , bpf fireGen 1200 (1/0.6)
               , bpf fireGen 2600 (1/0.4)
               , hpf fireGen 1000]
