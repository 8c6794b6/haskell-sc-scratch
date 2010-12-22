------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Bouncing ball
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Rolling_can>
--
-- /Example/:
--
-- > > anon fourResonance
-- > > nset n [("t_trig", 1)]
-- > > audition rollingCan
--
module DesigningSound.RollingCan where

import Sound.SC3
import Sound.SC3.ID

import DesigningSound.Util

-- | Four clear resonances are identified using a spectrogram of a can.
--
-- > (
-- > x = {  |t_trig=0|
-- >         // This line just creates a sharp little spike whenever we want:
-- >         var strike = EnvGen.ar(Env.perc(0.0001, 0.001, 0.1), t_trig);
-- >         // here's the resonances:
-- >         var son = Ringz.ar(strike, [359, 426, 1748, 3150], 0.2).sum;
-- >         // some distortion livens up the spectrum a little:
-- >         son = HPF.ar(son.clip2(0.6), 300);
-- >         son * 0.2
-- > }.play;
-- > )
-- > x.set(\t_trig, 1); // Run this line to hit the can!
-- >
fourResonances :: UGen
fourResonances = out 0 $ pan2 (son * 0.2) 0 1
  where
    son = hpf (clip2 son' 0.6) 300
    son' = mix $ ringz strike (mce [359, 426, 1748, 3150]) 0.2
    strike = envGen ar trg 1 0 1 DoNothing (envPerc 1e-4 1e-3)
    trg = kcont "t_trig" 0

-- | Simulates the regular tuning of the drinks can.
--
-- Implementation in haskell is not using bilinear random.
--
-- > ~regularroll = { |rate = 1|
-- >   // In the original code, Andy uses a master phase control,
-- >   // wrapping and re-scaling it before differentiating, to produce
-- >   // a repeating but disorderly set of impulses.
-- >   // Here we do it differently - we use Impulse.kr to generate the
-- >   // impulses directly.
-- >   // We evaluate this function multiple times using .dup so that
-- >   // we get a whole set of impulses with random phase positions.
-- >   {
-- >     Impulse.kr(rate, 1.0.rand, 1.0.bilinrand)
-- >   }.dup(10).sum
-- > };
regularroll :: UGen -> UGen
regularroll rt = sum [f x | x <-[1..10]]
  where
    f :: Int -> UGen
    f x = impulse kr rt (rand (x) 0 1.0) * rand (x+1000) 0 1.0

-- | rolling signature based on Mathias Rath's idea - see 'The
-- Sounding Object' (ajf2009)
--
-- > // This signal contribution to rolling signature based on Mathias
-- > //  Rath's idea - see 'The Sounding Object'
-- > // (ajf2009)
-- > //
-- > ~irregularground = { |rate=10|
-- >         var trigs = Dust.kr(rate);
-- >         EnvGen.ar(
-- >
-- >                 Env([0,0,-1,1.5,-1,1,0], [0.1, 0.1, 0.001, 0.001, 0.1, 0.1], 'sine'),
-- >                 trigs
-- >         ) * 0.1
-- > };
-- > ~irregularground.plot(4)
irregularground :: UGen -> UGen
irregularground rt = envGen ar trg 1 0 1 DoNothing shape * 0.1
  where
    trg = dust 'a' kr rt
    shape = env [0,0,-1,1.5,-1,1,0]
                [0.1, 0.1, 0.001, 0.001, 0.1, 0.1] [EnvSin] 1 1

-- | The sound of a can rolling a little and coming to a stop.
--
-- > x = {
-- >         var rate, strike, son;
-- >         // rate of motion starts fast and tails off
-- >         rate = XLine.kr(4, 0.001, 8, doneAction: 2);
-- >         // This rate affects both the regular rolling, and the irregular ground contacts.
-- >         strike =
-- >                 ~irregularground.(rate*2) * 0.04
-- >                         +
-- >                 K2A.ar(~regularroll.(rate) * 0.1)
-- >                         ;
-- >         // Force the strikes to die off in intensity:
-- >         strike = strike * XLine.ar(1, 0.0001, 8);
-- >         // And here are the tin-can resonances as in fig 31.3:
-- >         son = Ringz.ar(strike, [359, 426, 1748, 3150], 0.2).sum;
-- >         son = HPF.ar(son.clip2(0.6), 300);
-- >         son * 0.2
-- > }.play;
--
rollingCan :: UGen
rollingCan = out 0 $ pan2 (son * 0.2) 0 1
  where
    son = hpf (clip2 son' 0.6) 300
    son' = mix $ ringz strike (mce [359, 426, 1748, 3150]) 0.2
    strike = strike' * xLine ar 1 1e-4 8 RemoveSynth
    strike' = irregularground (rate * 2) * 0.04 +
              regularroll rate * 0.1
    rate = xLine kr 4 1e-3 8 DoNothing
