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
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Bouncing_ball>
--
-- /Example/:
--
-- > > audition bouncer
--
module DesigningSound.BouncingBall where

import Sound.SC3

-- | Bouncing ball
--
-- > Ndef(\bouncer, {
-- >
-- > var bounceperiod, bouncetrigs, amp, fm, mainosc;
-- >
-- > bounceperiod = Line.kr(0.3, 0, 3, doneAction: 2);
-- >
-- > bouncetrigs = Impulse.kr(bounceperiod.reciprocal.min(30));
-- >
-- > amp = EnvGen.ar(Env.perc(0.001, 0.0), bouncetrigs);
-- > amp = Amplitude.ar(amp, 0, bounceperiod) * Line.kr(1, 0.05, 3);
-- >
-- > fm =
-- >   SinOsc.ar(120).range(0, Line.ar(1, 0, 3))
-- >   +
-- >   (amp * Line.ar(1, 0, 3).cubed * 130 + 80)
-- > ;
-- >
-- > mainosc = SinOsc.ar(fm, pi/2);
-- >
-- > amp * mainosc;
-- > }).play
--
bouncer :: UGen
bouncer = out 0 $ pan2 (amp * mainOsc) 0 1
  where
    amp = amplitude ar amp' 0 bouncePeriod * line kr 1 0.0005 3 RemoveSynth
    amp' = envGen ar bounceTrigs 1 0 1 DoNothing (envPerc 0.001 0.0)
    bounceTrigs = impulse kr (min 30 (recip bouncePeriod)) 1
    bouncePeriod = line kr 0.3 0 3 DoNothing
    mainOsc = sinOsc ar fm (pi/2)
    fm = randRange (sinOsc ar 120 0) (line ar 1 0 3 DoNothing) +
         (amp * cubed (line ar 1 0 3 DoNothing) * 130 + 80)
