------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Boing
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Boing>
--
-- /Example/:
--
-- > > audition $ out 0 $ clampedMode 100 (impuse ar 10 1)
-- > > audition $ out 0 playRulerWave
-- > > audition $ out 0 playSomethingElse
--
-- /TODO/:
--
-- * Write series of wavetable shape creating functions. This shape is not
--   using sine curve for it's segments. Also make the implementation efficient
--   than merely using list of UGen.
--
module DesigningSound.Boing where

import Sound.SC3

-- | Clamed-mode vibration
--
-- > (
-- > ~clampedmodes = { |basefreq, env|
-- >
-- >         var freqs, amps;
-- >
-- >         freqs = [1, 6.267, 17.55, 34.39];
-- >         amps  = [0.5, 0.25, 0.125, 0.06125];
-- >
-- >         Klank.ar(`[freqs, amps, 0.2], env, basefreq);
-- > };
-- > )
clampedMode :: UGen -- ^ base frequency
            -> UGen -- ^ input
            -> UGen
clampedMode baseFreq envelope = klank envelope baseFreq 0 1 spec
  where
    spec = klankSpec
           [1, 6.267, 17.55, 34.39]
           [0.5, 0.25, 0.125, 0.06125]
           (replicate 6 0.2)

-- | Free-mode vibration
--
-- > ~freemodes = { |input, basefreq=100, res=80|
-- >   var filtfreqs;
-- >
-- >   // The actual filter freqs take these harmonic relationships:
-- >   filtfreqs = basefreq * [1, 2.7565, 5.40392, 8.93295, 13.3443, 18.6379];
-- >
-- >   BPF.ar(input, filtfreqs, 1/res).sum * 10
-- > };
--
freeMode :: UGen -- ^ input
         -> UGen -- ^ base frequency
         -> UGen -- ^ resonance
         -> UGen
freeMode input baseFreq res = mix (bpf input filtFreqs (1/res)) * 10
  where
    filtFreqs = mce $ map (* baseFreq)
                [1, 2.7565, 5.40392, 8.93295, 13.3443, 18.6379]

-- | Wave table shape for ruler sound.
--
-- >  ~rulerwave = Env([1, 0, -0.7, 0, 1],
-- >                   [0.3, 0.1, 0.1, 0.3],
-- >                   [4, -4, 4, -4]).asSignal(512).asWavetable;
--
-- This function returns a list of Double willed with the shape.
-- Currently, the shape is using liner curve only.....
--
rulerWave :: [UGen]
rulerWave = 1 : seg1 ++ seg2 ++ seg3 ++ seg4
  where
    seg1 = [f x | x <- [1..pat*3-1]
                , let f a = 1 + a * (sin ((pi/2) * (0-1) / (pat*3)))]
    seg2 = [f x | x <- [pat*3..pat*4-1]
                , let f a = 0 + (a - pat*3) * (sin ((pi/2) * (-0.7)-0) / pat)]
    seg3 = [f x | x <- [pat*4..pat*5-1]
                , let f a = (-0.7) + (a - pat*4) * (sin ((pi/2) * (0-(-0.7)) / pat))]
    seg4 = [f x | x <- [pat*5..pat*8-1]
                , let f a = 0 + (a - pat*5) * (sin ((pi/2) * (1-0) / (pat*3)))]
    pat = 2048 / 8

    -- seg1 = [f x | x <- [1..pat*3-1]
    --             , let f a = 1 + a * ((0-1) / (pat*3))]
    -- seg2 = [f x | x <- [pat*3..pat*4-1]
    --             , let f a = 0 + (a - pat*3) * (((-0.7)-0) / pat)]
    -- seg3 = [f x | x <- [pat*4..pat*5-1]
    --             , let f a = (-0.7) + (a - pat*4) * ((0-(-0.7)) / pat)]
    -- seg4 = [f x | x <- [pat*5..pat*8-1]
    --             , let f a = 0 + (a - pat*5) * ((1-0) / (pat*3))]

-- | Playing the rulerwave.
--
-- > {
-- >    var motion, thwacks, isDown, basefreq;
-- >    motion = Osc.ar(~rulerwave.as(LocalBuf), XLine.kr(10, 100, 1), mul: Line.kr(1, 0.001, 1, doneAction: 2));
-- >    isDown = motion < 0;
-- >    thwacks = Trig1.ar(isDown, 0) * (0-Slope.ar(motion)) * 0.01;
-- >    thwacks = LPF.ar(thwacks, 500);
-- >
-- >    basefreq = if(isDown, 289, 111);
-- >    ~freemodes.value(thwacks, basefreq, 100)
-- >      +
-- >    ~clampedmodes.value(basefreq, thwacks);
-- > }.play
--
playRulerWave :: UGen
playRulerWave = freeMode thwacks baseFreq 100 + clampedMode baseFreq thwacks
  where
    baseFreq = 111 + (isDown * 178)
    isDown = motion <* 0
    motion = osc ar (asLocalBuf 'a' rulerWave) (xLine kr 10 100 1 DoNothing) 0 *
             line kr 1 0.001 1 RemoveSynth
    thwacks = trig1 isDown 0 * (0 - slope motion) * 0.01

-- | Something ... else...
--
-- > {
-- >   var motion, thwacks, isDown, basefreq;
-- >   motion = Osc.ar(~rulerwave.as(LocalBuf), 80, mul: Line.kr(1, 0.001, 1, doneAction: 2));
-- >   isDown = motion < 0;
-- >   thwacks = Trig1.ar(isDown, 0) * (0-Slope.ar(motion)) * 0.01;
-- >
-- >   basefreq = if(isDown, 289, 111) * Pulse.ar(10).exprange(0.9, 1.1);
-- >   ~freemodes.value(thwacks, basefreq, 100)
-- >     +
-- >   ~clampedmodes.value(basefreq, thwacks);
-- > }.play
--
playSomethingElse :: UGen
playSomethingElse = freeMode thwacks baseFreq 100 +
                    clampedMode baseFreq thwacks
  where
    thwacks = trig1 isDown 0 * (0 - slope motion) * 0.01
    baseFreq = 111 + (isDown * 178) * linExp (pulse ar 10 1) 1e-8 1 0.9 1.1
    isDown = motion <* 0
    motion = osc ar (asLocalBuf 'a' rulerWave) 40 0 *
             line kr 1 0.001 1 RemoveSynth