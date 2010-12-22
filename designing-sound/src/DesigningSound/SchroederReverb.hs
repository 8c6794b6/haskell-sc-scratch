------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Schroeder reverb. Corresponding url for this module is:
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Schroeder_reverb>
--
-- /Example/:
--
-- > > :m + Sound.SC3.ID
-- > > audition $ verb1 $ dust2 'a' kr (mce [2.5,1.5]) * sinOsc ar 440 0 * 0.2
-- > > audition $ verb2 $ dust2 'a' kr (mce [2.5,1.5]) * sinOsc ar 440 0 * 0.2
--
module DesigningSound.SchroederReverb where

import Sound.SC3

-- | Create reverb, with four separate delay lines cross-fertilising each other.
--
-- Note the use of @mrg@.
--
-- > Ndef(\verb, {
-- >   var input, output, delrd, sig, deltimes;
-- >
-- >    // Choose which sort of input you want by (un)commenting these lines:
-- >
-- >    // buffer playback, panned halfway left
-- >    input = Pan2.ar(PlayBuf.ar(1, b, loop: 0), -0.5);
-- >    //input = SoundIn.ar([0,1]); // TAKE CARE of feedback - use headphones
-- >    //input = Dust2.ar([0.1, 0.01]); // Occasional clicks
-- >
-- >    // Read our 4-channel delayed signals back from the feedback loop
-- >    delrd = LocalIn.ar(4);
-- >
-- >    // This will be our eventual output, which will also be recirculated
-- >    output = input + delrd[[0,1]];
-- >
-- >    // Cross-fertilise the four delay lines with each other:
-- >    sig = [output[0]+output[1], output[0]-output[1], delrd[2]+delrd[3], delrd[2]-delrd[3]];
-- >    sig = [sig[0]+sig[2], sig[1]+sig[3], sig[0]-sig[2], sig[1]-sig[3]];
-- >    // Attenutate the delayed signals so they decay:
-- >    sig = sig * [0.4, 0.37, 0.333, 0.3];
-- >
-- >    // Here we give delay times in milliseconds, convert to seconds,
-- >    // then compensate with ControlDur for the one-block delay
-- >    // which is always introduced when using the LocalIn/Out fdbk loop
-- >    deltimes = [101, 143, 165, 177] * 0.001 - ControlDur.ir;
-- >
-- >    // Apply the delays and send the signals into the feedback loop
-- >    LocalOut.ar(DelayC.ar(sig, deltimes, deltimes));
-- >
-- >    // Now let's hear it:
-- >    Out.ar(0, output);
-- >
-- > }).play
--
verb1 :: UGen -> UGen
verb1 input = mrg [lo, out 0 output]
  where
    output = input + (mce $ take 2 (mceChannels delrd))
    delrd = localIn 4 ar
    sig0 = mce [ output !!* 0 + output !!* 1
               , output !!* 0 - output !!* 1
               , delrd !!* 2 + delrd !!* 3
               , delrd !!* 2 - delrd !!* 3 ]
    sig1 = mce [ sig0 !!* 0 + sig0 !!* 2
               , sig0 !!* 1 + sig0 !!* 3
               , sig0 !!* 0 - sig0 !!* 2
               , sig0 !!* 1 - sig0 !!* 3 ]
    sig2 = sig1 * mce [0.4, 0.37, 0.333, 0.3]
    deltimes = mce [101, 143, 165, 177] * 0.001 - (1/controlRate)
    lo = localOut $ delayC sig2 deltimes deltimes

-- | Alternative of @verb1@, using matrix instead of summing up each channel.
--
-- > Ndef(\verb, {
-- >   var input, output, delrd, sig, deltimes;
-- >
-- >    // Choose which sort of input you want by (un)commenting these lines:
-- >    // buffer playback, panned halfway left
-- >    input = Pan2.ar(PlayBuf.ar(1, b, loop: 0), -0.5);
-- >    //input = SoundIn.ar([0,1]); // TAKE CARE of feedback - use headphones
-- >    //input = Dust2.ar([0.1, 0.01]); // Occasional clicks
-- >
-- >    // Read our 4-channel delayed signals back from the feedback loop
-- >    delrd = LocalIn.ar(4);
-- >
-- >    // This will be our eventual output, which will also be recirculated
-- >    output = input + delrd[[0,1]];
-- >
-- >    sig = output ++ delrd[[2,3]];
-- >    // Cross-fertilise the four delay lines with each other:
-- >    sig = ([ [1,  1,  1,  1],
-- >             [1, -1,  1, -1],
-- >             [1,  1, -1, -1],
-- >             [1, -1, -1,  1]] * sig).sum;
-- >    // Attenutate the delayed signals so they decay:
-- >    sig = sig * [0.4, 0.37, 0.333, 0.3];
-- >
-- >    // Here we give delay times in milliseconds, convert to seconds,
-- >    // then compensate with ControlDur for the one-block delay
-- >    // which is always introduced when using the LocalIn/Out fdbk loop
-- >    deltimes = [101, 143, 165, 177] * 0.001 - ControlDur.ir;
-- >
-- >    // Apply the delays and send the signals into the feedback loop
-- >    LocalOut.ar(DelayC.ar(sig, deltimes, deltimes));
-- >
-- >    // Now let's hear it:
-- >    Out.ar(0, output);
-- >
-- > }).play
--
verb2 :: UGen -> UGen
verb2 input = mrg [lo, out 0 output]
  where
    output = input + mce [delrd !!* 0, delrd !!* 1]
    delrd = localIn 4 ar
    sig0 = mce [output !!* 0, output !!* 1, delrd !!* 2, delrd !!* 3]
    sig1 = mix $ mceEdit fn sig0
    sig2 = sig1 * mce [0.4, 0.37, 0.333, 0.3]
    fn = zipWith (*) (map mce mtx)
    mtx = [ [1,  1,  1,  1]
          , [1, -1,  1, -1]
          , [1,  1, -1, -1]
          , [1, -1, -1,  1] ]
    deltimes = mce [101, 143, 165, 177] * 0.001 - (1/controlRate)
    lo = localOut $ delayC sig2 deltimes deltimes

-- | Synonym operator of @mceChannel@
(!!*) :: UGen -> Int -> UGen
ug !!* n = mceChannel n ug

infixl 7 !!*
