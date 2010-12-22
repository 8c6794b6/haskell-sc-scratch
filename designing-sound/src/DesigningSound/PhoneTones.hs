------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Phone tone sounds. Corresponding url for this module is:
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Phone_tones>
--
-- /Example/:
--
-- > > withSC3 reset
-- > > audition $ dialtone
-- > > audition $ transmed source1
-- > > audition $ transmed source2
-- > > audition $ transmed source3
-- > > n <- source4
-- > > hitSource4 n
--
module DesigningSound.PhoneTones where

import Sound.SC3
import Sound.OpenSoundControl
import System.Random (randomRIO)

-- | Simple straight forward dial tone sound.
--
-- > Ndef(\dialtone, {
-- >   // Note: the array here specifies two frequencies, so we get two
-- >   // separate channels.  We sum the two channels so they combine
-- >   // into one signal - otherwise we  would hear one note on left,
-- >   // one note on right.
-- >   Pan2.ar(SinOsc.ar([350, 440], 0, 0.2).sum)
-- > }).play
--
dialtone :: UGen
dialtone = out 0 $ pan2 (mix $ sinOsc ar (mce [350, 440]) 0 * 0.2) 0 1

-- | Filters arg sound.
--
-- > Ndef(\transmed, {
-- >         var sig = Ndef(\phonesource).ar.clip2(0.9);
-- >         sig = BPF.ar(sig, 2000, 1/12);
-- >         sig =
-- >                 BPF.ar(sig * 0.5, 400, 1/3)
-- >                 +
-- >                 (sig.clip2(0.4) * 0.15);
-- >         HPF.ar(HPF.ar(sig, 90), 90) * 100;
-- > }).play
--
transmed :: UGen -> UGen
transmed src = out 0 $ pan2 (hpf (hpf sig 90) 90 * 100) 0 1
  where
    sig = bpf (sig' * 0.5) 400 (1/3) + clip2 sig' 0.4 * 0.15
    sig' = bpf sig'' 2000 (1/12)
    sig'' = clip2 src 0.9

-- | Control dialtone with mouseX
--
-- > Ndef(\phonesource, {
-- >         var onoff;
-- >         onoff = if(MouseX.kr > 0.2, 1, 0);
-- >         SinOsc.ar([350, 440], 0, onoff).sum * 0.2
-- > })
--
source1 :: UGen
source1 = mix (sinOsc ar (mce [330, 440]) 0 * onoff) * 0.2
  where onoff = mouseX kr 1 0 Linear 0.1 >* 0.5

-- | Control dialtone with lfPulse
--
-- > Ndef(\phonesource, {
-- >         var onoff;
-- >         onoff = LFPulse.ar(1/6, width: 1/3);
-- >         SinOsc.ar([480, 440], 0, onoff).sum * 0.2
-- > })
--
source2 :: UGen
source2 = mix (sinOsc ar (mce [480, 440]) 0 * onoff) * 0.2
  where onoff = lfPulse ar (1/6) 0 (1/3)

-- | Busy tone.
--
-- > Ndef(\phonesource, {
-- >         var onoff;
-- >         onoff = LPF.ar(LFPulse.ar(2), 100);
-- >         SinOsc.ar([480, 620], 0, onoff).sum * 0.2
-- > })
--
source3 :: UGen
source3 = mix (sinOsc ar (mce [480,620]) 0 * onoff) * 0.2
  where onoff = lpf (lfPulse ar 2 0 0.5) 100

-- | Pulse dialling, before DTMF
--
-- First:
--
-- > Ndef(\phonesource, { |t_trig=0, number=0|
-- >         var onoff, trigs, son;
-- >         // zero is represented by 10 clicks!
-- >         number = if(number < 0.5, 10, number);
-- >         onoff = Trig1.ar(t_trig, number * 0.1);
-- >         trigs = Impulse.ar(10) * onoff;
-- >         son = Trig1.ar(trigs, 0.04);
-- >         son;
-- > });
--
-- Then:
--
-- > Ndef(\phonesource).set(\t_trig, 1, \number, 10.rand.postln);
--
source4 :: IO Int
source4 = withSC3 $ \fd -> do
  send fd . d_recv $ synthdef "source4" $ transmed mkSource4
  _ <- wait fd "/done"
  send fd $ s_new "source4" (-1) AddToTail 1 []
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Reset trigger, with specifying node id.
hitSource4 :: Int -> IO ()
hitSource4 n = withSC3 $ \t -> do
  times <- (\x -> if x < 1 then 10 else x) `fmap` randomRIO (0,10)
  send t $ n_set n [("t_trig", 1), ("number", times)]

-- | Helper function for source4
mkSource4 :: UGen
mkSource4 = son
  where
    n = control kr "number" 10
    onoff = trig1 (tr_control "t_trig" 0) (n * 0.1)
    trigs = impulse ar 10 0 * onoff
    son = trig1 trigs 0.04
