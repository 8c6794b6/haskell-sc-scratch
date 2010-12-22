------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Bubbles
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Bubbles>
--
-- /Example/:
--
-- > > drecv "bubbleTrigs" bubbleTrigs
-- > > drecv "bubbleBub" bubbleBub
-- > > addBubbles 10
--
module DesigningSound.Bubbles where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import System.Random

import DesigningSound.Util

-- | A synthdef that output triggers but not sound
--
-- > SynthDef(\bubbletrigs, {|out=0, probability=0.5|
-- >   var trigs, buf, a;
-- >    // These two lines create a loop of zeroes
-- >    // with some ones (i.e. triggers) placed at prime-number locations
-- >    a = {0}.dup(200);
-- >    [29, 37, 47, 67, 89, 113, 157, 197].do{|val| a[val] = 1};
-- >    buf = a.as(LocalBuf);
-- >    // playbuf by default will use the server's rate, but we want one item every 15ms
-- >    trigs = PlayBuf.kr(1, buf, 0.015.reciprocal / (s.sampleRate / s.options.blockSize), loop: 1);
-- >    // Randomly discard half of them, to remove too much obvious looping
-- >    trigs = CoinGate.kr(probability, trigs);
-- >    // Let's poll to watch the events appearing
-- >    trigs.poll(trigs);
-- >    Out.kr(out, trigs);
-- > }).store
--
bubbleTrigs :: UGen
bubbleTrigs = out outBus trigs
  where
    outBus = control kr "out" 0
    probability = control kr "probability" 0.5
    trigs = coinGate 'a' probability trigs'
    trigs' = playBuf 1 buf (recip 0.015 / (sampleRate / blockSize))
             0 0 Loop DoNothing
    blockSize = 64
    buf = asLocalBuf 'b' a
    a = [if x `elem` [29,37,47,67,89,113,157,197::Integer] then 1 else 0
         | x <- [0..255]]

-- | Sound of a bubble
--
-- > SynthDef(\bubblebub, { |out=0, t_trig=0, attack=0.01, decay=0.08, pitchcurvelen=0.1, freq=1000, doneAction=0, amp=0.1|
-- >   var pitch, son;
-- >   amp   = amp * EnvGen.ar(Env.perc(attack, decay).delay(0.003), t_trig, doneAction: doneAction);
-- >   pitch = freq * EnvGen.ar(Env.new([0,0,1],[0,1]).exprange(1, 2.718), t_trig, timeScale: pitchcurvelen);
-- >   son = SinOsc.ar(pitch);
-- >   // high-pass to remove any lowpitched artifacts, scale amplitude
-- >   son = HPF.ar(son, 500) * amp * 10;
-- >   Out.ar(out, son);
-- > }).store
bubbleBub :: UGen
bubbleBub = out outBus son
  where
    outBus = kcont "out" 0
    t_trig = kcont "t_trig" 0
    attack = kcont "attack" 0.01
    dcy = kcont "decay" 0.08
    pitchcurvelen = kcont "pitchcurvelen" 0.1
    freq = kcont "freq" 1000
    doneAction = kcont "doneAction" 1
    amp = kcont "amp" 0.1

    son = hpf son' 500 * amp' * 10
    son' = sinOsc ar ptc 0
    ptc = freq * envGen ar t_trig 1 0 pitchcurvelen doneAction' pShape
    amp' = amp * envGen ar t_trig 1 0 1 doneAction' aShape
    aShape = envPerc attack dcy
    pShape = env [0,0,1] [0,1] [EnvCub] (-1) 0
    doneAction' = (DoneAction doneAction) -- get this value from control

-- | Four bubble system, simply triggerd at random
--
-- > s.bind{
-- >    // Here we'll create busses to hold the triggers, passing them from synth to synth
-- >    ~maintrigbus = Bus.control(s, 1);
-- >    ~bubtrigbus = Bus.control(s, 4);
-- >    // Note how we make sure things are running in the desired order, using \addAfter
-- >    ~trigs = Synth(\bubbletrigs, [\out: ~maintrigbus]);
-- >    // This reads the main trig and puts each trig on a randomly-chosen bus
-- >    ~randomdistrib = {
-- >       var trig, which;
-- >        trig = In.kr(~maintrigbus);
-- >        which = TIRand.kr(0,3, trig);
-- >        // or try the Stepper instead of TIRand for "round-robin" selection:
-- >        // which = Stepper.kr(trig, 0, 0, 3);
-- >        which = which.poll(trig);
-- >        Out.kr(~bubtrigbus.index + which, trig);
-- >    }.play(target: ~trigs, addAction: \addAfter);
-- >
-- >    s.sync;
-- >
-- >    ~bubs = [2400, 2600, 2500, 2700].collect{|afreq|
-- >       Synth(\bubblebub, [\freq, afreq], target: ~randomdistrib, addAction: \addAfter);
-- >    };
-- >
-- >    s.sync;
-- >
-- >    // "map" allows us to push the triggers from the control bus to the "t_trig" inputs:
-- >    ~bubs.do{|bub, bubindex| bub.map(\t_trig, ~bubtrigbus.index + bubindex) };
-- > };
--
fourBubbles :: IO ()
fourBubbles = do
  let mainTrigBusId = 101
      bubTrigBusId = 102
      bubbleIds = [1001..1004]
  drecv "bubbleTrigs" bubbleTrigs
  drecv "bubbleBub" bubbleBub
  _ <- snew "bubbleTrigs" [("out", fromIntegral mainTrigBusId)]
  randDistrib <- anon $ let trigs = in' 1 kr mainTrigBusId
                            which = tiRand 'a' 0 3 trigs
                        in  out (bubTrigBusId + which) trigs
  let mkBubble (i,f) = s_new "bubbleBub" i AddAfter randDistrib [("freq", f)]
      mapBubble (i,j) = n_map i [("t_trig", fromIntegral bubTrigBusId+j)]

  mapM_ (\f -> withSC3 $ \fd -> send fd $ mkBubble f)
    (zip bubbleIds [2400, 2600, 2500, 2700])
  mapM_ (\(i,j) -> withSC3 $ \fd -> send fd $ mapBubble (i,j))
    (zip bubbleIds [0..3])

-- | Add bubbles, with using dust ugen as triggers.
addBubbles :: Int -> IO ()
addBubbles numBubbles = withSC3 $ \fd -> do
  let buses = [101..]
      trigUg bus = out (fromIntegral bus) (dust 'a' kr 1)
  freqs <- randomRs (400,3000) `fmap` newStdGen
  mapM_ (anonymous fd) $ take numBubbles $ map trigUg buses
  bubbleNodes <- mapM (\f -> s_new_id fd "bubbleBub" AddToTail 1 [("freq", f)])
                 (take numBubbles $ freqs)
  mapM_ (\(b,t) -> send fd $ n_map b [("t_trig", t)]) (zip bubbleNodes buses)
