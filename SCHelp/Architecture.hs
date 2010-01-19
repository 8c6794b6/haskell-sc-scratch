------------------------------------------------------------------------------
-- | Scratch written while reading help files in Architecture section.
--

module SCHelp.Architecture where

import Reusable
import Instances
import SCTree
import SCQuery
import SCSched
import qualified Scratch.ControlArgs as Arg

import Sound.SC3
import Sound.OpenSoundControl
import FRP.Reactive

import Control.Arrow
import Control.Applicative
import System.Random

mceEx01 :: IO ()
mceEx01 = audition $ out 0 o
    where
      o = rlpf (saw ar (mce [100,250]) * 0.05)
          (xLine kr 8000 400 5 DoNothing) 0.05

mceEx02 :: IO ()
mceEx02 = audition $ out 0 o
    where
      o = rlpf (saw ar (mce [100,250]) * 0.05)
          (xLine kr (mce [8000,500]) (mce [400,7000]) 5 DoNothing)
          0.05

klankEx01 :: IO ()
klankEx01 = do
  n <- pinkNoise ar >>. (* 0.1)
  let ks = klankSpec [400,500,600] [1,2,1] [1,1,1]
      k = klank n 1 0 1 ks
  audition $ out 0 k

klankEx02 :: IO UGen
klankEx02 = do
  let u = [1,1,1,1]
      p = [200,171,153,172]
      q = [930,971,953,1323]
      r = [8900,16062,9013,7892]
      k = mce [klankSpec p u u, klankSpec q u u, klankSpec r u u]
      s = mceTranspose k
      i = mce [2,2.07,2,13]
      t = impulse ar i 0 * 0.1
  return $ out 0 $ mix $ klank t 1 0 1 s

mixEx01 :: UGen
mixEx01 = out 0 $ mix $ pulse ar (mce [400,501,600]) (mce [0.5,0.1]) * 0.1

mixEx02 :: IO UGen
mixEx02 = do
  let n = 8
  gen <- newStdGen
  let lens = mce $ take n $ randomRs (0.0003,0.0043) gen
      pans = mce $ take n $ randomRs (-1.0,1.0) gen
  ds <- clone n $ dust ar 1 >>. (* 0.3)
  let o = out 0 $ mix $ pan2 (combL ds 0.01 lens 4) pans 1
  return o

help_multichannel :: UGen
help_multichannel =
    out out' $ sinOsc ar ((lfPar kr mod 0 * modrange) + f) 0 *
    envGen kr 1 1 0 1 RemoveSynth (envSine 1 1)
    where
      out' = "out" @= 0
      f = "freq" @= 440
      mod = "mod" @= 0.1
      modrange = "modrange" @= 20

mceRoutine01 :: IO (Event OSC)
mceRoutine01 = do
  gen <- newStdGen
  let n = 8
      freqs = take n $ randomRs (400,5000) gen
      mods = take n $ randomRs (0.1,2) gen
      modranges = take n $ randomRs (0.1,40) gen
      times = scanl (+) 0 $ repeat 0.3
      g f m mr = s_new "help_multichannel" (-1) AddToTail 1
                 [("freq",f),("mod",m),("modrange",mr)]
      oscs = zipWith3 g freqs mods modranges
  return $ listE $ zip times oscs

blip :: UGen
blip = out 0 $ line ar 0.1 0 0.05 RemoveSynth * pulse ar (freq * mce [1,1.02]) 0
    where freq = "freq" @= 440

sigBus :: Num a => a
sigBus = 101

lfoBus :: Num a => a
lfoBus = 102

order_of_ex_dist :: UGen
order_of_ex_dist = replaceOut Arg.bus (sig * Arg.postGain)
    where sig = (in' 2 ar sigBus) * Arg.preGain @> distort

order_of_ex_pulse :: UGen
order_of_ex_pulse =
    out Arg.bus $ pan2 sig Arg.pan 1 *
    envGen kr 1 1 0 noteLen RemoveSynth (envPerc 0.1 1)
    where
      sig = rlpf (pulse ar Arg.freq 0.2 * 0.5) Arg.ffreq 0.3
      noteLen = in' 1 kr Arg.lfoBus

order_of_ex_lfnoise1 :: IO UGen
order_of_ex_lfnoise1 = do
    n <- lfNoise1 kr Arg.freq >>. (* Arg.mul) >>. (+ Arg.add)
    return $ out Arg.bus n

order_of_ex_out :: UGen
order_of_ex_out = out 0 $ (in' 2 ar Arg.in') * 0.25

updateOrderOfExUGens :: IO OSC
updateOrderOfExUGens = withSC3 $ \fd -> do
                         loadSynthdef "order_of_ex_dist" order_of_ex_dist fd
                         loadSynthdef "order_of_ex_pulse" order_of_ex_pulse fd
                         ooel <- order_of_ex_lfnoise1
                         loadSynthdef "order_of_ex_lfnoise1" ooel fd
                         loadSynthdef "order_of_ex_out" order_of_ex_out fd

order_of_ex_tree :: SCTree
order_of_ex_tree =
    Group 0
    [Group 1
     [Group 2
      [Synth (-1) "order_of_ex_lfnoise1"
       ["freq":=0.3, "mul":=1.68, "add":=1.7, "bus":=lfoBus]],
      Group 3
      [Synth (-1) "order_of_ex_dist"
       ["bus":= sigBus, "preGain":=8, "postGain":=0.6]],
      Group 4
      [Synth (-1) "order_of_ex_out"
       ["out":= 0, "in":=sigBus]]]]

order_of_ex_events :: IO (Event OSC)
order_of_ex_events = do
  gen <- newStdGen
  let freqs = randomRs (200,800) gen
      ffreqs = randomRs (1000,15000) gen
      pans = randomRs (-1.0,1.0) gen
      g f ff p = s_new "order_of_ex_pulse" (-1) AddToTail 2
                 [("freq",f),("ffreq",ff),("pan",p),
                  ("bus",sigBus),("lfobus",lfoBus)]
      zl = ZipList
      oscs = getZipList $ g <$> zl freqs <*> zl ffreqs <*> zl pans
      times = scanl (+) 0 $ repeat 0.7
  return $ listE $ zip times oscs

help_Infreq :: UGen
help_Infreq = out 0 $ sinOsc ar (in' 1 kr Arg.bus) 0 * 0.2

help_Outfreq :: UGen
help_Outfreq = out Arg.bus $ sinOsc kr (Arg.freq/40) 0 * Arg.freq

updateHelpInKrUGens :: IO OSC
updateHelpInKrUGens =
    withSC3 $ \fd -> do
      loadSynthdef "help_Infreq" help_Infreq fd
      loadSynthdef "help_Outfreq" help_Outfreq fd

help_Inkr_bus = 100
help_Inkr_bus_control = 101

help_Inkr_tree :: SCTree
help_Inkr_tree =
    Group 0
    [Group 1
     [Synth 1000 "help_Outfreq" -- z
      ["bus" := help_Inkr_bus, "freq" := 800],
      Synth 1001 "help_Infreq"  -- y
      ["bus" := help_Inkr_bus],
      Synth 1002 "help_Outfreq" -- x
      ["bus" := help_Inkr_bus, "freq" := 400]]]


