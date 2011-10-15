module Main where

import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (setup) -- -- hiding (s)

go :: Transport t => t -> IO ()
go = patchNode n0

setup fd = do
  f "basd" $ basd ("amp"@@0) ("freq"@@0) ("t_trig"@@0)
  f "basdt" $ basdt ("t_trig"@@0)
  f "basdmetro" $ basdmetro ("bpm"@@100)
  f "basdm" basdm
  where
    f n u = writeSynthdef n u >>
            send fd (d_recv $ synthdef n u)

g = Group
s = Synth
w = withSC3

n0 :: SCNode
n0 =
  g 0
  [g 1
   [g 10
    [s 1000 "basdmetro"
     ["out":=100, "bpm":=230]]
   ,g 20
    [s 2000 "basdt"
     ["t_trig":<-100, "out":=101]
    ,s 2001 "basd"
     ["out":=9, "amp":=0.3, "freq":<-101, "t_trig":<-100]
    ,s 2002 "basdm"
     ["out":=0, "a_in":<=9, "pan":=0.3]]
   ,g 90
    []]]

basdmetro :: UGen -> UGen
basdmetro bpm =
  out ("out"@@0) $ impulse kr (bpm/60) 0

basdt :: UGen -> UGen
basdt t_trig = out ("out"@@0) $ midiCPS $ demand t_trig 0 d where
  d = evalSupply p (mkStdGen 0)
  p = sseq sinf
      [62,64,65,65, 65,65,65,65
      ,62]

basd :: UGen -> UGen -> UGen -> UGen
basd amp freq t_trig =
  out ("out"@@0) $ sinOsc ar freq 0 *
  amp *
  (envGen kr t_trig 1 0 1 DoNothing $
   env [0,1,0] [1e-4,999e-3] [EnvCub] (-1) (-1))

basdm :: UGen
basdm = out ("out"@@0) $ f ("a_in"@@0) where
  f x = pan2 x ("pan"@@0) 1
