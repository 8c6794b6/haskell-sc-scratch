{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable
-}
module Ce22e9e.Synths where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

cefoo :: UGen
cefoo = out ("out"@@0) (pan2 sig 0.2 1) where
  sig = foldr (\_ ug -> combC ug 0.25 0.183872 0.6) sig0 [1..4]
  sig0 = sinOsc ar ("freq"@@440) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $
      envPerc 0.01 0.3
  t = impulse kr 1 0 + dust 'f' kr 0.75

cebar :: UGen
cebar = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = saw ar ("freq"@@330) * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envPerc 0.01 0.8
  t = dust 'a' kr 2

cebuzz :: UGen
cebuzz = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = lfPar ar ("freq"@@880) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envSine 0.8 2.2
  t = dust 'a' kr (1/3)

cequux :: UGen
cequux = out ("out"@@0) (pan2 sig ("pan"@@(-0.7)) 1) where
  sig = lfCub ar ("freq"@@660) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envPerc 0.001 0.04
  t = dust 'b' kr 1.75 + impulse kr 0.5 0.5
  
cehoge :: UGen  
cehoge = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = foldr (\_ ug -> combC ug 0.25 0.232382 0.82) hit [1..4]
  hit = lfCub ar freq 0 * ("amp"@@0.2) * e
  freq = select (tiRand 'i' 0 3 t) (mce $ map midiCPS [62, 69, 71, 76])
  e = envGen kr t 1 0 1 DoNothing $
      env [0,1,1,0] [d0,d1,d2]
      (repeat $ EnvCub) (-1) (-1)
  d0 = tRand '1' 1e-3 0.5 t
  d1 = tRand '2' 1e-3 0.5 t 
  d2 = tRand '3' 1e-3 0.5 t
  t = coinGate 'c' ("prob"@@0.5) ("t_trig"@@100)
  
cepippo :: UGen
cepippo = replaceOut ("out"@@0) sig where
  sig = foldr (\_ u -> combC u 0.25 0.232187 1.37) ("a_in"@@0) [1..6]
  
lfsin :: UGen  
lfsin = out ("out"@@100) $ sinOsc kr ("freq"@@1) 0 * ("mul"@@1) + ("add"@@0)

lftri :: UGen
lftri = out ("out"@@100) sig where
  sig = lfTri kr ("freq"@@1) 0 * ("mul"@@1) + ("add"@@0)
  
lfnz :: UGen  
lfnz = out ("out"@@100) sig where
  sig = lfdNoise3 'd' kr ("freq"@@1) * ("mul"@@1) + ("add"@@0)

lftrig :: UGen
lftrig = out ("out"@@100) $ impulse kr ("freq"@@1) ("phase"@@0)

lfdust :: UGen
lfdust = out ("out"@@100) $ dust 'l' kr ("freq"@@1)

