------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Sample SCNode used in repl code.
--
module SampleData where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

setup :: IO ()
setup = do
  mapM (uncurry writeSynthdef) defs
  withSC3 $ \fd -> do
    now <- utcr
    send fd $ bundle (UTCr now) $ map (d_recv . uncurry synthdef) defs
    wait fd $ "/done"
    reset fd
    addNode 0 t1 fd
  
defs :: [(String,UGen)]
defs =     
  [("foo",foo), ("bar", bar), ("buzz",buzz),("quux",quux),("hoge",hoge),
   ("lfsin",lfsin), ("lftri",lftri), ("lfnz",lfnz),("lftrig",lftrig),
   ("pippo",pippo)
  ]
  
-- Sample SCNode data for playing interactively
t1 :: SCNode
t1 = 
  Group 0
    [Group 1
      [Group 10
        [Group 100
         [Synth 1001 "lfnz" ["out":=100,"mul":=1500,"add":=1500,"freq":=0.125]
         ,Synth 1002 "lftri" ["out":=101,"mul":=1760,"add":=1.8,"freq":=0.5]]
        ,Synth 101 "foo" ["amp":=0.1,"freq":<-100]
        ,Synth 102 "foo" ["amp":=0.1,"freq":<-101]]
      ,Group 11
        [Group 110
         [Synth 1101 "lfsin" ["out":=110,"mul":=0.1,"add":=0.1,"freq":=0.25]
         ,Synth 1102 "lftri" ["out":=111,"mul":=0.1,"add":=0.1,"freq":=0.3]
         ,Synth 1103 "lfnz" ["out":=112,"mul":=0.4,"add":=0.4,"freq":=0.4]]
        ,Synth 111 "bar" ["amp":=0.2,"freq":=4399.8]
        ,Synth 112 "bar" ["amp":<-112,"freq":=6601]]
      ,Group 12
        [Group 120
         [Synth 1201 "lfsin" ["out":=121,"freq":=0.0232]
         ,Synth 1202 "lftri" ["out":=122,"freq":=0.0899]
         ,Synth 1203 "lfnz" ["out":=123,"freq":=0.0713]
         ,Synth 1204 "lfsin" ["out":=124,"freq":=0.1203]
         ,Synth 1205 "lfnz" ["out":=125,"freq":=0.0983]]
        ,Synth 121 "buzz" ["amp":=0.07,"freq":=440,"pan":<-121]
        ,Synth 122 "buzz" ["amp":=0.07,"freq":=554.365,"pan":<-122]
        ,Synth 123 "buzz" ["amp":=0.07,"freq":=660,"pan":<-123]         
        ,Synth 124 "buzz" ["amp":=0.07,"freq":=880,"pan":<-124]         
        ,Synth 125 "buzz" ["amp":=0.07,"freq":=1110,"pan":<-125]]
      ,Group 13
        [Synth 131 "quux" ["amp":=0.4,"freq":=9327,"pan":=0.7]
        ,Synth 132 "quux" ["amp":=0.3,"freq":=3422,"pan":=0.08]
        ,Synth 133 "quux" ["amp":=0.4,"freq":=121,"pan":=(-0.12)]
        ,Synth 134 "quux" ["amp":=0.2,"freq":=1893,"pan":=(-0.6)]]
      ,Group 14
        [Group 141 
          [Synth 1410 "lftrig" ["freq":=1,"out":=141]
          ,Synth 1411 "lftrig" ["freq":=2,"out":=142]]
        ,Synth 1401 "hoge" ["t_trig":<-141]
        ,Synth 1402 "hoge" ["t_trig":<-142,"flo":=880,"fhi":=1320]
        ,Synth 1402 "hoge" ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

foo :: UGen
foo = out ("out"@@0) (pan2 sig 0.2 1) where
  sig = foldr (\_ ug -> combC ug 0.25 0.183872 0.6) sig0 [1..4]
  sig0 = sinOsc ar ("freq"@@440) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $
      envPerc 0.01 0.3
  t = impulse kr 1 0 + dust 'f' kr 0.75

bar :: UGen
bar = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = saw ar ("freq"@@330) * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envPerc 0.01 0.8
  t = dust 'a' kr 2

buzz :: UGen
buzz = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = lfPar ar ("freq"@@880) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envSine 0.8 2.2
  t = dust 'a' kr (1/3)

quux :: UGen
quux = out ("out"@@0) (pan2 sig ("pan"@@(-0.7)) 1) where
  sig = lfCub ar ("freq"@@660) 0 * ("amp"@@0.2) * e
  e = envGen kr t 1 0 1 DoNothing $ 
      envPerc 0.001 0.04
  t = dust 'b' kr 1.75 + impulse kr 0.5 0.5
  
hoge :: UGen  
hoge = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  -- sig = hit
  sig = foldr (\_ ug -> combC ug 0.25 0.232382 0.82) hit [1..4]
  hit = lfCub ar freq 0 * ("amp"@@0.2) * e
  freq = select (tiRand 'i' 0 3 t) (mce $ map midiCPS [62, 69, 71, 76])
  e = envGen kr t 1 0 1 DoNothing $
      env [0,1,1,0] [d0,d1,d2]
      (repeat $ EnvCub) (-1) (-1)
  d0 = tRand '1' 1e-3 0.5 t
  d1 = tRand '2' 1e-3 0.5 t 
  d2 = tRand '3' 1e-3 0.5 t
  t = coinGate 'c' ("prob"@@0.5) ("t_trig"@@1)
  
pippo :: UGen
pippo = replaceOut ("out"@@0) sig where
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

