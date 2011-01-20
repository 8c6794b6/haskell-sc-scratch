{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a pile of oscillators, take 3
--
module Sound.Study.ForAPileOfOscillators.A003 where

import Control.Concurrent (threadDelay)
import Control.Monad (zipWithM_)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Sound.Study.ForAPileOfOscillators.Common

-- | For showing gui
main :: IO ()
main = withSC3 $ \fd -> do
  treeToGui (Group 0 (afpDefault++[smasterNode])) hints fd

setup :: (Transport t) => t -> IO ()
setup fd = do
  async fd $ d_recv $ synthdef "smaster" smaster
  addNode 0 a003Nodes fd

writeA003Score :: FilePath -> IO ()
writeA003Score path = do
  putStrLn "Not yet written"

a003Nodes :: SCNode
a003Nodes =
  Group 0
    [Group 1
       [Group 10 []
       ,Group 11 afp3
       ,Group 12 oscs
       ,Group 13 [smasterNode]]]

smasterNode :: SCNode
smasterNode =
  Synth 1301 "smaster"
    ["amp":=1,"rmix":=0.5,"rroom":=0.5,"rdamp":=0.5]

afp3 :: [SCNode]
afp3 =
  [Synth 1001 "ac1"
    ["del":=0, "vd":=0.5, "vc":=0.05]
  ,Synth 1002 "fc1" []
  ,Synth 1003 "pc1" []]

smaster :: UGen
smaster = out 0 (sig * amp)
  where
    sig = freeVerb2 (in' 1 ar 0) (in' 1 ar 1) rmix rroom rdamp
    amp = ctrl "amp" 1
    rmix = ctrl "rmix" 0.5
    rroom = ctrl "rroom" 0.5
    rdamp = ctrl "rdamp" 0.5

go :: (Transport t) => t -> IO ()
go fd = do -- zipWithM_ f (repeat 1) =<< runPIO p0
  ps <- runPIO p0
  as <- runPIO a0
  sequence_ $ zipWith3 f (repeat 1) as ps
  where
    f t a p = do
      send fd $ Bundle immediately
        [n_set 1001 [("t_trig",a)]
        ,n_set 1002 [("mix", p)]
        ,n_set 1003 [("t_trig", a)]]
      threadDelay $ floor $ 0.22 * 1e6

p0 = pcycle [pseq (pchoose 1 [3,5]) [p1, pchoose 1 [p2,p4]] ,p3]

p1 =
  pseq 6
    [pseq 3
      [plist [1e-3, 0.5, 999e-3, 0.75]]
    ,pchoose 4 [0.5, 0.25, 0.75, 0.125]]

p2 = pchoose 4
       [pseq 1 [1e-3, 0.5, 999e-3, 0.75]
       ,pseq 2 [pchoose 2 [0.125, 0.75]]
       ,pseq 1 [0.75,0.75,999e-3,0.125]
       ,pchoose 4 [1e-3, 0.125, 0.25, 0.5, 0.75, 999e-3]
       ,pseq 1 [0.125, 0.125, 0.25, 0.125]
       ,pseq 1 [0.125, 999e-3, 0.25, 999e-3]
       ,pseq 2 [999e-3, 1e-3]]

p3 = pseq 1 $
     (zipWith (\t v -> plist $ replicate t v))
       [8,8, 6,10, 10,6, 6,32, 2,1,1,1,1]
       [0.125, 1e-3
       ,0.125, 0.25
       ,0.125, 0.25
       ,0.333, 0.25
       ,0.125, 1e-3, 0.125, 0.75, 1e-3]

p4 = plist [0.125, 0.125, 0.25, 0.125
           ,999e-3, 0.25, 999e-3, 0.125
           ,999e-3, 0.25, 999e-3, 0.125
           ,0.25 ,0.125, 0.75, 1e-3]

a0 = pcycle [plist [1, 0, 0, 0
                   ,0, 0, 0, 0]]

v :: (Transport t) => Double -> t -> IO ()
v f fd = send fd $ n_set 1002 [("mix", f)]

p :: (Transport t) => R Double -> t -> IO ()
p p fd = mapM_  g =<< runPIO p
  where g x = v x fd >> threadDelay (floor $ 0.22 * 1e6)