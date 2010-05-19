------------------------------------------------------------------------------
-- | Translation of ex047/a.ly
--

module Scratch.Ex047a where

import Haskore.Basic.Duration
import Haskore.Melody 

import Sound.OpenSoundControl
import Sound.SC3.Wing.ScheduleSimple
import Scratch.HaskoreScratch

main :: IO ()
main = runSeqOSC id 120 $ noteToSeqEvNew "simplePitched" 1 g1 score

main2 :: IO ()
main2 = runSeqOSC id 120 $ noteToSeqEvGated organ01St g1 score

organ01St :: GateState
organ01St = defaultGateState {gsSynthdefName="gateOrgan01"}

score :: Melody ()
score = para [upper, lower]

upper :: Melody ()
upper = seri l
    where
      l = [
        c 6 qn (), 
        -- 1
        b 5 qn (), a 5 qn (), e 6 dqn (), c 6 sn (), a 5 sn (),
        -- 2
        e 5 dhn (), para [e 5 qn (), a 4 qn ()],
        -- 3 
        para [e 5 qn (), d 5 qn (), gs 4 qn ()], 
        para [e 5 qn (), c 5 qn (), a 4 qn ()],
        para [e 5 qn (), b 4 qn ()],
        para [a 5 qn (), ds 5 qn (), a 4 qn ()],
        -- 4
        para [gs 5 dhn (), ds 5 dhn (), a 4 dhn ()], d 6 qn (),
        
        -- 5 
        c 6 qn (), b 5 qn (), f 6 dqn (), d 6 sn (), b 5 sn (),
        -- 6 
        e 5 dhn (), para [gs 5 qn (), e 5 qn (), b 4 qn ()],
        -- 7 
        para [a 5 qn (), e 5 qn (), a 4 qn ()],
        para [b 5 qn (), e 5 qn (), b 4 qn ()],
        para [c 6 qn (), e 5 qn (), a 4 qn ()],
        para [e 6 qn (), b 5 qn (), e 4 qn ()],
        -- 8 
        para [a 6 wn (), e 6 wn (), a 5 wn ()],
             
        -- 9 
        para [
         seri [a 6 en (), b 6 sn (), a 6 sn (), gs 6 en (), a 6 en (), 
               c 7 qn (), b 6 qn ()],
         para [ds 6 wn (), a 5 wn ()]
        ],
        -- 10
        para [gs 5 en (), gs 4 en (), e 4 en ()]
       ]

lower :: Melody ()
lower = seri [
         c 5 qn (),
         -- 1
         b 4 qn (), a 4 qn (), e 5 dqn (), c 5 sn (), a 4 sn (),
         -- 2
         e 4 dhn (), para [e 4 qn (), c 3 qn ()],
         -- 3 
         para [e 4 qn (), b 3 qn ()],
         para [e 4 qn (), a 3 qn ()],
         para [e 4 qn (), gs 3 qn ()],
         para [e 4 qn (), f 3 qn ()],
         -- 4
         para [e 4 dhn (), e 3 dhn ()], d 4 qn (),
              
         -- 5
         c 5 qn (), b 4 qn (), f 5 dqn (), d 5 sn (), b 4 sn (),
         -- 6 
         e 4 dhn (), para [e 4 qn (), d 4 qn ()],
         -- 7 
         para [e 4 qn (), c 4 qn ()],
         para [e 4 qn (), b 3 qn ()],
         para [e 4 qn (), a 3 qn ()],
         para [e 4 qn (), g 3 qn ()],
         -- 8 
         para [fs 4 wn (), fs 3 wn ()],
         
         -- 9
         para [f 4 wn (), f 3 wn ()],
         -- 10 
         para [e 4 en (), e 3 en ()]
        ]
