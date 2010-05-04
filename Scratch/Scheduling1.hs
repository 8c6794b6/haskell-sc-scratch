------------------------------------------------------------------------------
-- | Playing with scheduling events.

module Scratch.Scheduling1 where

import Control.Applicative
import Control.Arrow
import System.Random
import FRP.Reactive (listE, TimeT, withTimeE)
import FRP.Reactive.Reactive (exactNB)
import FRP.Reactive.Internal.Reactive (runE)

import Sound.OpenSoundControl
import Sound.SC3

type SeqEvent a = [(Double, [a])]

type BPM = Double

runSeqEvent :: Show a => SeqEvent a -> (([a], Double) -> IO ()) -> IO ()
runSeqEvent evnt act = do
  t0 <- utcr
  let evnt' = fmap print (withTimeE . listE . addTime $ evnt)
  runE (pauseThreadUntil . (+t0) . exactNB) evnt'

runSeq :: SeqEvent a -> ([a] -> IO ()) -> IO ()
runSeq [] _ = return ()
runSeq ((et,ev):es) act = do
  act ev
  pauseThread et
  runSeq es act

runSeqOSC :: BPM -> SeqEvent OSC -> IO ()
runSeqOSC _ [] = return ()
runSeqOSC bpm es = do
  t0 <- utcr
  runSeqOSC' t0 0 bpm es

runSeqOSC' :: Double -> Double -> BPM -> SeqEvent OSC -> IO ()
runSeqOSC' _ _ _ [] = return ()
runSeqOSC' t0 t1 bpm ((dt,osc):es) = do
    withSC3 $ \fd -> send fd $ Bundle (UTCr $ t0+t1+ltc) osc
    pauseThread (t1 * 60 / bpm) -- dt'
    runSeqOSC' (t0+t1) dt' bpm es
        where
          dt' = dt * 60 / bpm
          ltc = 0.01

o :: Transport t => ((t -> IO ()) -> IO ()) -> OSC -> IO ()
o w msg = w $ \fd -> send fd msg

-- | Merges two SeqEvent.
merge :: SeqEvent a -> SeqEvent a -> SeqEvent a
merge a [] = a
merge [] b = b
merge ((at,avs):as) ((bt,bvs):bs)
    | at == bt = (at,avs ++ bvs) : merge as bs
    | at > bt = (bt,bvs) : merge ((at-bt, avs):as) bs
    | at < bt = (at,avs) : merge as ((bt-at,bvs):bs)

-- | Infix version of merge.
(//) :: SeqEvent a -> SeqEvent a -> SeqEvent a
a // b = merge a b

se1 :: SeqEvent String
se1 = take 16 $ zip (repeat 1) $ map (:[]) $
      cycle $ words "hello world this is a test for merging events"

se2 :: SeqEvent String
se2 = take 32 $ zip (repeat 0.5) $ cycle [["foo"], ["bar"]]

se3 :: SeqEvent String
se3 = (8, []): (take 32 $ zip (repeat 0.25) $ repeat ["rest"])

addTime :: Num a => [(a, b)] -> [(a, b)]
addTime = scanl1 (\(a1,_) (a2,b2) -> (a1+a2,b2))

-- | SimplePitched, duration in beat and pitch in MIDI Pitch
data SP = SP Double Double

sp :: SP -> (Double, [OSC])
sp (SP dur pit) = (dur, [newSimplePitched pit])
    where
      newSimplePitched :: Double -> OSC
      newSimplePitched p = s_new "simplePitched" (-1) AddToTail 1
                           [("freq", midiCPS p)]

spe01 :: SeqEvent OSC
spe01 = map sp
        [SP 1 72, SP 1 74, SP 1 76, SP 1 77,
         SP 1 72, SP 1 74, SP 2 76]

spe02 :: SeqEvent OSC
spe02 = map sp
        [SP 2 60, SP 2 57,
         SP 2 65, SP 2 60]


-- | Sample SeqEvent OSC.
addDefaultSynths :: SeqEvent OSC
addDefaultSynths =
    [(0.5, [s_new "default" (1000) AddToTail 1 [("freq",midiCPS 60)]]),
     (0.5, [s_new "default" (1001) AddToTail 1 [("freq",midiCPS 64)]]),
     (0.5, [s_new "default" (1002) AddToTail 1 [("freq",midiCPS 67)]]),
     (0.5, [s_new "default" (1003) AddToTail 1 [("freq",midiCPS 72)]]),
     (0.0, [n_set 1000 [("gate",0)],
            n_set 1001 [("gate",0)],
            n_set 1002 [("gate",0)],
            n_set 1003 [("gate",0)]]),
     (0.5, [s_new "default" (1004) AddToTail 1 [("freq",midiCPS 59)]]),
     (0.5, [s_new "default" (1005) AddToTail 1 [("freq",midiCPS 62)]]),
     (0.5, [s_new "default" (1006) AddToTail 1 [("freq",midiCPS 67)]]),
     (0.5, [s_new "default" (1007) AddToTail 1 [("freq",midiCPS 74)]]),
     (0.0, [n_set 1004 [("gate",0)],
            n_set 1005 [("gate",0)],
            n_set 1006 [("gate",0)],
            n_set 1007 [("gate",0)]])
    ]
