{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | Playing with Reactive.
--

module FRPScratch where

import Control.Applicative
import Control.Concurrent (forkIO,killThread)
import Data.Monoid
import Data.List (sort)
import System.Random hiding (next)

import Data.VectorSpace

import FRP.Reactive
import FRP.Reactive.Reactive (exactNB)
import FRP.Reactive.LegacyAdapters
import FRP.Reactive.Internal.Reactive (runE,forkR)
import Sound.OpenSoundControl
import Sound.SC3 hiding (stepper)

type BellMachine = Event () -> Event ()

doorBell :: BellMachine
doorBell = id

mToS :: Double -> Double
mToS = (*) 60

eggTimer :: Event ()
eggTimer = atTime (mToS 3)

eggTimerM :: BellMachine
eggTimerM = const eggTimer

nifty :: BellMachine
nifty button = eggTimer `mappend` button

silent :: BellMachine
silent = const mempty

withElapsed :: Event a -> Event (a, TimeT)
withElapsed = fmap f . withPrevE . withTimeE
    where
      f ((aCur,tCur), (_,tPrev)) = (aCur,tCur-tPrev)

withElapsed_ :: Event a -> Event TimeT
withElapsed_ = (fmap . fmap) snd withElapsed

data State = Idle
           | WaitingForPeriodEnd
           | PlayingCycle

next :: State -> State
next Idle = WaitingForPeriodEnd
next WaitingForPeriodEnd = PlayingCycle
next PlayingCycle = Idle

period :: TimeT -> TimeT -> Event()
period delta t0 = atTimes (iterate ((+) delta) t0)

metronome :: BellMachine
metronome = switchE . fmap f . withTimeE . mealy Idle next . withElapsed_
    where
      f ((dt,PlayingCycle), t) = period dt t
      f _ = mempty

type Dot = (Double,Double)

data Input = I { mouse :: Behavior (Double,Double)
               , button :: Event ()
               , integral :: (VectorSpace v, Scalar v ~ TimeT) => 
                             Behavior v -> Behavior v
               }

type DotMachine = Input -> Behavior [Dot]
type SingleDotMachine = Input -> Behavior Dot

toDotMachine :: SingleDotMachine -> DotMachine
toDotMachine = (fmap . fmap) pure

printThem :: Show a => [a] -> IO ()
printThem = adaptE . fmap print . listE . zip [0,0.5..]

instance Show (IO ()) where
    show = const "<Action>"

type SCEvent = OSC -> Double -> Event ()

event01 :: Event Action
event01 = fmap print . listE $ zip [0,0.25..] ['a'..'g']


-- | Try:
-- 
-- > > t1 <- forkIO $ adaptSC msgs01
-- > > t2 <- forkIO $ adaptSC msgs02
-- 
adaptSC :: Sink (Event OSC)
adaptSC e = do
  t0 <- fmap (fromIntegral . ceiling) utcr
  let latency = 0.01
      e' = fmap (\(osc,dt) -> withSC3 $ 
                 \fd -> send fd $ 
                 Bundle (UTCr $ t0 + dt + latency) [osc]) 
           (withTimeE e)
  runE (pauseThreadUntil . (+ t0) . exactNB) e'

-- | Example OSC messages.
msgs01 :: Event OSC
msgs01 = listE $ zip [0,0.5..]
         (cycle [p [("dur",2)],
                 p [("freq",330)],
                 p [("freq",220)],
                 p [("freq",110),("dur",4)],
                 p [("freq",220)],
                 p [("freq",330)]])
    where
      p xs = s_new "percussive01" (-1) AddToTail 1 xs

-- | Another example OSC messages.
msgs02 :: Event OSC
msgs02 = listE $ zip [0,1..]
         (cycle [p 880, p 1320, p 1760, p 1320])
    where
      p f = s_new "percussive01" (-1) AddToTail 1 [("freq",f),("out",1)]

-- | Add some randomness to osc messages.
msgs03 :: IO (Event OSC)
msgs03 = do
  gen <- newStdGen
  let freqs = randomRs (50,12600) gen
      outs = randomRs (0,1::Int) gen
      times = scanl (+) 0 $ randomRs (0.001,0.1::Double) gen
      ms f o = s_new "percussive01" (-1) AddToTail 1 
             [("freq",f),
              ("dur",0.08),
              ("out",fromIntegral o),
              ("amp",0.04)]
  return $ listE $ zip times (zipWith ms freqs outs)
