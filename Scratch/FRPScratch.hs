{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | Playing with Reactive.
--

module Scratch.FRPScratch where

import Control.Applicative
import Control.Concurrent (forkIO,killThread)
import Control.Concurrent.SampleVar
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.List (sort,unfoldr)
import System.Random hiding (next)

import Data.VectorSpace
import Data.AddBounds
import FRP.Reactive
import FRP.Reactive.Internal.Behavior
import FRP.Reactive.Internal.Fun
import FRP.Reactive.Internal.Misc
import FRP.Reactive.Internal.Timing
import FRP.Reactive.Improving (exact)
import FRP.Reactive.Reactive (exactNB)
import FRP.Reactive.LegacyAdapters 
import FRP.Reactive.Internal.Reactive (runE,forkR)

import Sound.OpenSoundControl
import Sound.SC3 hiding (stepper, K)

import Reusable
import SCTree
import SCQuery
import SCSched

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

instance Show (IO ()) where
    show = const "<Action>"

printThem :: Show a => [a] -> IO ()
printThem = adaptE . fmap print . listE . zip [0,0.5..]

event01 :: Event Action
event01 = fmap print . listE $ zip [0,0.25..] ['a'..'g']

drainS :: SampleVar a -> Sink (Sink a)
drainS sv snk = do
  emptySVar <- isEmptySampleVar sv
  unless emptySVar (readSampleVar sv >>= snk)

noSink = mempty

myUpdater :: IO TimeT -> Behavior Action -> IO Action
myUpdater getT acts = do
  actSVar <- newEmptySampleVar
  tid <- forkR (sleepPast' getT . exact)
         (writeSampleVar' actSVar <$> unb acts)
  tfunRef <- newIORef (noSink :: Sink TimeT)
  return $ do
    now <- getT
    drainS actSVar $ \actF ->
        case actF of
          K c -> do writeIORef tfunRef noSink >> c
          Fun f -> do writeIORef tfunRef f
    readIORef tfunRef >>= ($ now)
  where
    writeSampleVar' v x = do writeSampleVar v x

sleepPast' :: IO TimeT -> Sink (AddBounds TimeT)
sleepPast' _ MinBound = return ()
sleepPast' getT (NoBound target) = sleepPast getT target
sleepPast' _ MaxBound = error "sleepPast MaxBound. Expected?"


-- | Example OSC messages.
msgs01 :: Event OSC
msgs01 = listE $ zip [0,0.5..]
         (cycle [p [("dur",2)],
                 p [("freq",330)],
                 p [("freq",220)],
                 p [("freq",110),("dur",4)],
                 p [("freq",220)],
                 p [("freq",330)],
                 p [("freq",220)],
                 p [("freq",330)]])
    where
      p xs = s_new "percussive01" (-1) AddToTail 1 xs

-- | Another example OSC messages.
msgs02 :: Event OSC
msgs02 = listE $ zip times $
         cycle $ map p [880, 1320, 1760, 1320, 880]
    where
      p f = s_new "percussive01" (-1) AddToTail 1 [("freq",f),("pan",1)]
      times = scanl (+) 0 $ cycle [1.0,1,0.5,1.0,0.5]

-- | Another osc messages with randomness.
msgs03 :: IO (Event OSC)
msgs03 = do
  gen <- newStdGen
  let freqs = randomRs (50,12600) gen
      outs = randomRs (-0.5,0.5::Double) gen
      times = scanl (+) 0 $ randomRs (0.001,0.1::Double) gen
      ms f o = s_new "percussive01" (-1) AddToTail 1
             [("freq",f),
              ("dur",0.08),
              ("pan", o),
              ("amp",0.04)]
  return $ listE $ zip times (zipWith ms freqs outs)

msgs04 :: Event OSC
msgs04 = listE $ zip times (cycle oscs)
    where
      p f = s_new "percussive01" (-1) AddToTail 1 [("freq",f),("dur",0.5)]
      oscs = map p (repeat 88)
      times = scanl (+) 0 (cycle [1.0, 1.0, 1.0, 0.75, 0.25])

bpm :: Num a => a
bpm = 178

ms04 :: Event OSC
ms04 = listE $ zip times (cycle es)
    where
      times = [0,0.5..]
      es = map f [60, 62, 64, 60, 62, 64, 62, 60]
      f m = s_new "percussive01" (-1) AddToTail 1
            [("freq",midiCPS m),("amp",0.1),("dur",1),("pan",-0.5)]

ms05 :: Event OSC
ms05 = mconcat [e1,e2,e3]
    where
      [e1,e2,e3] = map (listE . zip times . cycle . (fmap f)) [f1,f2,f3]
      times = [0,4..]
      f1 = [48, 48, 50, 47]
      f2 = [55, 53, 53, 53]
      f3 = [64, 69, 69, 67]
      f m = s_new "percussive01" (-1) AddToTail 1
            [("freq",midiCPS m),("amp",0.1),("dur",4)]

b01 :: Behavior Action
b01 = stepper (return ()) $ fmap print $
      listE [(x,y)|x<-[0..10],y<-["hello","functioal","reactive"]]

b02 :: Behavior Action
b02 = stepper (return ()) $ fmap g $
      listE $ zip times oscs
    where
      g msg = withSC3 $ \fd -> send fd msg
      times = scanl1 (+) $ cycle [1,0.5,0.5]
      oscs = [s_new "percussive01" (-1) AddToTail 1 [("freq",440)],
              s_new "percussive01" (-1) AddToTail 1 [("freq",660)],
              s_new "percussive01" (-1) AddToTail 1 [("freq",440)],
              s_new "percussive01" (-1) AddToTail 1 [("freq",330)]]

b03 :: Behavior Action
b03 = stepper (return ()) $ fmap g $ listE $ zip [0..] oscs
    where
      g m = withSC3 $ \fd -> send fd m
      oscs = cycle oscs'
      oscs' = map tone [440,660,440,330]
      tone freq = s_new "percussive01" (-1) AddToTail 1 [("freq",freq)]
