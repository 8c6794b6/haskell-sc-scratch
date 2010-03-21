------------------------------------------------------------------------------
-- | Playing with scheduling.
--
-- Using FRP from reactive package.
--
module Sound.SC3.Wing.Schedule
    ( BPM,
      Event,
      TimeT,
      spawn,
      adaptSC,
      standby,
      takeE,
      -- dropE,
      -- shiftE,
      -- loopE,
      listE )
where

import Control.Applicative
import Control.Arrow (first,second)
import Control.Concurrent (forkIO,killThread,ThreadId)
import Data.Monoid

import Sound.OpenSoundControl
import Sound.SC3 (withSC3)

import FRP.Reactive
import FRP.Reactive.Future
import FRP.Reactive.LegacyAdapters
import FRP.Reactive.Reactive (exactNB)
import FRP.Reactive.Internal.Reactive (runE,forkE)
import FRP.Reactive.PrimReactive (eventOcc)

type BPM = Double

-- | Wait for given beats in given bpm, and plays the event.
spawn :: Double -> BPM -> Event OSC -> IO ()
spawn dn b es = adaptSC (standby b dn) b es

-- | Try:
--
-- > > t1 <- forkIO $ adaptSC (standby 120 16) 120 msgs04
-- > > t2 <- forkIO $ adaptSC (standby 120 16) 120 (untilE msgs02 (atTime 8))
-- > > t3 <- msgs03 >>= \m3 -> forkIO $ adaptSC (standby 120 16) m3
--
-- Event has instance definition of @Monoid@. Try below.
--
-- > > t1 <- forkIO $ adaptSC id $ mconcat [msgs01,msgs02]
--
adaptSC :: (Double -> Double) -- ^ Function applyed to initial starting time
        -> BPM -- ^ Beats per minute
        -> Event OSC -- ^ Event of OSC message send to default scsynth
        -> IO ()
adaptSC f b e = do
  t0 <- fmap f utcr
  let toB d = d * 60 / b
      latency = 0.01
      e' = fmap (\(osc,dt) -> withSC3 $ \fd ->
                 send fd $ Bundle (UTCr $ t0 + toB dt + latency) [osc])
           (withTimeE e)
  runE (pauseThreadUntil . (+t0) . toB . exactNB) e'


-- | Standby timing of @adaptSC@. Could be used to specify offset of
-- starting time.
standby :: BPM -- ^ bpm
        -> Double -- ^ Number of beats to wait.
        -> Double -- ^ Current time
        -> Double
standby _ 0 t0 = t0 -- Not the best definition.
standby beats num t0 = (60/beats) * num * q
    where
      q = fromIntegral $ ceiling $ t0 / (num * (60/beats))

-- | Take events until specified time
takeE :: TimeT -> Event a -> Event a
takeE t e = untilE e (atTime t)

-- | Drop first event until second event occurs
--
-- XXX: Delays a lot. And even worth, ghci hangs.
--
-- dropE :: TimeT -> Event a -> Event a
-- dropE t = fmap fst . filterE (snd . second (>= t)) . withTimeE

-- -- | Shift the time of occurrences.
-- shiftE :: (TimeT -> TimeT) -> Event a -> Event a
-- shiftE f es = error "Not implemented yet."

-- -- | Loop a event forever.
-- loopE :: Event a -> Event a
-- loopE es = error "Not implemented yet."


