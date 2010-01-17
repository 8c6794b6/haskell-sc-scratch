------------------------------------------------------------------------------
-- | Playing with scheduling.
-- Using FRP thingy from reactive package.
-- 
module SCSched where

import Control.Concurrent (forkIO,killThread)
import Sound.OpenSoundControl
import Sound.SC3 (withSC3)

import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import FRP.Reactive.Reactive (exactNB)
import FRP.Reactive.Internal.Reactive (runE)

type BPM = Double

-- | Try:
--
-- > > t1 <- forkIO $ adaptSC (standby 120 16) msgs04
-- > > t2 <- forkIO $ adaptSC (standby 120 16) msgs02
-- > > t3 <- msgs03 >>= \m3 -> forkIO $ adaptSC (standby 120 16) m3
--
-- Event has instance definition of @Monoid@.
--
-- > > t1 <- forkIO $ adaptSC id $ mconcat [msgs01,msgs02]
--
adaptSC :: (Double -> Double) -> BPM -> Sink (Event OSC)
adaptSC f b e = do
  t0 <- fmap f utcr
  let toB d = d * 60 / b
      latency = 0.01
      e' = fmap (\(osc,dt) -> withSC3 $ \fd ->
                 send fd $ Bundle (UTCr $ t0 + toB dt + latency) [osc])
           (withTimeE e)
  runE (pauseThreadUntil . (+t0) . toB . exactNB) e'

-- | Standby timing of @adaptSC@. Specify offset of starting time.
standby :: Double -> Double -> Double -> Double
standby beats num now = nextTime
    where
      nextTime = (60/beats) * num * q
      q = fromIntegral $ ceiling $ now / (num * (60/beats))


