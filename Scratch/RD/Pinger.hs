------------------------------------------------------------------------------
-- Ping example from rd.
--

module Pinger where

import Sound.SC3
import Sound.OpenSoundControl
import Control.Monad (when)
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay)

-- | Pause current thread for the indicated duration, given in seconds.
sleep :: Double -> IO ()
sleep n = when (n > 1e-4) (threadDelay (floor (n * 1e6)))

-- | Pause current thread until the given utcr time.
sleepUntil :: Double -> IO ()
sleepUntil t = sleep . (t -) =<< utcr

at :: Double -> (Double -> IO Double) -> IO t
at t f = do
  n <- f t
  sleepUntil (n+t)
  at (n+t) f

ping :: UGen
ping = out 0 (pan2 (sinOsc ar f 0 * e) p 1)
    where e = envGen kr 1 a 0 1 RemoveSynth s
          s = envPerc 1e-3 200e-3
          a = control kr "amp" 0.1
          f = control kr "freq" 440
          p = control kr "pan" 0

latency :: Double
latency = 0.01

bundle :: Double -> [OSC] -> OSC
bundle t m = Bundle (UTCr $ t + latency) m

tu = 1.1389823209

pinger :: Double -> Double -> Double -> IO a
pinger freq a c = do
  now <- utcr
  let (q,_) = properFraction (now/tu)
      d0 = tu * (fromIntegral q + 1)
  at d0 (g freq a c)

g :: Double -> Double -> Double -> (Double -> IO Double)
g freq a c t = withSC3 $ \fd -> do
  send fd $ bundle t
       [s_new "ping" (-1) AddToTail 1
              [("pan", c), ("freq", freq), ("amp", a)]]
  putStrLn "Sending ping"
  return tu

main :: IO ()
main = withSC3 $ \fd -> do
         channel <- fmap (read . head) getArgs
         async fd $ d_recv $ synthdef "ping" $ ping
         putStrLn "Resetting scsynth"
         reset fd
         putStrLn "Starting schedule thread"
         forkIO (pinger 440 0.1 channel)
         putStrLn "delaying main thread"
         pauseThread 30
         putStrLn "End of delay, existing"
