------------------------------------------------------------------------------
-- Ping example from rd.
--

module Pinger where

import Sound.SC3
import Sound.OpenSoundControl
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

at :: Double -> (Double -> IO Double) -> IO t
at t f = do
  n <- f t
  pauseThreadUntil (n + t)
  at (n + t) f

ping = out (control kr "out" 0) (sinOsc ar f 0 * e)
    where e = envGen kr 1 a 0 1 RemoveSynth s
          s = envPerc 0.1 0.6
          a = control kr "amp" 0.1
          f = control kr "freq" 440

latency = 0.01

bundle t m = Bundle (UTCr $ t + latency) m

pinger freq a c = do
  now <- utcr
  at (fromIntegral $ ceiling now) f
    where
      f t = withSC3 $ \fd -> 
            do send fd $ bundle t 
                  [s_new "ping" (-1) AddToTail 1 
                   [("out",c),("freq",freq),("amp",a)]]
               putStrLn "Sending ping"
               return 1

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
