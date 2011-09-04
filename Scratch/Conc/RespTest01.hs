{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Testing functions and actions in Respond.
-}
module RespTest01 where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Random

import Data.Unique
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

main :: IO ()
main = bracket
  (mapM (forkIO . w) [loop01,loop02])
  (mapM_ killThread)
  (const $ w loop03)

-- main :: IO ()
-- main = w loop04

offsetDelay :: Double
offsetDelay = 0.1

-- | Wait until specified message has been returned from server.
waitUntil ::
  Transport t
  => t
  -> String
  -- ^ String to match in returned OSC message.
  -> Int
  -- ^ Int to match in first element of returned OSC message.
  -> IO ()
waitUntil fd str n = recv fd >>= \m -> case m of
  Message str' (Int n':_) | str == str' && n == n' -> return ()
  _                        -> waitUntil fd str n
{-# SPECIALISE waitUntil :: UDP -> String -> Int -> IO () #-}
{-# SPECIALISE waitUntil :: TCP -> String -> Int -> IO () #-}

-- | Wraps sending @notify True@ and @notify False@ messages before and
-- after 'withSC3'.
w :: (UDP -> IO a) -> IO a
w k = withSC3 $ \fd -> bracket
  (async fd (notify True) >> return fd)
  (\fd' -> send fd' $ notify False)
  k

newNid :: IO Int
newNid = (+ 10000) . hashUnique <$> newUnique

setup :: Transport t => t -> IO OSC
setup fd = do
  -- Respond.setup fd
  send fd $ bundle immediately
    [d_recv $ synthdef "rspdef1" rspdef1
    ,d_recv $ synthdef "rspdef2" rspdef2
    ,d_recv $ synthdef "rspdef3" rspdef3
    ,c_set [(100,1)]]
  wait fd "/done"

rspdef1 :: UGen
rspdef1 =
  out 0 $ pan2
  (sinOsc AR ("freq"@@440 * ("fmul"@@1 `lag2` 3.5)) 0 * 0.3 *
   envGen kr ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef2 :: UGen
rspdef2 =
  out 0 $ pan2
  (resonz (whiteNoise 'd' AR)
   ("freq"@@1320)
   (clip ("q"@@0.8 * mouseY KR 0.125 4 Exponential 0.1) 1e-5 9999e-4) * 0.3 *
   envGen kr ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvSin] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef3 :: UGen
rspdef3 = out ("out"@@100) (tExpRand 'f' 0.25 4 ("t_trig"@@1))

loop01 :: Transport t => t -> IO a
loop01 fd = go =<< utcr where
  go :: Double -> IO a
  go t0 = do
    let dt = 1/17
    let freqs = map midiCPS [40,41,48,52,55,58,62,67,70,74,79,86,90]
    nid  <- (+10000) <$> newNid
    freq <- (freqs !!) <$> randomRIO (0,length freqs -1)
    pan  <- randomRIO (-1,1)
    atk  <- randomRIO (1e-4,1)
    dcy  <- randomRIO (1e-2,1)
    amp  <- randomRIO (1e-3,1)
    send fd $ bundle (UTCr $ t0+dt+offsetDelay)
      [s_new "rspdef1" nid AddToTail 1
       [("freq",freq),("pan",pan),("atk",atk),("dcy",dcy),("amp",amp)]
      ,n_map nid [("fmul",100)]]
    waitUntil fd "/n_go" nid
    go (t0+dt)

loop02 :: Transport t => t -> IO a
loop02 fd = go =<< utcr where
  go :: Double -> IO a
  go t0 = do
    nid  <- newNid
    dt   <- randomRIO (1e-1,5e-1)
    freq <- exp <$> randomRIO (log 110, log 11000)
    atk  <- randomRIO (1e-4,2)
    dcy  <- randomRIO (1e-4,2)
    amp  <- randomRIO (1e-2,1)
    pan  <- randomRIO (-1,1)
    q    <- randomRIO (1e-3,99e-2)
    send fd $ bundle (UTCr $ t0+dt+offsetDelay)
      [s_new "rspdef2" nid AddToTail 1
       [("freq",freq),("pan",pan),("atk",atk)
       ,("dcy",dcy),("amp",amp),("q",q)]]
    waitUntil fd "/n_go" nid
    go (t0+dt)

loop03 :: Transport t => t -> IO a
loop03 fd = do
  now  <- utcr
  nid1 <- newNid
  nid2 <- newNid
  send fd $ bundle immediately
    [s_new "rspdef3" nid1 AddToHead 1 []
    ,s_new "tr" nid2 AddBefore nid1 []]
  go nid1 nid2 now
  where
    go :: Int -> Int -> Double -> IO a
    go n1 n2 t0 = do
      dt <- randomRIO (4,16)
      send fd $ bundle (UTCr $ t0+dt+offsetDelay)
        [n_set n1 [("out",100),("t_trig",1)]
        ,n_set n2 [("t_trig",1)]]
      waitUntil fd "/tr" n2
      go n1 n2 (t0+dt)

loop04 :: Transport t => t -> IO a
loop04 fd = do
  now <- utcr
  go now 0
  where
    atks = as ++ reverse as
    as = take 1024 $ iterate (*1.006) 0.002
    go t0 idx = do
      nid <- newNid
      dur <- randomRIO (1e-3, 7.5e-2)
      freq <- exp <$> randomRIO (log 80, log 12000)
      let atk = atks !! idx
      dcy <- randomRIO (1e-4,2e-1)
      amp <- randomRIO (1e-1,3e-1)
      pan <- randomRIO (-1,1)
      send fd $ bundle (UTCr $ t0+dur+offsetDelay)
        [s_new "rspdef1" nid AddToTail 1
          [("freq",freq),("pan",pan),("atk",atk),("dcy",dcy),("amp",amp)]]
      go (t0+dur) (succ idx `mod` 2048)
