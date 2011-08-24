{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Testing functions and actions in Respond.
-}
module RespTest01 where

import Control.Applicative
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Respond hiding (setup)
import qualified Respond

setup :: Transport t => t -> IO OSC
setup fd = do
  Respond.setup fd
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
loop01 fd = do
  now <- utcr
  go now
  where
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
loop02 fd = do
  now <- utcr
  go now
  where
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
