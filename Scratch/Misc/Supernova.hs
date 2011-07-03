module Supernova where

import Control.Monad (replicateM_)
import Data.Unique
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Monadic
import Sound.SC3.Lepton

setup :: (Transport t) => t -> IO ()
setup fd = do
  reset fd
  async fd $ d_recv $ synthdef "foo" foo
  send fd $ g_new $ map (\n -> (n,AddToTail,1)) [101..108]
  
withS :: (UDP -> IO a) -> IO a
withS = withTransport (openUDP "127.0.0.1" 57111)
  
makeFoos :: (Transport t) => Int -> t -> IO ()
makeFoos n fd = replicateM_ n go
  where
    go = do
      freq <- randomRIO (100,8000)
      pan  <- randomRIO (-1,1)
      nid <- hashUnique `fmap` newUnique
      let gid = 101 + (nid `mod` 8)
      send fd $ s_new "foo" nid AddToTail gid
        [("freq",freq),("pan",pan),("amp",0.0001)]
      send fd (sync 1)
      wait fd "/synced"
  
foo :: UGen
foo = out 0 sig
  where
      sig = pan2 o position 1 
      o = sinOsc ar freq 0 * amp
      position = control kr "pan" 0
      freq = control kr "freq" 440
      amp = control kr "amp" 0.1

