{-

Scratchy workspace used inside emacs.

-}
module Main where

import Control.Concurrent
import Control.Exception (bracket)

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import qualified Network.Memcache as Mem
import qualified Network.Memcache.Protocol as Mem

main = withSC3 printRootNode

withMem :: (Mem.Server -> IO a) -> IO a
withMem k = bracket con discon k where
  con = Mem.connect "127.0.0.1" 11211
  discon = Mem.disconnect
  
showMstat :: IO ()
showMstat = withMem $ \mem -> mapM_ print =<< Mem.stats mem

a01 = withMem $ \mem -> Mem.set mem "foo" (3::Int)
  
a02 = withMem $ \mem -> Mem.get mem "foo" :: IO (Maybe Int)
  
keys = ["bar1", "bar2"]
