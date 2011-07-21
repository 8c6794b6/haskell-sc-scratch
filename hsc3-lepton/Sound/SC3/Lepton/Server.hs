------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- A server for sending OSC message to scsynth. Manages nodes, buffers, and 
-- busses.
--
-- Note that "/n_go" message will only returned when node id is positive. 
--
module Lepton.Server where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)

import Sound.OpenSoundControl
import Sound.SC3

serveAll :: IO [ThreadId]
serveAll = mapM (forkIO . withSC3 . serve) 
  ["/tr", "/n_go", "/n_end", "/n_on", "/n_off", "/n_info", "/n_move"]

serve :: (Transport t) => String -> t -> IO ()
serve message fd = async fd (notify True) >> go
 where
  go = forever (wait fd message >>= print)
