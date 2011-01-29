------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Print incoming OSC message.
--
module PrintResponder where

import Control.Monad (forever)

import Sound.OpenSoundControl
import Sound.SC3 (notify, async, withSC3)

main = withSC3 respond

respond :: (Transport t) => t -> IO ()
respond fd = do
  async fd $ notify True
  forever $ recv fd >>= print