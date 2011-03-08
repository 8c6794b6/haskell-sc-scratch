------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with concurrency, client side.
--
module Client where

import Control.Exception (bracket)
import Sound.OpenSoundControl 
  ( Transport(..)
  , UDP(..)
  , OSC(..)
  , Datum(..)  
  , openUDP )

-- | Default server, at least at this stage.
withL :: (UDP -> IO a) -> IO a
withL = (openUDP "127.0.0.1" 57130) `bracket` close

-- For S4.

kill :: String -> OSC
kill name = Message "/kill" [String name]

dump :: OSC
dump = Message "/dump" []

dumpOne :: String -> OSC
dumpOne name = Message "/show" [String name]

pause :: String -> OSC
pause name = Message "/pause" [String name]

resume :: String -> OSC
resume name = Message "/resume" [String name]

-- For S5.

rep :: [Int] -> OSC
rep xs = Message "/rep" (map Int xs)

once :: [Int] -> OSC
once xs = Message "/once" (map Int xs)


