{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Client side utility for communicating with pattern server.

-}
module Scratch.Client where

import Data.Fixed

import Data.Binary (decode, encode)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import qualified Data.ByteString.Lazy.Char8 as LC8

import Scratch.Bz (lazyByteStringP)

------------------------------------------------------------------------------
-- Client side message sending utility

-- | Connect to default lepton server, like 'withSC3'.
withLept :: (UDP -> IO a) -> IO a
withLept = withTransport (openUDP "127.0.0.1" 58110)

-- | Make bundled OSC message starting from next multiple of given
-- unit plus initial shift.
bundle' ::
  Double
  -- ^ Time unit, must be positive. Non-positive value would be ignored.
  -> Double
  -- ^ Initial shift, 0 means no shift.
  -> [OSC]
  -- ^ OSC Messages to bundle
  -> IO OSC
bundle' unit dt oscs
  | unit <= 0 = (\t -> bundle (UTCr $ t+dt) oscs) `fmap` utcr
  | otherwise = do
    now <- utcr
    let q = now `div'` unit
        t = (fromIntegral (succ q) * unit) + dt
    return $ bundle (UTCr t) oscs

------------------------------------------------------------------------------
-- OSC Messages building functions

-- | Add new pattern and run it.
l_new :: String -> Expr (ToOSC Double) -> OSC
-- l_new key pat = Message "/l_new" [String key,Blob (encode pat)]
l_new key pat = Message "/l_new" [String key, Blob pat'] where
  pat' = case fromExpr pat of
    -- Right p  -> LC8.pack $ showP $ p
    Right p  -> lazyByteStringP p
    Left err -> error $ "l_new: " ++ err

-- | Free pattern. When pattern with given key does not exist, do nothing.
l_free :: String -> OSC
l_free key = Message "/l_free" [String key]

-- | Free all patterns.
l_freeAll :: OSC
l_freeAll = Message "/l_freeAll" []

-- | Show information of patterns.
l_dump :: OSC
l_dump = Message "/l_dump" []

{-

Not implemented yet.

-}

-- | Pause pattern. When pattern with given key does not exist, do nothing.
l_pause :: String -> OSC
l_pause key = Message "/l_pause" [String key]

-- | Resume pattern. When pattern with given key does not exist, do nothing.
l_run :: String -> OSC
l_run key = Message "/l_run" [String key]

-- | Add new pattern, but not start it.
l_add :: String -> Expr (ToOSC Double) -> OSC
l_add key pat = Message "/l_add" [String key, Blob (encode pat)]
