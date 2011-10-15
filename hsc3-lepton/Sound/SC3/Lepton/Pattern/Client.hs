{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Client side utility for communicating with pattern server.

-}
module Sound.SC3.Lepton.Pattern.Client
  ( -- * Client side utility action
    withLept
  , leptseq
  , bundle'
    -- * Commands
  , l_add
  , l_dump
  , l_new
  , l_free
  , l_freeAll
  , l_pause
  , l_run
  , l_update
  ) where

import Data.Fixed

import Data.Binary (encode)
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.E

import qualified Codec.Compression.Zlib as Z

------------------------------------------------------------------------------
-- Client side message sending utility

-- | Connect to default lepton server, like 'withSC3'.
withLept :: (UDP -> IO a) -> IO a
withLept = withTransport (openUDP "127.0.0.1" 58110)

-- | Send given OSC message to default `leptseq` server.
leptseq :: OSC -> IO ()
leptseq o = withLept (\fd -> send fd o)

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
    let q = now `div'` unit :: Int
        t = (fromIntegral (succ q) * unit) + dt
    return $ bundle (UTCr t) oscs

------------------------------------------------------------------------------
-- OSC Messages building functions

-- | Add new pattern and run it.
l_new :: String -> E () (ToOSC Double) -> OSC    
l_new key pat = Message "/l_new" [String key, encodePattern pat]

-- | Free pattern. When pattern with given key does not exist, do nothing.
l_free :: String -> OSC
l_free key = Message "/l_free" [String key]

-- | Free all patterns.
l_freeAll :: OSC
l_freeAll = Message "/l_freeAll" []

-- | Show information of patterns.
l_dump :: OSC
l_dump = Message "/l_dump" []

-- | Pause pattern. When pattern with given key does not exist, do nothing.
l_pause :: String -> OSC
l_pause key = Message "/l_pause" [String key]

-- | Resume pattern. When pattern with given key does not exist, do nothing.
l_run :: String -> OSC
l_run key = Message "/l_run" [String key]

-- | Add new pattern, but not start it.
l_add :: String -> E () (ToOSC Double) -> OSC
l_add key pat = Message "/l_add" [String key, encodePattern pat]

l_update :: String -> E () (ToOSC Double) -> OSC
l_update key pat = Message "/l_update" [String key, encodePattern pat]

-- | Encode pattern to compressed Blob message.
encodePattern :: E () (ToOSC Double) -> Datum
encodePattern = Blob . Z.compress . encode . etree

-- Alternative.
--
-- encodePattern :: Expr (ToOSC Double) -> Datum
-- encodePattern = Blob . Z.compress . encode
