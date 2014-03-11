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

import Data.ByteString.Char8 (pack)
import Data.Fixed

import Data.Binary (encode)
import Sound.OSC

import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.E

import qualified Codec.Compression.Zlib as Z

------------------------------------------------------------------------------
-- Client side message sending utility

-- | Connect to default lepton server, like 'withSC3'.
withLept :: Connection UDP a -> IO a
withLept = withTransport (openUDP "127.0.0.1" 58110)

-- | Send given OSC message to default `leptseq` server.
leptseq :: OSC o => o -> IO ()
leptseq = withLept . sendOSC

-- | Make bundled OSC message starting from next multiple of given
-- unit plus initial shift.
bundle' ::
  Double
  -- ^ Time unit, must be positive. Non-positive value would be ignored.
  -> Double
  -- ^ Initial shift, 0 means no shift.
  -> [Message]
  -- ^ OSC Messages to bundle
  -> IO Bundle
bundle' unit dt oscs
  | unit <= 0 = (\t -> bundle (t+dt) oscs) `fmap` time
  | otherwise = do
    now <- time
    let q = now `div'` unit :: Int
        t = (fromIntegral (succ q) * unit) + dt
    return $ bundle t oscs

------------------------------------------------------------------------------
-- OSC Messages building functions

-- | Add new pattern and run it.
l_new :: String -> E () (ToOSC Double) -> Message
l_new key pat = Message "/l_new" [ASCII_String $ pack key, encodePattern pat]

-- | Free pattern. When pattern with given key does not exist, do nothing.
l_free :: String -> Message
l_free key = Message "/l_free" [ASCII_String $ pack key]

-- | Free all patterns.
l_freeAll :: Message
l_freeAll = Message "/l_freeAll" []

-- | Show information of patterns.
l_dump :: Message
l_dump = Message "/l_dump" []

-- | Pause pattern. When pattern with given key does not exist, do nothing.
l_pause :: String -> Message
l_pause key = Message "/l_pause" [ASCII_String $ pack key]

-- | Resume pattern. When pattern with given key does not exist, do nothing.
l_run :: String -> Message
l_run key = Message "/l_run" [ASCII_String $ pack key]

-- | Add new pattern, but not start it.
l_add :: String -> E () (ToOSC Double) -> Message
l_add key pat = Message "/l_add" [ASCII_String $ pack key, encodePattern pat]

l_update :: String -> E () (ToOSC Double) -> Message
l_update key pat =
    Message "/l_update" [ASCII_String $ pack key, encodePattern pat]

-- | Encode pattern to compressed Blob message.
encodePattern :: E () (ToOSC Double) -> Datum
encodePattern = Blob . Z.compress . encode . etree

-- Alternative.
--
-- encodePattern :: Expr (ToOSC Double) -> Datum
-- encodePattern = Blob . Z.compress . encode
