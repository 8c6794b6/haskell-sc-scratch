------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Parser for Datum used by Open Sound Control Message.
--
-- Note that, those parsers for 'Int32', 'Int64', 'Double', etc have
-- conflicting name with functions from 'Sound.OSC'. Use qualified import when
-- both functions were required.
--
module Sound.SC3.Lepton.Parser.Datum
  ( -- * Type
    DatumParser -- (..)

    -- * Parsing function
  , parseDatum

    -- * Parser building functions
  , datum
  , int32
  , int64
  , double
  , float
  , string
  , blob
  , timeStamp
  , midi
  ) where

import Data.Int (Int32, Int64)
import Control.Monad.Identity (Identity)
import Text.Parsec (ParsecT, ParseError)

import qualified Data.ByteString.Lazy as L
import qualified Text.Parsec as P

import Sound.OSC hiding (double, float, int32, int64, midi, string)

-- | Type synonym for Parser for list of Datum.
type DatumParser a = ParsecT [Datum] () Identity a

-- | Parse list of Datum with given DatumParser.
parseDatum :: DatumParser a -> [Datum] -> Either ParseError a
parseDatum p = P.parse p "osc"

-- | Parse datum.
datum :: DatumParser Datum
datum = dp return

-- | Parse OSC int32.
int32 :: DatumParser Int32
int32 = dp $ \d -> case d of Int32 x -> Just x; _ -> Nothing

-- | Parse OSC int64.
int64 :: DatumParser Int64
int64 = dp $ \d -> case d of Int64 x -> Just x; _ -> Nothing

-- | Parse OSC float.
float :: DatumParser Float
float = dp $ \d -> case d of Float x -> Just x; _ -> Nothing

-- | Parse OSC double.
double :: DatumParser Double
double = dp $ \d -> case d of Double x -> Just x; _ -> Nothing

-- | Parse OSC ASCII string.
string :: DatumParser ASCII
string = dp $ \d -> case d of ASCII_String x -> Just x; _ -> Nothing

-- | Parse OSC blob.
blob :: DatumParser L.ByteString
blob = dp $ \d -> case d of Blob x -> Just x; _ -> Nothing

-- | Parse OSC timestamp.
timeStamp :: DatumParser Time
timeStamp = dp $ \d -> case d of TimeStamp x -> Just x; _ -> Nothing

-- | Parse OSC midi
midi :: DatumParser MIDI
midi = dp $ \d -> case d of Midi m -> Just m; _ -> Nothing

-- | Wrapper for parser builder functions.
dp :: (Datum -> Maybe a) -> DatumParser a
dp f = P.tokenPrim showFunc updateFunc f
  where
    showFunc = show
    updateFunc pos _ _ = P.setSourceColumn pos (succ $ P.sourceColumn pos)

------------------------------------------------------------------------------
--
-- Below is implementation of simple parser without using parsec
-- Might use this again if parsec dependency get annoying.
--

-- -- | Parser for datum.
-- newtype DatumParser a = DatumParser {parse::[Datum] -> [(a,[Datum])]}

-- instance Monad DatumParser where
--     return a = DatumParser $ \ds -> [(a,ds)]
--     p >>= f = DatumParser $ \cs ->
--               concat [parse (f a) cs' | (a,cs') <- parse p cs]

-- instance MonadPlus DatumParser where
--     mzero = DatumParser $ \_ -> []
--     p `mplus` q = DatumParser $ \cs -> parse p cs ++ parse q cs

-- datum :: DatumParser Datum
-- datum = DatumParser $ \cs ->
--        case cs of
--          []     -> []
--          (d:ds) -> [(d,ds)]

-- int :: DatumParser Int
-- int = do {d <- datum; case d of {Int x -> return x; _ -> mzero}}

-- double :: DatumParser Double
-- double = do {d <- datum; case d of {Double x -> return x; _ -> mzero}}

-- float :: DatumParser Double
-- float = do {d <- datum; case d of {Float x -> return x; _ -> mzero}}

-- string :: DatumParser String
-- string = do {d <- datum; case d of {String x -> return x; _ -> mzero}}

-- blob :: DatumParser [Word8]
-- blob = do {d <- datum; case d of {Blob x -> return x; _ -> mzero}}

-- timeStamp :: DatumParser Time
-- timeStamp = do {d <- datum; case d of {TimeStamp x -> return x; _ -> mzero}}
