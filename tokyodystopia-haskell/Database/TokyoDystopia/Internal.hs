------------------------------------------------------------------------------
-- |
-- Module      : Database.TokyoDystopia.Internal
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal helper functions .
--

module Database.TokyoDystopia.Internal 
    ( Cab.OpenMode(..)
    , modeFromCab
    , openModes
    ) where

import Data.Bits
import Data.ByteString (ByteString, pack)
import Data.Char (ord)

import qualified Database.TokyoDystopia.FFI.TCI as TCI
import qualified Database.TokyoCabinet as Cab

-- | Converter function for OpenMode from TokyoCabinet to TokyoDystopia.
modeFromCab :: Cab.OpenMode -> TCI.OpenMode
modeFromCab Cab.OREADER = TCI.omReader
modeFromCab Cab.OWRITER = TCI.omWriter
modeFromCab Cab.OCREAT  = TCI.omCreate
modeFromCab Cab.OTRUNC  = TCI.omTrunc
modeFromCab Cab.ONOLCK  = TCI.omNolck
modeFromCab Cab.OLCKNB  = TCI.omLcknb

-- | Bitwise or for OpenMode.
openModes :: [TCI.OpenMode] -> TCI.OpenMode
openModes = TCI.OpenMode . foldr (.|.) 0 . map TCI.unOpenMode

-- | ByteString String packer
packString :: String -> ByteString
packString = pack . (fmap (fromIntegral . ord)) 
