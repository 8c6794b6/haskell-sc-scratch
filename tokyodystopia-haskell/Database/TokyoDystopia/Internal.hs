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
    ( TC.OpenMode(..)
    , TCT.TuningOption(..)
    , GetMode(..)
    , modeFromCab
    , openModes
    ) where

import Data.Bits ((.|.))

import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.TDB as TCT
import qualified Database.TokyoDystopia.FFI.TCI as TCI

-- | Converter function for OpenMode from TokyoCabinet to TokyoDystopia.
modeFromCab :: TC.OpenMode -> TCI.OpenMode
modeFromCab TC.OREADER = TCI.omReader
modeFromCab TC.OWRITER = TCI.omWriter
modeFromCab TC.OCREAT  = TCI.omCreate
modeFromCab TC.OTRUNC  = TCI.omTrunc
modeFromCab TC.ONOLCK  = TCI.omNolck
modeFromCab TC.OLCKNB  = TCI.omLcknb

-- | Bitwise or for OpenMode.
openModes :: [TCI.OpenMode] -> TCI.OpenMode
openModes = TCI.OpenMode . foldr (.|.) 0 . map TCI.unOpenMode

data GetMode = GMSUBSTR
             | GMPREFIX
             | GMSUFFIX
             | GMFULL
             | GMTOKEN
             | GMTOKPRE
             | GMTOKSUF
               deriving (Eq, Show)

