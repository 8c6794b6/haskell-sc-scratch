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
    ( modeFromCab
    , openModes
    ) where

import Data.Bits (Bits, (.|.))


import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.TDB as TCT
import qualified Database.TokyoDystopia.Types as TDT
import qualified Database.TokyoDystopia.FFI.IDB as IDB

-- | Convert OpenMode from TokyoCabinet to TokyoDystopia.
modeFromCab :: TC.OpenMode -> IDB.OpenMode
modeFromCab TC.OREADER = IDB.omReader
modeFromCab TC.OWRITER = IDB.omWriter
modeFromCab TC.OCREAT  = IDB.omCreate
modeFromCab TC.OTRUNC  = IDB.omTrunc
modeFromCab TC.ONOLCK  = IDB.omNolck
modeFromCab TC.OLCKNB  = IDB.omLcknb

toFromCab :: TCT.TuningOption -> IDB.TuningOption
toFromCab TCT.TLARGE = IDB.toLarge
toFromCab TCT.TDEFLATE = IDB.toDeflate
toFromCab TCT.TBZIP = IDB.toBzip
toFromCab TCT.TTCBS = IDB.toTcbs


-- | Bitwise or for OpenMode.
openModes :: [IDB.OpenMode] -> IDB.OpenMode
openModes = IDB.OpenMode . foldr (.|.) 0 . map IDB.unOpenMode

-- | Bitwise or for bits.
bitOr :: (Bits a) => [a] -> a
bitOr = foldr (.|.) 0
