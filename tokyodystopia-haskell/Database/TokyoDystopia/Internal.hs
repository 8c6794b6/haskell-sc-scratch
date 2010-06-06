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
    ( bitOr
    ) where

import Data.Bits (Bits, (.|.))

import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.TDB as TCT
import qualified Database.TokyoDystopia.Types as TDT
import qualified Database.TokyoDystopia.FFI.IDB as IDB

-- | Bitwise or for bits.
bitOr :: (Bits a) => [a] -> a
bitOr = foldr (.|.) 0
