------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.Types
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Types.
--

module Database.TokyoDystopia.Types
    (
    -- * Common datatypes with Tokyo Cabinet.
      TC.ECODE(..)
    , TC.OpenMode(..)
    , TCT.TuningOption(..)

    -- * Tokyo Dystopia addition
    , GetMode(..)
    ) where

import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.TDB as TCT

-- | Options for searching.
data GetMode = GMSUBSTR  -- ^ substring matching
             | GMPREFIX  -- ^ prefix matching
             | GMSUFFIX  -- ^ suffix matching
             | GMFULL    -- ^ full matching
             | GMTOKEN   -- ^ token matching
             | GMTOKPRE  -- ^ token prefix matching
             | GMTOKSUF  -- ^ token suffix matching
               deriving (Eq, Ord, Show)
