------------------------------------------------------------------------------
-- |
-- Module      : Database.TokyoDystopia.Utils
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Utility functions for tokyodystopia.
--

module Database.TokyoDystopia.Utils 
    ( errmsg
    ) where

import Data.Bits ((.|.))
import Database.TokyoCabinet
    ( ECODE(..) )
import qualified Database.TokyoCabinet as TC

-- | Get string message from error code.
errmsg :: ECODE -> String
errmsg = TC.errmsg

