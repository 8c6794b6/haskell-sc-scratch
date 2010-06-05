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
    ( TC.OpenMode(..)
    , TC.ECODE(..)
    , I.GetMode(..)
    , I.TuningOption(..)
    ) where

import Data.Bits

import Database.TokyoCabinet
    ( ECODE(..) )
import Database.TokyoCabinet as TC

import Database.TokyoDystopia.Internal as I
import Database.TokyoDystopia.TCI
import qualified Database.TokyoDystopia.FFI.TCI as IF

-- | Get string message from error code.
errmsg :: ECODE -> String
errmsg = TC.errmsg

withTCIDB :: IO TCIDB -> (TCIDB -> IO a) -> IO a 
withTCIDB = undefined