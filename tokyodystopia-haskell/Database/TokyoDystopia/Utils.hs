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
    (C.OpenMode(..)
    )where

import Data.Bits

import Database.TokyoCabinet as C

import Database.TokyoDystopia.Internal
import Database.TokyoDystopia.TCI
import qualified Database.TokyoDystopia.FFI.TCI as IF

withTCIDB :: IO TCIDB -> (TCIDB -> IO a) -> IO a 
withTCIDB = undefined