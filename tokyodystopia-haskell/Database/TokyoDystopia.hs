------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia full text search.
--

module Database.TokyoDystopia
    ( 
      IDB.IDB 
    , QDB.QDB
    , JDB.JDB
    , module Database.TokyoDystopia.Class
    , module Database.TokyoDystopia.Types
    , module Database.TokyoDystopia.Utils

    ) where

import Database.TokyoDystopia.Class
import Database.TokyoDystopia.Types
import Database.TokyoDystopia.Utils

import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.IDB as IDB



