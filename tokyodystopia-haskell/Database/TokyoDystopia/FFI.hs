------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.Class
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- FFI bindings.
--

module Database.TokyoDystopia.FFI 
    ( IDB.TCIDB
    , QDB.TCQDB
    , JDB.TCJDB
    , WDB.TCWDB
    ) where

import qualified Database.TokyoDystopia.FFI.IDB as IDB
import qualified Database.TokyoDystopia.FFI.JDB as JDB
import qualified Database.TokyoDystopia.FFI.QDB as QDB
import qualified Database.TokyoDystopia.FFI.WDB as WDB

