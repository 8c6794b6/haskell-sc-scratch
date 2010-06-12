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
    , withIDB
    , withJDB
    , withQDB
    , withWDB
    , withTDDB
    ) where

import Data.ByteString ( ByteString )
import Database.TokyoDystopia.Types 
    ( ECODE(..)
    , OpenMode(..) )
import Database.TokyoDystopia.Class ()
import Database.TokyoDystopia.Class ( TDDB, TDM )
import Database.TokyoDystopia.IDB ( IDB )
import Database.TokyoDystopia.JDB ( JDB )
import Database.TokyoDystopia.QDB ( QDB )
import Database.TokyoDystopia.WDB ( WDB )

import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia.Class as TDDB
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.WDB as WDB


withTDDB :: (TDDB db val) => FilePath -> [OpenMode] -> (db -> TDM a) -> IO a
withTDDB file modes func= TDDB.runTDM $ do
  db <- TDDB.new
  TDDB.open db file modes
  res <- func db
  TDDB.close db
  TDDB.del db
  return res


withIDB :: FilePath -> [OpenMode] -> (IDB -> IO a) -> IO a
withIDB = withDB IDB.new IDB.open IDB.close IDB.del


withQDB :: FilePath -> [OpenMode] -> (QDB -> IO a) -> IO a
withQDB = withDB QDB.new QDB.open QDB.close QDB.del


withJDB :: FilePath -> [OpenMode] -> (JDB -> IO a) -> IO a
withJDB = withDB JDB.new JDB.open JDB.close JDB.del


withWDB :: FilePath -> [OpenMode] -> (WDB -> IO a) -> IO a
withWDB = withDB WDB.new WDB.open WDB.close WDB.del


-- | Helper for making `with` variants.
withDB :: (Monad m)
       => m db                           -- ^ new
       -> (db -> path -> modes -> m a)   -- ^ open
       -> (db -> m b)                    -- ^ close
       -> (db -> m c)                    -- ^ del
       -> path -> modes -> (db -> m res) -- ^ target function
       -> m res
withDB newF openF closeF delF = \file modes task -> do
  db <- newF
  openF db file modes
  res <- task db
  closeF db
  delF db
  return res


-- | Get string message from error code.
errmsg :: ECODE -> String
errmsg = TC.errmsg
