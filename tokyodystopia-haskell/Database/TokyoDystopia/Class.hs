{-# LANGUAGE GeneralizedNewtypeDeriving,
             PackageImports,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances
  #-}
------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.Class
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- TypeClass and instance definitions for tokyo dystopia database.
--

module Database.TokyoDystopia.Class where

import Data.ByteString ( ByteString )
import Data.Int ( Int64 )
import "monads-fd" Control.Monad.Trans( MonadIO )

import Database.TokyoCabinet.List ( List )

import Database.TokyoDystopia.Types 
    ( OpenMode 
    , GetMode
    , TuningOption )
import Database.TokyoDystopia.IDB ( IDB )
import Database.TokyoDystopia.QDB ( QDB )
import Database.TokyoDystopia.JDB ( JDB )
import Database.TokyoDystopia.WDB ( WDB )
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.WDB as WDB


-- | Wrapper for Tokyo Dystopia database related computation.
newtype TDM a = TDM 
    { -- | Unwraps Tokyo Dystopia Monad.
      runTDM :: IO a 
    } deriving (Functor, Monad, MonadIO)


-- | Typeclass for types of database found in tokyo dystopia.
-- 
-- * IDB : All functions are implemented.
-- 
-- * QDB : @get@ will always return @Nothing@.
-- 
-- * JDB : All functions are implemented.
--
-- * WDB : @get@ will always return @Nothing@, and @GetMode@ in search has no effect.
-- 
class TDDB db val | db -> val where

    new :: TDM db
    new = undefined

    open :: db -> FilePath -> [OpenMode] -> TDM Bool
    open = undefined
    
    close :: db -> TDM Bool
    close = undefined

    get :: db -> Int64 -> TDM (Maybe val)
    get = undefined

    put :: db -> Int64 -> val -> TDM Bool
    put = undefined

    search :: db -> String -> [GetMode] -> TDM [Int64]
    search = undefined

    del :: db -> TDM ()
    del = undefined

    tune :: db -> Int64 -> Int64 -> Int64 -> [TuningOption] -> TDM Bool
    tune = undefined

    path :: db -> TDM FilePath
    path = undefined

    fsiz :: db -> TDM Int64
    fsiz = undefined

    sync :: db -> TDM Bool
    sync = undefined


------------------------------------------------------------------------------
-- 
-- IDB
-- 
------------------------------------------------------------------------------

instance TDDB IDB ByteString where

    new = TDM IDB.new

    open db file modes = TDM (IDB.open db file modes)

    close db = TDM (IDB.close db)

    get db key = TDM (IDB.get db key)

    put db key val = TDM (IDB.put db key val)

    search db query modes = TDM $ IDB.search db query modes

    del = TDM . IDB.del

    tune db etnum ernum iusiz opts = TDM $ IDB.tune db etnum ernum iusiz opts 

    path = TDM . IDB.path

    fsiz = TDM . IDB.fsiz

    sync = TDM . IDB.sync


------------------------------------------------------------------------------
-- 
-- QDB
-- 
------------------------------------------------------------------------------

instance TDDB QDB ByteString where

    new = TDM QDB.new

    open db file modes = TDM (QDB.open db file modes)

    close db = TDM (QDB.close db)

    get _ _  = return Nothing

    put db key val = TDM (QDB.put db key val)

    search db query modes = TDM $ QDB.search db query modes

    del = TDM . QDB.del

    tune db etnum _ _ opts = TDM $ QDB.tune db etnum opts 

    path = TDM . QDB.path

    fsiz = TDM . QDB.fsiz

    sync = TDM . QDB.sync


------------------------------------------------------------------------------
-- 
-- JDB
-- 
------------------------------------------------------------------------------

instance TDDB JDB (List ByteString) where

    new = TDM JDB.new

    open db file modes = TDM $ JDB.open db file modes

    close = TDM . JDB.close

    get db k = TDM $ fmap return $ JDB.get db k

    put db k v = TDM $ JDB.put db k v 

    search db query modes = TDM $ JDB.search db query modes

    del = TDM . JDB.del

    tune db etnum ernum iusiz opts = TDM $ JDB.tune db etnum ernum iusiz opts 

    path = TDM . JDB.path

    fsiz = TDM . JDB.fsiz

    sync = TDM . JDB.sync


------------------------------------------------------------------------------
-- 
-- WDB
-- 
------------------------------------------------------------------------------

instance TDDB WDB (List ByteString) where

    new = TDM WDB.new

    open db file modes = TDM $ WDB.open db file modes

    close = TDM . WDB.close

    get _ _ = TDM (return Nothing)

    put db k v = TDM $ WDB.put db k v 

    search db query _ = TDM $ WDB.search db query 

    del = TDM . WDB.del

    tune db etnum _ _ opts = TDM $ WDB.tune db etnum opts 

    path = TDM . WDB.path

    fsiz = TDM . WDB.fsiz

    sync = TDM . WDB.sync
