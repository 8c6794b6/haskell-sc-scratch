{-# LANGUAGE GeneralizedNewtypeDeriving,
             PackageImports,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             UndecidableInstances
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
import "monads-fd" Control.Monad.Trans
    ( MonadIO )

import Database.TokyoCabinet.List ( List )
import Database.TokyoCabinet.Storable ( Storable )
import qualified Database.TokyoCabinet.List as TCL

import Database.TokyoDystopia.Types 
    ( OpenMode 
    , GetMode
    , TuningOption )
import Database.TokyoDystopia.IDB ( IDB )
import Database.TokyoDystopia.QDB ( QDB )
import Database.TokyoDystopia.JDB ( JDB )
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB


-- | Wrapper for Tokyo Dystopia database related computation.
newtype TDM a = TDM 
    { -- | Unwraps Tokyo Dystopia Monad.
      runTDM :: IO a 
    } deriving (Functor, Monad, MonadIO)


-- | Typeclass for types of database found in tokyo dystopia.
-- 
-- * IDB : All functions are implemented.
-- 
-- * QDB : @get@ is not implemented, always returns Nothing.
-- 
-- * JDB : Value must be defined as concrete type.
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


instance TDDB IDB ByteString where

    new = TDM IDB.new

    open db path modes = TDM (IDB.open db path modes)

    close db = TDM (IDB.close db)

    get db key = TDM (IDB.get db key)

    put db key val = TDM (IDB.put db key val)

    search db query modes = TDM $ IDB.search db query modes

    del = TDM . IDB.del


instance TDDB QDB ByteString where

    new = TDM QDB.new

    open db path modes = TDM (QDB.open db path modes)

    close db = TDM (QDB.close db)

    -- Get is not implemented in QDB.
    get _ _  = return Nothing

    put db key val = TDM (QDB.put db key val)

    search db query modes = TDM $ QDB.search db query modes

    del = TDM . QDB.del

instance (Storable a) => TDDB JDB (List a) where
    new = TDM JDB.new
    open db path modes = TDM $ JDB.open db path modes
    close = TDM . JDB.close
    get = undefined
    put = undefined
    search = undefined
    del = undefined