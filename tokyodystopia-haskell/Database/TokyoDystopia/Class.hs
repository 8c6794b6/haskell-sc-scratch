{-# LANGUAGE GeneralizedNewtypeDeriving,
             PackageImports,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeSynonymInstances
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

import Database.TokyoDystopia.Types
    ( OpenMode )

import Database.TokyoDystopia.IDB ( IDB )
import Database.TokyoDystopia.QDB ( QDB )
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB


-- | Wrapper for Tokyo Dystopia database related computation.
newtype TDM a = TDM 
    { -- | Unwraps Tokyo Dystopia Monad.
      runTDM :: IO a 
    } deriving (Functor, Monad, MonadIO)


-- | Typeclass for types of database found in tokyo dystopia.
-- 
-- * IDB
-- 
--       * All functions are implemented.
-- 
--       * Value type is @String@ (better to change this?)
-- 
-- * QDB 
-- 
--       * @get@ is not implemented, always returns Nothing.
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

    del :: db -> TDM ()
    del = undefined


instance TDDB IDB ByteString where

    new = TDM IDB.new

    open db path modes = TDM (IDB.open db path modes)

    close db = TDM (IDB.close db)

    get db key = TDM (IDB.get db key)

    put db key val = TDM (IDB.put db key val)

    del = TDM . IDB.del


instance TDDB QDB String where

    new = TDM QDB.new

    open db path modes = TDM (QDB.open db path modes)

    close db = TDM (QDB.close db)

    -- Get is not implemented in QDB.
    get _ _  = return Nothing

    put db key val = TDM (QDB.put db key val)

    del = TDM . QDB.del