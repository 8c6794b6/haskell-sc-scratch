------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.JDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCJDB interface.
--

module Database.TokyoDystopia.JDB
    ( JDB
    , new
    , open
    , close
    ) where

import Foreign ( Ptr )
import qualified Foreign.C.String as CS

import Database.TokyoCabinet.List ( List )
import Database.TokyoCabinet.Storable ( Storable )
import qualified Database.TokyoCabinet.List as TCL

import Database.TokyoDystopia.Internal
    ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )
import qualified Database.TokyoDystopia.FFI.JDB as FJ

-- | Wrapper for TCJDB
newtype JDB = JDB { unJDB :: Ptr FJ.TCJDB }

-- | Creates new JDB
new :: IO JDB
new = JDB `fmap` FJ.c_new

-- | OPen database from given path and open modes.
open :: JDB -> FilePath -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- CS.newCString path
  let mode = bitOr $ fmap (FJ.unOpenMode . f) modes
  FJ.c_open (unJDB db) path' mode
    where
      f OREADER = FJ.omReader
      f OWRITER = FJ.omWriter
      f OCREAT  = FJ.omCreat
      f OTRUNC  = FJ.omTrunc
      f ONOLCK  = FJ.omNolck
      f OLCKNB  = FJ.omLcknb

-- | Closes database
close :: JDB -> IO Bool
close = FJ.c_close . unJDB

-- | Put data with given key and values
put :: (Storable k) => JDB -> k -> List a -> IO Bool
put db k vs = do
  undefined
