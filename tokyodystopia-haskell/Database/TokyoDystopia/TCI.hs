------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.TCI
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCIDB interface.
--

module Database.TokyoDystopia.TCI
    ( TCIDB()
    , tcidbnew
    , open
    , close
    , put
    , get
    , search
    , search2
    ) where

import Data.ByteString
import Data.Int
import Foreign
import Foreign.C.Types
import Foreign.C.String

import Database.TokyoDystopia.Internal
import qualified Database.TokyoDystopia.FFI.TCI as F

-- | Datatype wrapper for TCIDB FFI binding.
newtype TCIDB = TCIDB { unTCIDB :: Ptr F.TCIDB }

-- | Creates new TCIDB.
tcidbnew :: IO TCIDB
tcidbnew = TCIDB `fmap` F.tcidbnew

-- | Open database from given path and conjunction of open modes.
open :: TCIDB -> String -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- newCString path
  let mode = openModes $ fmap modeFromCab modes
  res <- F.tcidbopen (unTCIDB db) path' (F.unOpenMode mode)
  return res

-- | Closes database
close :: TCIDB -> IO Bool
close db = F.tcidbclose (unTCIDB db)

-- | Put data with given key and value.
put :: TCIDB -> Int64 -> String -> IO Bool
put db k v = do
  v' <- newCString v
  F.tcidbput (unTCIDB db) k v'

-- | Get data with given key.
get :: TCIDB -> Int64 -> IO String
get db i = F.tcidbget (unTCIDB db) i >>= peekCString

-- | Search. 
search db query = undefined

-- | Search with given query and returns list of id keys.
search2 :: TCIDB -> String -> IO [Int64]
search2 db query = do
  counterP <- new 0 
  query' <- newCString query
  res <- F.tcidbsearch2 (unTCIDB db) query' counterP
  numResult <- fromIntegral `fmap` peek counterP
  peekArray numResult res
