------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.IDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCIDB interface.
--

module Database.TokyoDystopia.IDB
    ( TCIDB()
    , close
    , copy
    , del
    , ecode
    , fsiz
    , get
    , iterinit
    , iternext
    , new
    , open
    , optimize
    , path
    , put
    , rnum
    , search
    , search2
    , setcache
    , setfwmmax
    , sync
    , tune
    , vanish
    ) where

import qualified Data.ByteString as B
import Data.Int ( Int64 )
import Foreign ( Ptr )
import qualified Foreign.ForeignPtr as FF
import qualified Foreign as FG
import Foreign.C.Types as CT
import Foreign.C.String as CS
import Database.TokyoCabinet.Storable ( Storable )
import Database.TokyoCabinet ( ECODE(..) ) 
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS

import Database.TokyoDystopia.Internal
    ( openModes
    , modeFromCab )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )
import qualified Database.TokyoDystopia.FFI.IDB as F
import qualified Database.TokyoDystopia.Internal as I

-- | Newtype wrapper for TCIDB FFI binding.
newtype TCIDB = TCIDB { unTCIDB :: Ptr F.TCIDB }

-- | Creates new TCIDB.
new :: IO TCIDB
new = TCIDB `fmap` F.c_new

-- | Open database from given path and conjunction of open modes.
open :: TCIDB -> FilePath -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- CS.newCString path
  let mode = openModes $ fmap modeFromCab modes
  res <- F.c_open (unTCIDB db) path' (F.unOpenMode mode)
  return res

-- | Closes database
close :: TCIDB -> IO Bool
close = F.c_close . unTCIDB

-- | Put data with given key and value.
put :: (Storable k) => TCIDB -> k -> String -> IO Bool
put db k v = do
  F.c_put (unTCIDB db) (TCS.toInt64 k) =<< CS.newCString v

-- | Get data with given key.
get :: (Storable k, Storable v) => TCIDB -> k -> IO v
get db i = F.c_get (unTCIDB db) (TCS.toInt64 i) >>= 
           fmap TCS.fromString . CS.peekCString

-- | Search with GetMode options.
search :: TCIDB -> String -> [GetMode] -> IO [Int64]
search db query opt = do
  undefined

-- | Search with given query and returns list of id keys.
search2 :: TCIDB -> String -> IO [Int64]
search2 db query = do
  counterP <- FG.new 0 
  query' <- CS.newCString query
  res <- F.c_search2 (unTCIDB db) query' counterP
  numResult <- fromIntegral `fmap` FG.peek counterP
  FG.peekArray numResult res


-- | Delete database, in memory.
del :: TCIDB -> IO ()
del = F.c_del . unTCIDB

-- | Get the last happened error code of an indexed database object.
ecode :: TCIDB -> IO ECODE
ecode db = fmap TCE.cintToError (F.c_ecode $ unTCIDB db)

tune :: TCIDB -> Int64 -> Int64 -> Int64 -> [TuningOption] -> IO Bool
tune db ernum etnum iusuz opts = undefined

setcache :: TCIDB -> Int64 -> Int -> IO Bool
setcache db icsiz lcnum = undefined

setfwmmax :: TCIDB -> Int -> IO Bool
setfwmmax db fwmmax = undefined

-- | Initialize the iterator.
iterinit :: TCIDB -> IO Bool
iterinit = F.c_iterinit . unTCIDB

-- | Get next key for iterator
iternext :: TCIDB -> IO Int64
iternext =  F.c_iternext . unTCIDB

-- | Sync database.
sync :: TCIDB -> IO Bool
sync = F.c_sync . unTCIDB

-- | Optimize database.
optimize :: TCIDB -> IO Bool
optimize = F.c_optimize . unTCIDB

-- | Delete the database from disk. 
vanish :: TCIDB -> IO Bool
vanish = F.c_vanish . unTCIDB

-- | Copy the database to given filepath.
copy :: TCIDB -> FilePath -> IO Bool
copy db path = F.c_copy (unTCIDB db) =<< CS.newCString path

-- | Get filepath of the database
path :: TCIDB -> IO FilePath
path db = F.c_path (unTCIDB db) >>= CS.peekCString

-- | Get number of records in database.
rnum :: TCIDB -> IO Int64
rnum = F.c_rnum . unTCIDB

-- | Get filesize of the database.
fsiz :: TCIDB -> IO Int64
fsiz = F.c_fsiz . unTCIDB
