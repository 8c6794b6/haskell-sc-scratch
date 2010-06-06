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
    ( IDB()
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

import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Int ( Int64 )
import Foreign ( Ptr, maybePeek )
import qualified Foreign.ForeignPtr as FF
import qualified Foreign as FG
import qualified Foreign.C.Types as CT
import qualified Foreign.C.String as CS
import Database.TokyoCabinet.Storable ( Storable )
import Database.TokyoCabinet ( ECODE(..) ) 
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS

import Database.TokyoDystopia.Internal
    ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )
import qualified Database.TokyoDystopia.FFI.IDB as F
import qualified Database.TokyoDystopia.Internal as I

-- | Wrapper for TCIDB.
newtype IDB = IDB { unIDB :: Ptr F.TCIDB }

-- | Creates new IDB.
new :: IO IDB
new = IDB `fmap` F.c_new

-- | Open database from given path and open modes.
open :: IDB -> FilePath -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- CS.newCString path
  let mode = F.OpenMode . bitOr $ fmap (F.unOpenMode . f) modes
  F.c_open (unIDB db) path' (F.unOpenMode mode)
    where
      f OREADER = F.omReader
      f OWRITER = F.omWriter
      f OCREAT  = F.omCreate
      f OTRUNC  = F.omTrunc
      f ONOLCK  = F.omNolck
      f OLCKNB  = F.omLcknb

-- | Closes database
close :: IDB -> IO Bool
close = F.c_close . unIDB

-- | Put data with given key and value.
put :: (Storable k) => IDB -> k -> ByteString -> IO Bool
put db k v = C8.useAsCString v
             (\str -> F.c_put (unIDB db) (TCS.toInt64 k) str)

-- | Get data with given key.
get :: (Storable k, Storable v) => IDB -> k -> IO (Maybe v)
get db i = do
  val <- F.c_get (unIDB db) (TCS.toInt64 i)
  str <- maybePeek CS.peekCString val
  return $ fmap TCS.fromString str

-- | Search with GetMode options.
search :: IDB -> String -> [GetMode] -> IO [Int64]
search db query opt = do
  undefined

-- | Search with given query and returns list of id keys.
search2 :: IDB -> String -> IO [Int64]
search2 db query = do
  counterP <- FG.new 0 
  query' <- CS.newCString query
  res <- F.c_search2 (unIDB db) query' counterP
  numResult <- fromIntegral `fmap` FG.peek counterP
  FG.peekArray numResult res

-- | Delete database, from memory.
del :: IDB -> IO ()
del = F.c_del . unIDB

-- | Get the last happened error code of an indexed database object.
ecode :: IDB -> IO ECODE
ecode db = fmap TCE.cintToError (F.c_ecode $ unIDB db)

-- | Tune the database. Must be used before opening database.
tune :: IDB -> Int64 -> Int64 -> Int64 -> [TuningOption] -> IO Bool
tune db ernum etnum iusuz opts = 
    F.c_tune (unIDB db) ernum etnum iusuz opts'
    where
      opts' = fromIntegral $ I.bitOr $ map (F.unTuningOption . f) opts
      f TLARGE   = F.toLarge
      f TDEFLATE = F.toDeflate
      f TBZIP    = F.toBzip
      f TTCBS    = F.toTcbs

-- | Set caching parameters. Must be used before opening database.
setcache :: IDB -> Int64 -> Int -> IO Bool
setcache db icsiz lcnum = F.c_setcache (unIDB db) icsiz (fromIntegral lcnum)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: IDB -> Int -> IO Bool
setfwmmax db fwmmax = F.c_setfwmmax (unIDB db) (fromIntegral fwmmax)

-- | Initialize the iterator.
iterinit :: IDB -> IO Bool
iterinit = F.c_iterinit . unIDB

-- | Get next key for iterator
iternext :: IDB -> IO Int64
iternext =  F.c_iternext . unIDB

-- | Sync database.
sync :: IDB -> IO Bool
sync = F.c_sync . unIDB

-- | Optimize database.
optimize :: IDB -> IO Bool
optimize = F.c_optimize . unIDB

-- | Removes record with given key
out :: (Storable k) => IDB -> k -> IO Bool
out db key = F.c_out (unIDB db) (TCS.toInt64 key) 

-- | Delete the database from disk. 
vanish :: IDB -> IO Bool
vanish = F.c_vanish . unIDB

-- | Copy the database to given filepath.
copy :: IDB -> FilePath -> IO Bool
copy db path = F.c_copy (unIDB db) =<< CS.newCString path

-- | Get filepath of the database
path :: IDB -> IO FilePath
path db = F.c_path (unIDB db) >>= CS.peekCString

-- | Get number of records in database.
rnum :: IDB -> IO Int64
rnum = F.c_rnum . unIDB

-- | Get filesize of the database.
fsiz :: IDB -> IO Int64
fsiz = F.c_fsiz . unIDB
