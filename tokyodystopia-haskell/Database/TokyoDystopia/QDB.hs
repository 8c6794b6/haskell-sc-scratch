------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.QDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCQDB interface.
--

module Database.TokyoDystopia.QDB
    ( QDB()
    , close
    , copy
    , del
    , ecode
    , fsiz
    , new
    , open
    , optimize
    , out
    , path
    , put
    , search
    , setcache
    , setfwmmax
    , sync
    , tnum
    , tune
    , vanish
    ) where

import Data.ByteString ( ByteString )
import Data.Int ( Int64 )
import Foreign ( Ptr )
import qualified Foreign as FG
import qualified Data.ByteString.Char8 as C8
import qualified Foreign.C.String as CS

import Database.TokyoCabinet ( ECODE(..) )
import Database.TokyoCabinet.Storable
    ( Storable )
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS
import Database.TokyoDystopia.Internal ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , TuningOption(..)
    , GetMode(..) )
import qualified Database.TokyoDystopia.FFI.QDB as FQ

-- | Wrapper for TCQDB.
newtype QDB = QDB { unQDB :: Ptr FQ.TCQDB }

-- | Close database.
close :: QDB -> IO Bool
close = FQ.c_close . unQDB

-- | Open database from given path and open modes.
open :: QDB -> FilePath -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- CS.newCString path
  FQ.c_open (unQDB db) path' modes'
    where
      modes' = bitOr $ fmap (FQ.unOpenMode . f) modes
      f OREADER = FQ.omReader
      f OWRITER = FQ.omWriter
      f OCREAT  = FQ.omCreat
      f OTRUNC  = FQ.omTrunc
      f ONOLCK  = FQ.omNolck
      f OLCKNB  = FQ.omLcknb

-- | Delete database from memory.
del :: QDB -> IO ()
del = FQ.c_del . unQDB

-- | Get the last happened error code of database.
ecode :: QDB -> IO ECODE
ecode db = fmap TCE.cintToError (FQ.c_ecode $ unQDB db)

-- | Get file size.
fsiz :: QDB -> IO Int64
fsiz = FQ.c_fsiz . unQDB

-- | Creates new QDB.
new :: IO QDB
new = fmap QDB FQ.c_new

-- | Optimize database.
optimize :: QDB -> IO Bool
optimize = FQ.c_optimize . unQDB

-- | Removes record with given key.
out :: (Storable k) => QDB -> k -> String -> IO Bool
out db key val = do
  val' <- CS.newCString val
  FQ.c_out (unQDB db) (TCS.toInt64 key) val'

-- | Get filepath of the database
path :: QDB -> IO String
path db = FQ.c_path (unQDB db) >>= CS.peekCString

-- | Put data with given key and value.
put :: (Storable k) => QDB -> k -> ByteString -> IO Bool
put db k v = C8.useAsCString v
             (\str -> FQ.c_put (unQDB db) (TCS.toInt64 k) str)

-- | Get the number of token from database.
tnum :: QDB -> IO Int64
tnum = FQ.c_tnum . unQDB

-- | Search phrase with given GetMode.
search :: QDB -> String -> [GetMode] -> IO [Int64]
search db query modes = do
  counterP <- FG.new 0
  query' <- CS.newCString query
  res <- FQ.c_search (unQDB db) query' mode counterP
  numResult <- fromIntegral `fmap` FG.peek counterP
  FG.peekArray numResult res
    where
      mode = bitOr (map (FQ.unGetMode . f) modes)
      f GMSUBSTR = FQ.gmSubstr
      f GMPREFIX = FQ.gmPrefix
      f GMSUFFIX = FQ.gmSuffix
      f GMFULL   = FQ.gmFull
      f _        = FQ.GetMode 0

-- | Set caching parameters. Must be used before opening database.
setcache :: QDB -> Int64 -> Int -> IO Bool
setcache db ic lc  = FQ.c_setcache (unQDB db) ic (fromIntegral lc)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: QDB -> Int -> IO Bool
setfwmmax db fwm = FQ.c_setfwmmax (unQDB db) (fromIntegral fwm)

-- | Sync database.
sync :: QDB -> IO Bool
sync = FQ.c_sync . unQDB

-- | Tune the database. Must be used before opening database.
tune :: QDB -> Int64 -> [TuningOption] -> IO Bool
tune db etnum opts = undefined

-- | Delete the database from disk.
vanish :: QDB -> IO Bool
vanish = FQ.c_vanish . unQDB

-- | Copy the database to given filepath.
copy :: QDB -> FilePath -> IO Bool
copy db path = do
  path' <- CS.newCString path
  FQ.c_copy (unQDB db) path'