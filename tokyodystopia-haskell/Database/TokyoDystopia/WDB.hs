------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.WDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCWDB interface.
--

module Database.TokyoDystopia.WDB
    ( WDB()
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
import Foreign ( Ptr, withForeignPtr )
import qualified Foreign as FG
import qualified Data.ByteString.Char8 as C8
import qualified Foreign.C.String as CS

import Database.TokyoCabinet ( ECODE(..) )
import Database.TokyoCabinet.List.C ( List(..) )
import Database.TokyoCabinet.Storable ( Storable )
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.List as TCL
import qualified Database.TokyoCabinet.Storable as TCS

import Database.TokyoDystopia.Internal ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , TuningOption(..)
    , GetMode(..) )
import qualified Database.TokyoDystopia.FFI.WDB as FW

-- | Wrapper for TCWDB.
newtype WDB = WDB { unWDB :: Ptr FW.TCWDB }

-- | Close database.
close :: WDB -> IO Bool
close = FW.c_close . unWDB

-- | Open database from given path and open modes.
open :: WDB -> FilePath -> [OpenMode] -> IO Bool
open db path modes = do
  path' <- CS.newCString path
  FW.c_open (unWDB db) path' modes'
    where
      modes' = bitOr $ fmap (FW.unOpenMode . f) modes
      f OREADER = FW.omReader
      f OWRITER = FW.omWriter
      f OCREAT  = FW.omCreat
      f OTRUNC  = FW.omTrunc
      f ONOLCK  = FW.omNolck
      f OLCKNB  = FW.omLcknb

-- | Delete database from memory.
del :: WDB -> IO ()
del = FW.c_del . unWDB

-- | Get the last happened error code of database.
ecode :: WDB -> IO ECODE
ecode db = fmap TCE.cintToError (FW.c_ecode $ unWDB db)

-- | Get file size.
fsiz :: WDB -> IO Int64
fsiz = FW.c_fsiz . unWDB

-- | Creates new WDB.
new :: IO WDB
new = fmap WDB FW.c_new

-- | Optimize database.
optimize :: WDB -> IO Bool
optimize = FW.c_optimize . unWDB

-- | Removes record with given key.
out :: (Storable k) => WDB -> k -> String -> IO Bool
out db key val = do
  val' <- CS.newCString val
  FW.c_out (unWDB db) (TCS.toInt64 key) val'

-- | Get filepath of the database
path :: WDB -> IO String
path db = FW.c_path (unWDB db) >>= CS.peekCString

-- | Put data with given key and value.
put :: (Storable k) => WDB -> k -> List ByteString -> IO Bool
put db k vs = do
  withForeignPtr (unTCList vs) (\v -> FW.c_put (unWDB db) (TCS.toInt64 k) v)

-- | Get the number of token from database.
tnum :: WDB -> IO Int64
tnum = FW.c_tnum . unWDB

-- | Search phrase with given GetMode.
search :: WDB -> String -> IO [Int64]
search db query = do
  counterP <- FG.new 0
  query' <- CS.newCString query
  res <- FW.c_search (unWDB db) query' counterP
  numResult <- fromIntegral `fmap` FG.peek counterP
  FG.peekArray numResult res

-- | Set caching parameters. Must be used before opening database.
setcache :: WDB -> Int64 -> Int -> IO Bool
setcache db ic lc  = FW.c_setcache (unWDB db) ic (fromIntegral lc)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: WDB -> Int -> IO Bool
setfwmmax db fwm = FW.c_setfwmmax (unWDB db) (fromIntegral fwm)

-- | Sync database.
sync :: WDB -> IO Bool
sync = FW.c_sync . unWDB

-- | Tune the database. Must be used before opening database.
tune :: WDB -> Int64 -> [TuningOption] -> IO Bool
tune db etnum opts = undefined

-- | Delete the database from disk.
vanish :: WDB -> IO Bool
vanish = FW.c_vanish . unWDB

-- | Copy the database to given filepath.
copy :: WDB -> FilePath -> IO Bool
copy db path = do
  path' <- CS.newCString path
  FW.c_copy (unWDB db) path'