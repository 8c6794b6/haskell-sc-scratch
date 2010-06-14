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
    , new
    , del
    , ecode
    , tune
    , setcache
    , setfwmmax
    , cnum
    , open
    , close
    , put
    , put2
    , out
    , out2
    , search
    , optimize
    , vanish
    , copy
    , path
    , tnum
    , fsiz
    , sync
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
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS

import Database.TokyoDystopia.Internal ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , TuningOption(..) )
import qualified Database.TokyoDystopia.FFI.WDB as FW
import qualified Database.TokyoDystopia.Internal as I

-- | Wrapper for TCWDB.
newtype WDB = WDB { unWDB :: Ptr FW.TCWDB }

-- | Close database.
close :: WDB -> IO Bool
close = FW.c_close . unWDB

-- | Open database from given path and open modes.
open :: WDB -> FilePath -> [OpenMode] -> IO Bool
open = I.mkOpen FW.c_open unWDB (FW.unOpenMode . f)
    where
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

-- | Removes record with specifying delimiter.
out2 :: (Storable k) 
     => WDB         -- ^ WDB database
     -> k           -- ^ Key for the record
     -> ByteString  -- ^ Deleting values separated with delimeter
     -> ByteString  -- ^ The delimeter
     -> IO Bool
out2 db k vs v = 
  C8.useAsCString vs $ \vs' ->
    C8.useAsCString v $ \v' ->
      FW.c_out2 (unWDB db) (TCS.toInt64 k) vs' v'

-- | Get filepath of the database
path :: WDB -> IO String
path db = FW.c_path (unWDB db) >>= CS.peekCString

-- | Put data with given key and value.
put :: (Storable k) => WDB -> k -> List ByteString -> IO Bool
put db k vs = do
  withForeignPtr (unTCList vs) (\v -> FW.c_put (unWDB db) (TCS.toInt64 k) v)

-- | Put with specifying delimiter.
put2 :: (Storable k) 
     => WDB         -- ^ JDB database
     -> k           -- ^ Key for the record
     -> ByteString  -- ^ Value separated by delimiter
     -> ByteString  -- ^ Delimiter
     -> IO Bool
put2 db k vs v = do
  C8.useAsCString vs $ \vs' ->
    C8.useAsCString v $ \v' ->
      FW.c_put2 (unWDB db) (TCS.toInt64 k) vs' v'

-- | Get the number of token from database.
tnum :: WDB -> IO Int64
tnum = FW.c_tnum . unWDB

-- | Search phrase with given GetMode.
search :: WDB -> String -> IO [Int64]
search db query = do
  FG.with 0 $ \counterP ->
    CS.withCString query $ \query' -> do
       res <- FW.c_search (unWDB db) query' counterP
       numResult <- fromIntegral `fmap` FG.peek counterP
       res' <- FG.peekArray numResult res
       FG.free res
       return res'

-- | Set caching parameters. Must be used before opening database.
setcache :: WDB -> Int64 -> Int -> IO Bool
setcache db ic lc  = FW.c_setcache (unWDB db) ic (fromIntegral lc)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: WDB -> Int -> IO Bool
setfwmmax db fwm = FW.c_setfwmmax (unWDB db) (fromIntegral fwm)

-- | Get the number of chunks
cnum :: WDB -> IO Int
cnum db = FW.c_cnum (unWDB db) >>= return . fromIntegral

-- | Sync database.
sync :: WDB -> IO Bool
sync = FW.c_sync . unWDB

-- | Tune the database. Must be used before opening database.
tune :: WDB -> Int64 -> [TuningOption] -> IO Bool
tune db etnum opts = FW.c_tune (unWDB db) etnum opts'
  where
    opts' = fromIntegral . bitOr $ map (FW.unTuningOption . f) opts
    f TLARGE   = FW.toLarge
    f TDEFLATE = FW.toDeflate
    f TBZIP    = FW.toBzip
    f TTCBS    = FW.toTcbs
    f _        = FW.TuningOption 0

-- | Delete the database from disk.
vanish :: WDB -> IO Bool
vanish = FW.c_vanish . unWDB

-- | Copy the database to given filepath.
copy :: WDB -> FilePath -> IO Bool
copy db file = do
  file' <- CS.newCString file
  FW.c_copy (unWDB db) file'