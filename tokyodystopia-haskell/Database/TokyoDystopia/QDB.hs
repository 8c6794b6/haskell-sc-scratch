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
    , del
    , ecode
    , fsiz
    , new
    , open
    , optimize
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

import Data.Int ( Int64 )
import Foreign ( Ptr )
import qualified Foreign.C.String as CS

import Database.TokyoCabinet ( ECODE(..) )
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.Error as TCE
import Database.TokyoDystopia.Internal ( bitOr )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , TuningOption(..)
    , GetMode(..) )
import qualified Database.TokyoDystopia.FFI.QDB as FQ

-- | Newtype wrapper for QDB FFI binding.
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
      f OCREAT  = FQ.omCreate
      f OTRUNC  = FQ.omTrunc
      f ONOLCK  = FQ.omNolck
      f OLCKNB  = FQ.omLcknb

-- | Delete database from memory.
del :: QDB -> IO ()
del = FQ.c_del . unQDB

-- | Get the last happened error code of database.
ecode :: QDB -> IO ECODE
ecode db = fmap TCE.cintToError (FQ.c_ecode $ unQDB db)

fsiz = undefined

-- | Creates new QDB.
new :: IO QDB
new = fmap QDB FQ.c_new 

-- | Optimize database.
optimize :: QDB -> IO Bool
optimize = FQ.c_optimize . unQDB

-- | Get filepath of the database
path :: QDB -> IO String
path db = FQ.c_path (unQDB db) >>= CS.peekCString 

-- | Put data with given key and value.
put :: QDB -> Int64 -> String -> IO Bool
put db k v = undefined

tnum = undefined

search = undefined

setcache = undefined

setfwmmax = undefined

sync = undefined

tune = undefined

vanish = undefined