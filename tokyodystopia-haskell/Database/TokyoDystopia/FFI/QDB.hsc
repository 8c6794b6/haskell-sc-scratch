{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}

------------------------------------------------------------------------------
-- | 
-- Inner guts of haskell binding for tcqdb.h.
-- 
module Database.TokyoDystopia.FFI.QDB
    ( 
    -- * Types and constants  

    -- ** Database
      TCQDB

    -- ** OpenMode
    , OpenMode(..)
    , omReader
    , omWriter
    , omCreate
    , omTrunc
    , omNolck
    , omLcknb

    -- ** GetMode
    , GetMode(..)
    , gmSubstr
    , gmPrefix
    , gmSuffix
    , gmFull

    -- ** Tuning options
    , TuningOption(..)
    , toLarge
    , toDeflate
    , toBzip
    , toTcbs

    -- * C Functions
    , c_close
    , c_copy
    , c_del
    , c_ecode
    , c_errmsg
    , c_fsiz
    , c_new
    , c_open
    , c_optimize
    , c_out
    , c_path
    , c_put
    , c_tnum
    , c_search
    , c_setcache
    , c_setfwmmax
    , c_sync
    , c_tune
    , c_vanish
    ) where

import Data.Word
import Data.Int
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <tcqdb.h>

------------------------------------------------------------------------------
-- 
-- Haskell Side Datatype definitions
--
------------------------------------------------------------------------------

data OpenMode = OpenMode { unOpenMode :: CInt }
              deriving (Eq, Show)

data GetMode = GetMode { unGetMode :: CInt }
             deriving (Eq, Show)

data TuningOption = TuningOption { unTuningOption :: CInt }
               deriving (Eq, Show)

data TCQDB

------------------------------------------------------------------------------
-- 
-- Enums 
-- 
------------------------------------------------------------------------------

#{enum OpenMode, OpenMode
 , omReader = QDBOREADER
 , omWriter = QDBOWRITER
 , omCreate = QDBOCREAT
 , omTrunc  = QDBOTRUNC
 , omNolck  = QDBONOLCK
 , omLcknb  = QDBOLCKNB }

#{enum GetMode, GetMode
 , gmSubstr = QDBSSUBSTR
 , gmPrefix = QDBSPREFIX
 , gmSuffix = QDBSSUFFIX
 , gmFull = QDBSFULL }

#{enum TuningOption, TuningOption
 , toLarge = QDBTLARGE
 , toDeflate = QDBTDEFLATE
 , toBzip = QDBTBZIP
 , toTcbs = QDBTTCBS }

------------------------------------------------------------------------------
-- 
-- Function calls
-- 
------------------------------------------------------------------------------

foreign import ccall "tcqdb.h tcqdberrmsg"
        c_errmsg :: CInt -> CString

foreign import ccall "tcqdb.h tcqdbnew"
        c_new :: IO (Ptr TCQDB)

foreign import ccall "tcqdb.h tcqdbdel"
        c_del :: Ptr TCQDB -> IO ()

foreign import ccall "tcqdb.h tcqdbecode"
        c_ecode :: Ptr TCQDB -> IO CInt

foreign import ccall "tcqdb.h tcqdbtune"
        c_tune :: Ptr TCQDB -> Int64 -> CUInt -> IO Bool

foreign import ccall "tcqdb.h tcqdbsetcache"
        c_setcache :: Ptr TCQDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "tcqdb.h tcqdbsetfwmmax"
        c_setfwmmax :: Ptr TCQDB -> Int32 -> IO Bool

foreign import ccall "tcqdb.h tcqdbopen"
        c_open :: Ptr TCQDB -> CString -> CInt -> IO Bool

foreign import ccall "tcqdb.h tcqdbclose"
        c_close :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbput"
        c_put :: Ptr TCQDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbout"
        c_out :: Ptr TCQDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbsearch"
        c_search :: Ptr TCQDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcqdb.h tcqdbsync"
        c_sync :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdboptimize"
        c_optimize :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbvanish"
        c_vanish :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbcopy"
        c_copy :: Ptr TCQDB -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbpath"
        c_path :: Ptr TCQDB -> IO CString

foreign import ccall "tcqdb.h tcqdbtnum"
        c_tnum :: Ptr TCQDB -> IO Int64

foreign import ccall "tcqdb.h tcqdbfsiz"
        c_fsiz :: Ptr TCQDB -> IO Int64
