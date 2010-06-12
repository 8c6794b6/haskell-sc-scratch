{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}

------------------------------------------------------------------------------
-- | 
-- Inner guts of haskell binding for tcwdb.h.
-- 
module Database.TokyoDystopia.FFI.WDB
    ( 
    -- * Types and constants  

    -- ** Database
      TCWDB

    -- ** OpenMode
    , OpenMode(..)
    , omReader
    , omWriter
    , omCreat
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

#include <tcwdb.h>

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

data TCWDB

------------------------------------------------------------------------------
-- 
-- Enums 
-- 
------------------------------------------------------------------------------

#{enum OpenMode, OpenMode
 , omReader = QDBOREADER
 , omWriter = QDBOWRITER
 , omCreat = QDBOCREAT
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

foreign import ccall "tcwdb.h tcwdberrmsg"
        c_errmsg :: CInt -> CString

foreign import ccall "tcwdb.h tcwdbnew"
        c_new :: IO (Ptr TCWDB)

foreign import ccall "tcwdb.h tcwdbdel"
        c_del :: Ptr TCWDB -> IO ()

foreign import ccall "tcwdb.h tcwdbecode"
        c_ecode :: Ptr TCWDB -> IO CInt

foreign import ccall "tcwdb.h tcwdbtune"
        c_tune :: Ptr TCWDB -> Int64 -> CUInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbsetcache"
        c_setcache :: Ptr TCWDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "tcwdb.h tcwdbsetfwmmax"
        c_setfwmmax :: Ptr TCWDB -> Int32 -> IO Bool

foreign import ccall "tcwdb.h tcwdbopen"
        c_open :: Ptr TCWDB -> CString -> CInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbclose"
        c_close :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbput"
        c_put :: Ptr TCWDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbput2"
        c_put2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout"
        c_out :: Ptr TCWDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout2"
        c_out2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbsearch"
        c_search :: Ptr TCWDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcwdb.h tcwdbsync"
        c_sync :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdboptimize"
        c_optimize :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbvanish"
        c_vanish :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbcopy"
        c_copy :: Ptr TCWDB -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbpath"
        c_path :: Ptr TCWDB -> IO CString

foreign import ccall "tcwdb.h tcwdbtnum"
        c_tnum :: Ptr TCWDB -> IO Int64

foreign import ccall "tcwdb.h tcwdbfsiz"
        c_fsiz :: Ptr TCWDB -> IO Int64
