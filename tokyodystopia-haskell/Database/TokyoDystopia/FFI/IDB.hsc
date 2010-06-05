{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}

------------------------------------------------------------------------------
-- | 
-- Inner guts of haskell binding for dystopia.h.
-- 
module Database.TokyoDystopia.FFI.IDB
    ( 
    -- * Types and constants  

    -- ** Database
      TCIDB

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
    , gmToken
    , gmTokPre
    , gmTokSuf

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
    , c_get
    , c_iterinit
    , c_iternext
    , c_new
    , c_open
    , c_optimize
    , c_out
    , c_path
    , c_put
    , c_rnum
    , c_search
    , c_search2
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

#include <dystopia.h>

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

data TCIDB

------------------------------------------------------------------------------
-- 
-- Enums 
-- 
------------------------------------------------------------------------------

#{enum OpenMode, OpenMode
 , omReader = IDBOREADER
 , omWriter = IDBOWRITER
 , omCreate = IDBOCREAT
 , omTrunc  = IDBOTRUNC
 , omNolck  = IDBONOLCK
 , omLcknb  = IDBOLCKNB }

#{enum GetMode, GetMode
 , gmSubstr = IDBSSUBSTR
 , gmPrefix = IDBSPREFIX
 , gmSuffix = IDBSSUFFIX
 , gmFull = IDBSFULL
 , gmToken = IDBSTOKEN
 , gmTokPre = IDBSTOKPRE
 , gmTokSuf = IDBSTOKSUF }

#{enum TuningOption, TuningOption
 , toLarge = IDBTLARGE
 , toDeflate = IDBTDEFLATE
 , toBzip = IDBTBZIP
 , toTcbs = IDBTTCBS }

------------------------------------------------------------------------------
-- 
-- Function calls
-- 
------------------------------------------------------------------------------

foreign import ccall "dystopia.h tcidberrmsg"
        c_errmsg :: CInt -> CString

foreign import ccall "dystopia.h tcidbnew"
        c_new :: IO (Ptr TCIDB)

foreign import ccall "dystopia.h tcidbdel"
        c_del :: Ptr TCIDB -> IO ()

foreign import ccall "dystopia.h tcidbecode"
        c_ecode :: Ptr TCIDB -> IO CInt

foreign import ccall "dystopia.h tcidbtune"
        c_tune :: Ptr TCIDB -> Int64 -> Int64 -> Int64 -> CUInt -> IO Bool

foreign import ccall "dystopia.h tcidbsetcache"
        c_setcache :: Ptr TCIDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "dystopia.h tcidbsetfwmmax"
        c_setfwmmax :: Ptr TCIDB -> Int32 -> IO Bool

foreign import ccall "dystopia.h tcidbopen"
        c_open :: Ptr TCIDB -> CString -> CInt -> IO Bool

foreign import ccall "dystopia.h tcidbclose"
        c_close :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbput"
        c_put :: Ptr TCIDB -> Int64 -> CString -> IO Bool

foreign import ccall "dystopia.h tcidbout"
        c_out :: Ptr TCIDB -> Int64 -> IO Bool

foreign import ccall "dystopia.h tcidbget"
        c_get :: Ptr TCIDB -> Int64 -> IO CString

foreign import ccall "dystopia.h tcidbsearch"
        c_search :: Ptr TCIDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "dystopia.h tcidbsearch2"
        c_search2 :: Ptr TCIDB -> CString -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "dystopia.h tcidbiterinit"
        c_iterinit :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbiternext"
        c_iternext :: Ptr TCIDB -> IO Int64

foreign import ccall "dystopia.h tcidbsync"
        c_sync :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidboptimize"
        c_optimize :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbvanish"
        c_vanish :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbcopy"
        c_copy :: Ptr TCIDB -> CString -> IO Bool

foreign import ccall "dystopia.h tcidbpath"
        c_path :: Ptr TCIDB -> IO CString

foreign import ccall "dystopia.h tcidbrnum"
        c_rnum :: Ptr TCIDB -> IO Int64

foreign import ccall "dystopia.h tcidbfsiz"
        c_fsiz :: Ptr TCIDB -> IO Int64
