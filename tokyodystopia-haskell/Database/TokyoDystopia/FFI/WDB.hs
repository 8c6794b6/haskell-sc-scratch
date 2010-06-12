{-# INCLUDE <tcwdb.h> #-}
{-# LINE 1 "Database/TokyoDystopia/FFI/WDB.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface,
{-# LINE 2 "Database/TokyoDystopia/FFI/WDB.hsc" #-}
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

import Data.Int ( Int32, Int64 )
import Foreign ( Ptr )
import Foreign.C.Types ( CInt, CUInt )
import Foreign.C.String ( CString )

import Database.TokyoCabinet.List ( List )
import Database.TokyoCabinet.List.C ( LIST )


{-# LINE 70 "Database/TokyoDystopia/FFI/WDB.hsc" #-}

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

omReader  :: OpenMode
omReader  = OpenMode 1
omWriter  :: OpenMode
omWriter  = OpenMode 2
omCreat  :: OpenMode
omCreat  = OpenMode 4
omTrunc   :: OpenMode
omTrunc   = OpenMode 8
omNolck   :: OpenMode
omNolck   = OpenMode 16
omLcknb   :: OpenMode
omLcknb   = OpenMode 32

{-# LINE 101 "Database/TokyoDystopia/FFI/WDB.hsc" #-}

gmSubstr  :: GetMode
gmSubstr  = GetMode 0
gmPrefix  :: GetMode
gmPrefix  = GetMode 1
gmSuffix  :: GetMode
gmSuffix  = GetMode 2
gmFull  :: GetMode
gmFull  = GetMode 3

{-# LINE 107 "Database/TokyoDystopia/FFI/WDB.hsc" #-}

toLarge  :: TuningOption
toLarge  = TuningOption 1
toDeflate  :: TuningOption
toDeflate  = TuningOption 2
toBzip  :: TuningOption
toBzip  = TuningOption 4
toTcbs  :: TuningOption
toTcbs  = TuningOption 8

{-# LINE 113 "Database/TokyoDystopia/FFI/WDB.hsc" #-}

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

foreign import ccall "tcwdb.h tcwdbcnum"
        c_cnum :: Ptr TCWDB -> IO CUInt

foreign import ccall "tcwdb.h tcwdbopen"
        c_open :: Ptr TCWDB -> CString -> CInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbclose"
        c_close :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbput"
        c_put :: Ptr TCWDB -> Int64 -> Ptr LIST -> IO Bool

foreign import ccall "tcwdb.h tcwdbput2"
        c_put2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout"
        c_out :: Ptr TCWDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout2"
        c_out2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbsearch"
        c_search :: Ptr TCWDB -> CString -> Ptr CInt -> IO (Ptr Int64)

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
