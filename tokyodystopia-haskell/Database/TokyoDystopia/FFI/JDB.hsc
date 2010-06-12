{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}

------------------------------------------------------------------------------
-- |
-- Inner guts of haskell binding for laputa.h.
--
module Database.TokyoDystopia.FFI.JDB
    ( -- * Types and constants
      TCJDB

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
    , gmSuffix
    , gmPrefix
    , gmSubstr
    , gmFull

    -- ** Tuning options
    , TuningOption(..)
    , toLarge
    , toDeflate
    , toBzip
    , toTcbs

    -- * C functions
    , c_errmsg
    , c_new
    , c_del
    , c_ecode
    , c_tune
    , c_setcache
    , c_setfwmmax
    , c_open
    , c_close
    , c_put
    , c_put2
    , c_out
    , c_get
    , c_get2
    , c_search
    , c_search2
    , c_iterinit
    , c_iternext
    , c_sync
    , c_optimize
    , c_vanish
    , c_copy
    , c_path
    , c_rnum
    , c_fsiz

    ) where

import Data.Int ( Int32, Int64 )
import Foreign ( Ptr )
import Foreign.C.Types ( CInt , CUInt )
import Foreign.C.String ( CString )

import Database.TokyoCabinet.List.C ( LIST )

#include <laputa.h>

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

data TCJDB


#{enum OpenMode, OpenMode
 , omReader = JDBOREADER
 , omWriter = JDBOWRITER
 , omCreat  = JDBOCREAT
 , omTrunc  = JDBOTRUNC
 , omNolck  = JDBONOLCK
 , omLcknb  = JDBOLCKNB }

#{enum GetMode, GetMode
 , gmSubstr = JDBSSUBSTR
 , gmPrefix = JDBSPREFIX
 , gmSuffix = JDBSSUFFIX
 , gmFull   = JDBSFULL }

#{enum TuningOption, TuningOption
 , toLarge   = JDBTLARGE
 , toDeflate = JDBTDEFLATE
 , toBzip    = JDBTBZIP
 , toTcbs    = JDBTTCBS }


------------------------------------------------------------------------------
--
-- Function calls
--
------------------------------------------------------------------------------

foreign import ccall "laputa.h tcjdberrmsg"
        c_errmsg :: CInt -> CString

foreign import ccall "laputa.h tcjdbnew"
        c_new :: IO (Ptr TCJDB)

foreign import ccall "laputa.h tcjdbdel"
        c_del :: Ptr TCJDB -> IO ()

foreign import ccall "laputa.h tcjdbecode"
        c_ecode :: Ptr TCJDB -> IO CInt

foreign import ccall "laputa.h tcjdbtune"
        c_tune :: Ptr TCJDB -> Int64 -> Int64 -> Int64 -> CUInt -> IO Bool

foreign import ccall "laputa.h tcjdbsetcache"
        c_setcache :: Ptr TCJDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "laputa.h tcjdbsetfwmmax"
        c_setfwmmax :: Ptr TCJDB -> Int32 -> IO Bool

foreign import ccall "laputa.h tcjdbopen"
        c_open :: Ptr TCJDB -> CString -> CInt -> IO Bool

foreign import ccall "laputa.h tcjdbclose"
        c_close :: Ptr TCJDB -> IO Bool

foreign import ccall "laputa.h tcjdbput"
        c_put :: Ptr TCJDB -> Int64 -> Ptr LIST -> IO Bool

foreign import ccall "laputa.h tcjdbput2"
        c_put2 :: Ptr TCJDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "laputa.h tcjdbout"
        c_out :: Ptr TCJDB -> Int64 -> IO Bool

foreign import ccall "laputa.h tcjdbget"
        c_get :: Ptr TCJDB -> Int64 -> IO (Ptr LIST)

foreign import ccall "laputa.h tcjdbget2"
        c_get2 :: Ptr TCJDB -> Int64 -> IO CString

foreign import ccall "laputa.h tcjdbsearch"
        c_search :: Ptr TCJDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "laputa.h tcjdbsearch2"
        c_search2 :: Ptr TCJDB -> CString -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "laputa.h tcjdbiterinit"
        c_iterinit :: Ptr TCJDB -> IO Bool

foreign import ccall "laputa.h tcjdbiternext"
        c_iternext :: Ptr TCJDB -> IO Int64

foreign import ccall "laputa.h tcjdbsync"
        c_sync :: Ptr TCJDB -> IO Bool

foreign import ccall "laputa.h tcjdboptimize"
        c_optimize :: Ptr TCJDB -> IO Bool

foreign import ccall "laputa.h tcjdbvanish"
        c_vanish :: Ptr TCJDB -> IO Bool

foreign import ccall "laputa.h tcjdbcopy"
        c_copy :: Ptr TCJDB -> CString -> IO Bool

foreign import ccall "laputa.h tcjdbpath"
        c_path :: Ptr TCJDB -> IO CString

foreign import ccall "laputa.h tcjdbrnum"
        c_rnum :: Ptr TCJDB -> IO Int64

foreign import ccall "laputa.h tcjdbfsiz"
        c_fsiz :: Ptr TCJDB -> IO Int64
