{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}

------------------------------------------------------------------------------
-- | 
-- Inner guts of haskell binding for dystopia.h.
-- 
module Database.TokyoDystopia.FFI.TCI where

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

------------------------------------------------------------------------------
-- 
-- Function calls
-- 
------------------------------------------------------------------------------

foreign import ccall "dystopia.h tcidberrmsg"
        tcidberrmsg :: CInt -> CString

foreign import ccall "dystopia.h tcidbnew"
        tcidbnew :: IO (Ptr TCIDB)

foreign import ccall "dystopia.h tcidbdel"
        tcidbdel :: Ptr TCIDB -> IO ()

foreign import ccall "dystopia.h tcidbecode"
        tcidbecode :: Ptr TCIDB -> IO CInt

foreign import ccall "dystopia.h tcidbtune"
        tcidbtune :: Ptr TCIDB -> Int64 -> Int64 -> Int64 -> CUInt -> IO Bool

foreign import ccall "dystopia.h tcidbsetcache"
        tcidbsetcache :: Ptr TCIDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "dystopia.h tcidbsetfwmmax"
        tcidbsetfwmmax :: Ptr TCIDB -> CString -> CInt -> IO Bool

foreign import ccall "dystopia.h tcidbopen"
        tcidbopen :: Ptr TCIDB -> CString -> CInt -> IO Bool

foreign import ccall "dystopia.h tcidbclose"
        tcidbclose :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbput"
        tcidbput :: Ptr TCIDB -> Int64 -> CString -> IO Bool

foreign import ccall "dystopia.h tcidbout"
        tcidbout :: Ptr TCIDB -> Int64 -> IO Bool

foreign import ccall "dystopia.h tcidbget"
        tcidbget :: Ptr TCIDB -> Int64 -> IO CString

foreign import ccall "dystopia.h tcidbsearch"
        tcidbsearch :: Ptr TCIDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "dystopia.h tcidbsearch2"
        tcidbsearch2 :: Ptr TCIDB -> CString -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "dystopia.h tcidbiterinit"
        tcidbiterinit :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbiternext"
        tcidbiternext :: Ptr TCIDB -> IO Int64

foreign import ccall "dystopia.h tcidbsync"
        tcidbsync :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidboptimize"
        tcidboptimize :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbvanish"
        tcidbvanish :: Ptr TCIDB -> IO Bool

foreign import ccall "dystopia.h tcidbcopy"
        tcidbcopy :: Ptr TCIDB -> CString -> IO Bool

foreign import ccall "dystopia.h tcidbpath"
        tcidbpath :: Ptr TCIDB -> IO CString

foreign import ccall "dystopia.h tcidbrnum"
        tcidbrnum :: Ptr TCIDB -> IO Int64

foreign import ccall "dystopia.h tcidbfsiz"
        tcidbfsiz :: Ptr TCIDB -> IO Int64
