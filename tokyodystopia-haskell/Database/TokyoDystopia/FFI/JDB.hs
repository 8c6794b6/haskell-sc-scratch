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
    -- ** GetMode
    , GetMode(..)
    -- ** Tuning options
    , 
    ) where

import Data.Int (Int64)
import Foreign.C.Types 
    ( CInt )
import Foreign.C.String
    ( CString )

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

#{enum GerMode, GetMode
 , gmSubstr = JDBSSUBSTR
 , gmPrefix = JDBSPREFIX
 , gmSuffix = JDBSSUFFIX
 , gmFull   = JDBSFULL }

