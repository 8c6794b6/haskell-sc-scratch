------------------------------------------------------------------------------
-- |
-- Module      : Database.TokyoDystopia.Internal
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal helper functions .
--

module Database.TokyoDystopia.Internal
    ( bitOr
    , openDB
    ) where

import Data.Bits (Bits, (.|.))
import Foreign ( Ptr )
import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString )
import Database.TokyoDystopia.Types 
    ( OpenMode(..) 
    , GetMode(..)
    , TuningOption(..) )

import qualified Foreign.C.String as CS
-- import qualified Database.TokyoCabinet as TC

-- | Bitwise or for bits.
bitOr :: (Bits a) => [a] -> a
bitOr = foldr (.|.) 0

-- | Helper function for opening database.
openDB :: (Ptr a -> CString -> CInt -> IO Bool) 
       -> (b -> Ptr a) -> (OpenMode -> CInt) 
       -> b -> FilePath -> [OpenMode] -> IO Bool
openDB dbFunc unDB modeFunc db path modes = 
    CS.withCString path $ \path' ->
        dbFunc (unDB db) path' (bitOr $ fmap modeFunc modes)
      
