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
    , mkOpen
    , mkSearch
    , mkSearch2
    ) where

import Data.Bits (Bits, (.|.))
import Foreign ( Ptr, Storable )
import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )
import qualified Foreign as FG
import qualified Foreign.C.String as CS
-- import qualified Database.TokyoCabinet as TC

-- | Bitwise or for bits.
bitOr :: (Bits a) => [a] -> a
bitOr = foldr (.|.) 0


-- | Helper function for opening database.
mkOpen :: (Ptr a -> CString -> CInt -> IO Bool)
       -> (b -> Ptr a) 
       -> (OpenMode -> CInt)
       -> b 
       -> FilePath -> [OpenMode] -> IO Bool
mkOpen dbFunc unDB modeFunc = \db path modes ->
    CS.withCString path $ \path' ->
        dbFunc (unDB db) path' (bitOr $ fmap modeFunc modes)


-- | Helper function for searching database.
mkSearch :: (Integral a, Storable a, Storable a1)
         => (db -> CString -> t3 -> Ptr a -> IO (Ptr a1))
         -> (t -> db)
         -> (t1 -> t3)
         -> t
         -> String
         -> t1
         -> IO [a1]
mkSearch searchFunc unDB modeFunc db query modes =
  FG.with 0 $ \counterP ->
    CS.withCString query $ \query' -> do
      res <- searchFunc (unDB db) query' (modeFunc modes) counterP
      numResult <- fromIntegral `fmap` FG.peek counterP
      res' <- FG.peekArray numResult res
      FG.free res
      return res'

mkSearch2 searchFunc unDB = \db query -> do
  FG.with 0 $ \counterP ->
    CS.withCString query $ \query' -> do
      res <- searchFunc (unDB db) query' counterP
      numResult <- fromIntegral `fmap` FG.peek counterP
      res' <- FG.peekArray numResult res
      FG.free res
      return res'
