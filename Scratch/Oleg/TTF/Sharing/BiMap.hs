{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Utility code used in DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/BiMap.hs>
-}
module BiMap
  ( BiMap
  , empty
  , lookup_key
  , lookup_val
  , insert
  , size
  ) where

import qualified Data.Map as M
import qualified Data.IntMap as IM

data BiMap a = BiMap (M.Map a Int) (IM.IntMap a)

instance Show a => Show (BiMap a) where
  show (BiMap _ m) = "BiMap" ++ show (IM.toList m)

lookup_key :: Ord a => a -> BiMap a -> Maybe Int
lookup_key v (BiMap m _) = M.lookup v m

lookup_val :: Int -> BiMap a -> a
lookup_val k (BiMap _ m) = m IM.! k

insert :: Ord a => a -> BiMap a -> (Int,BiMap a)
insert v (BiMap m im) = (k, BiMap m' im') where
  m' = M.insert v k m
  im' = IM.insert k v im
  k = IM.size im
  
empty :: BiMap a  
empty = BiMap M.empty IM.empty

size :: BiMap a -> Int
size (BiMap _ m) = IM.size m