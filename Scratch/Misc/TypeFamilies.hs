{-# LANGUAGE TypeFamilies #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module TypeFamilies where

import Prelude hiding (lookup)
import qualified Data.Char as CH
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

data family XList a

data instance XList Char = XCons !Char !(XList Char) | XNil

data instance XList () = XListUnit !Int

class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v        = GMapInt (IM.IntMap v)
  empty                  = GMapInt IM.empty
  lookup k (GMapInt m)   = IM.lookup k m
  insert k v (GMapInt m) = GMapInt (IM.insert k v m)

instance GMapKey Char where
  data GMap Char v        = GMapChar (IM.IntMap v)
  empty                   = GMapChar IM.empty
  lookup k (GMapChar m)   = IM.lookup (CH.ord k) m
  insert k v (GMapChar m) = GMapChar (IM.insert (CH.ord k) v m)

instance GMapKey () where
  data GMap () v           = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup () (GMapUnit v)   = v
  insert () v (GMapUnit _) = GMapUnit $ Just v

instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v = GMapPair (GMap a (GMap b v))
  empty = GMapPair empty
  lookup (a, b) (GMapPair gm) = lookup a gm >>= lookup b
  insert (a, b) v (GMapPair gm) = GMapPair $ case lookup a gm of
    Nothing  -> insert a (insert b v empty) gm
    Just gm2 -> insert a (insert b v gm2) gm

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v              = GMapEither (GMap a v) (GMap b v)
  empty                                 = GMapEither empty empty
  lookup (Left k) (GMapEither ml _)     = lookup k ml
  lookup (Right k) (GMapEither _ mr)    = lookup k mr
  insert (Left k) v (GMapEither ml mr)  = GMapEither (insert k v ml) mr
  insert (Right k) v (GMapEither ml mr) = GMapEither ml (insert k v mr)

myGMap :: GMap (Int, Either Char ()) String
myGMap =
  insert (5, Left 'c') "(5, Left 'c')" $
  insert (4, Right ()) "(4, Right ())" $
  insert (5, Right ()) "This is the one!" $
  insert (5, Right ()) "This is the two!" $
  insert (6, Right ()) "(6, Right ())" $
  insert (5, Left 'a') "(5, Left 'a')" $
  empty

gmLookup k = maybe "Couldn't find key!" id $ lookup k myGMap

-- | Below is alternate implementation writtein with functional dependencies.
--
-- See also:
--
-- * http://www.haskell.org/pipermail/haskell-cafe/2009-February/055890.html
--
-- > class Collects e ce | ce -> e where
-- >   emptyC  :: ce
-- >   insertC :: e -> ce -> ce
-- >   memberC :: e -> ce -> Bool
-- >   toListC :: ce -> [e]
-- >
-- > instance Eq e => Collects e [e] where
-- >   emptyC = []
-- >   insertC e l = (e:l)
-- >   memberC e []     = False
-- >   memberC e (x:xs) | e == x = True
-- >                    | otherwise = member e xs
-- >   toListC l = l
-- >
-- > sumCollects :: (Collects e c1, Collects e c2) => c1 -> c2 -> c2
-- > sumCollects c1 c2 = foldr insert c2 (toListC c1)
--
class Collects ce where
  type Elem ce :: *
  emptyC  :: ce
  insertC :: Elem ce -> ce -> ce
  memberC :: Elem ce -> ce -> Bool
  toListC :: ce -> [Elem ce]

instance Eq e => Collects [e] where
  type Elem [e] = e
  emptyC = []
  insertC e l = (e:l)
  memberC e [] = False
  memberC e (x:xs) | e == x    = True
                   | otherwise = memberC e xs
  toListC l = l

instance (Eq e, Ord e) => Collects (S.Set e) where
  type Elem (S.Set e) = e
  emptyC = S.empty
  insertC e s = S.insert e s
  memberC e s = S.member e s
  toListC l = S.elems l

-- | Try:
--
-- > > let s = S.fromList [1,2,3]
-- > > let l = [4..10]
-- > > sumCollects s l
-- > [1,2,3,4,5,6,7,8,9,10]
--
sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
sumCollects c1 c2 = foldr insertC c2 (toListC c1)
