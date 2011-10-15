{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

From 'lambda-calculus cooked four ways'.
-}
module IdInt where

import Control.Monad.State
import qualified Data.Map as M

import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as H

import Lambda

newtype IdInt = IdInt Int deriving (Eq, Ord)

instance Enum IdInt where
  toEnum i = IdInt i
  fromEnum (IdInt i) = i

instance Hashable IdInt where
  hash (IdInt i) = hash i

instance Show IdInt where
  show (IdInt i) = if i < 0 then "f" ++ show (-i) else "x" ++ show i

instance Hashable Id where
  hash (Id s) = hash s

firstBoundId :: IdInt
firstBoundId = IdInt 0

toIdInt :: (Hashable v, Ord v) => LC v -> LC IdInt
toIdInt e = evalState (conv e) (0, fvmap) where
  fvmap = foldr (\(v,i) m -> H.insert v (IdInt (-i)) m) H.empty
          (zip (freeVars e) [1..])

type M v a = State (Int, H.HashMap v IdInt) a

convVar :: (Hashable v, Ord v) => v -> M v IdInt
convVar v = do
  (i,m) <- get
  case H.lookup v m of
    Nothing -> do
      let ii = IdInt i
      put (i+1, H.insert v ii m)
      return ii
    Just ii -> return ii

conv :: (Hashable v, Ord v) => LC v -> M v (LC IdInt)
conv (Var v) = liftM Var (convVar v)
conv (Lam v e) = liftM2 Lam (convVar v) (conv e)
conv (App f a) = liftM2 App (conv f) (conv a)