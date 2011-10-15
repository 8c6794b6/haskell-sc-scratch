{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

From 'lambda-calculus cooked four ways'.
-}
module HOAS (nf) where

import qualified Data.HashMap.Strict as H

import Lambda
import IdInt

data HOAS
  = HVar IdInt
  | HLam (HOAS -> HOAS)
  | HApp HOAS HOAS

nf :: LC IdInt -> LC IdInt
nf = toLC . nfh . fromLC

nfh :: HOAS -> HOAS
nfh e = case e of
  HVar _   -> e
  HLam b   -> HLam (nfh . b)
  HApp f a -> case whnf f of
    HLam b -> nfh (b a)
    f'     -> HApp (nfh f') (nfh a)

whnf :: HOAS -> HOAS
whnf e = case e of
  HVar _ -> e
  HLam _ -> e
  HApp f a -> case whnf f of
    HLam b -> whnf (b a)
    f'     -> HApp f' a

fromLC :: LC IdInt -> HOAS
fromLC = from H.empty where
  from m (Var v) = maybe (HVar v) id (H.lookup v m)
  from m (Lam v e) = HLam $ \x -> from (H.insert v x m) e
  from m (App f a) = HApp (from m f) (from m a)

toLC :: HOAS -> LC IdInt
toLC = to firstBoundId where
  to _ (HVar v) = Var v
  to n (HLam b) = Lam n (to (succ n) (b (HVar n)))
  to n (HApp f a) = App (to n f) (to n a)