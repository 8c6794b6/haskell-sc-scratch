{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Type representation used in pattern expression tree deserializer.

-}
module Sound.SC3.Lepton.Pattern.Expression.Type where

import Sound.SC3.Lepton.Pattern.ToOSC

-- | Class for types.
class TyC t where
  tint    :: t Int
  tdouble :: t Double
  tbool   :: t Bool
  ttoosc  :: t a -> t (ToOSC a)
  tlist   :: t a -> t [a]
  ttup    :: t a -> t b -> t (a,b)
  tarr    :: t a -> t b -> t (a->b)

-- | GADT style data type to represent types used in pattern expressions.
data Ty t where
  TyInt    :: Ty Int
  TyDouble :: Ty Double
  TyBool   :: Ty Bool
  TyToOSC  :: Ty a -> Ty (ToOSC a)
  TyList   :: Ty a -> Ty [a]
  TyTup    :: Ty a -> Ty b -> Ty (a,b)
  TyArr    :: Ty a -> Ty b -> Ty (a->b)
  TyAny    :: Ty a

toTy :: Ty t -> Ty t
toTy = id

instance TyC Ty where
  tint = TyInt
  tdouble = TyDouble
  tbool = TyBool
  ttoosc = TyToOSC
  tlist = TyList
  ttup = TyTup
  tarr = TyArr

tyc :: TyC typ => (forall typ1. TyC typ1 => typ1 a) -> typ a
tyc t = unTR (TR t)

-- | Type representation wrapper.
newtype TR a = TR {unTR :: forall typ. TyC typ => typ a}

instance Show (TR a) where
  show (TR a) = "TR " ++ (show $ toTy a)

instance TyC TR where
  tint = TR tint
  tdouble = TR tdouble
  tbool = TR tbool
  ttoosc a = TR (ttoosc (unTR a))
  tlist a = TR (tlist (unTR a))
  ttup a b = TR (ttup (unTR a) (unTR b))
  tarr a b = TR (tarr (unTR a) (unTR b))

data ExtTy where
  ExtTy :: Ty t -> ExtTy

data ExtTR where
  ExtTR :: TR t -> ExtTR

instance Show ExtTy where
  show (ExtTy ty) = "ExtTy (" ++ show ty ++ ")"

instance Show (Ty t) where
  show t = go t where
    go :: Ty a -> String
    go u = case u of
      TyInt     -> "Int"
      TyDouble  -> "Double"
      TyBool    -> "Bool"
      TyToOSC a -> "ToOSC (" ++ go a ++ ")"
      TyList a  -> '[' : go a ++ "]"
      TyTup a b -> '(' : go a ++ "," ++ go b ++ ")"
      TyArr a b -> '(' : go a ++ " -> " ++ go b ++ ")"
      _         -> "any"

data Equal a b where
  Equal :: Equal c c

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy x y = case (x,y) of
  (TyInt,TyInt)             -> return Equal
  (TyDouble,TyDouble)       -> return Equal
  (TyBool,TyBool)           -> return Equal
  (TyToOSC a,TyToOSC b)     -> cmpTy a b >>= \Equal -> return Equal
  (TyList a,TyList b)       -> cmpTy a b >>= \Equal -> return Equal
  (TyTup a1 b1,TyTup a2 b2) -> do
    Equal <- cmpTy a1 a2
    Equal <- cmpTy b1 b2
    return Equal
  (TyArr a1 b1,TyArr a2 b2) -> do
    Equal <- cmpTy a1 a2
    Equal <- cmpTy b1 b2
    return Equal
  _                         -> Nothing

