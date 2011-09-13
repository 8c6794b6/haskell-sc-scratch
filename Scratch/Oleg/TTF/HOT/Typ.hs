{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Lecture: <http://okmij.org/ftp/tagless-final/course/Typ.hs>

-}
module Typ where

import Control.Monad
import Control.Monad.Error

-- | Language of type representations, first-order and typed.
class TSYM trepr where
  tint :: trepr Int
  tarr :: trepr a -> trepr b -> trepr (a->b)

-- | View interpreter for Type.
newtype ShowT a = ShowT String

instance TSYM ShowT where
  tint = ShowT "Int"
  tarr (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ " -> " ++ b ++ ")"

view_t :: ShowT a -> String
view_t (ShowT t) = t

------------------------------------------------------------------------------
-- Adding Bool type
--
-- New classe is TBool, new concreate data type is AsBool.
-- New instance definitions are instances of TBool for existing type,
-- ShowT, TQ, ShowAs, SafeCast, AsInt, AsArrow. And TSYM and AsBool
-- instance for TBool instance.
--
-- Also, type class constraint for TQ need to be modified from:
--
-- TSYM trepr => ...
--
-- to:
--
-- (TSYM trepr, TBool trepr) => ....
--
-- Will this happen whenever we add primitive type?
--

class TBool trepr where
  tbool :: trepr Bool

instance TBool ShowT where
  tbool = ShowT "Bool"

instance TBool TQ where
  tbool = TQ tbool

instance TBool ShowAs where
  tbool = ShowAs tbool show

instance TBool AsInt where
  tbool = AsInt Nothing

instance TBool AsArrow where
  tbool = AsArrow tbool Nothing

instance TBool SafeCast where
  tbool = SafeCast $ \tb ->
    case unTQ tb of AsBool eq -> fmap symm eq

newtype AsBool a = AsBool (Maybe (EQU a Bool))

instance TBool AsBool where
  tbool = AsBool $ Just refl

instance TSYM AsBool where
  tint = AsBool Nothing
  tarr _ _ = AsBool Nothing

tb1 = (tint `tarr` tbool)
tb1_view = view_t tb1 -- (Int -> Bool)

-- | Quantifying over TSYM interpreter.
newtype TQ t =
  TQ {unTQ :: forall (trepr :: * -> *). (TSYM trepr,TBool trepr) => trepr t}

instance TSYM TQ where
  tint = TQ tint
  tarr (TQ a) (TQ b) = TQ (tarr a b)

tt1 = (tint `tarr` tint) `tarr` tint   -- ((Int -> Int) -> Int)
tt1_view = view_t tt1

tt2 = tint `tarr` (tint `tarr` tint)   -- (Int -> Int -> Int)
tt2_view = view_t tt2

------------------------------------------------------------------------------
-- Show Typ-able expressions

show_as :: TQ a -> a -> String
show_as tr a = case unTQ tr of ShowAs _ f -> f a

data ShowAs a = ShowAs (TQ a) (a -> String)

instance TSYM ShowAs where
  tint = ShowAs tint show
  tarr (ShowAs t1 _) (ShowAs t2 _) =
    let t = tarr t1 t2
    in  ShowAs t $ \_ -> "<function of the type " ++ view_t (unTQ t) ++ ">"

------------------------------------------------------------------------------
-- Type representation
data Typ = forall t. Typ (TQ t)

------------------------------------------------------------------------------
-- Copying

data TCOPY trep1 trep2 a = TCOPY (trep1 a) (trep2 a)

instance (TSYM trep1, TSYM trep2) => TSYM (TCOPY trep1 trep2) where
  tint = TCOPY tint tint
  tarr (TCOPY a1 a2) (TCOPY b1 b2) = TCOPY (tarr a1 b1) (tarr a2 b2)

------------------------------------------------------------------------------
-- Equality and safe cast

newtype EQU a b = EQU {equ_cast :: forall c. c a -> c b}

refl :: EQU a a
refl = EQU id

-- tran :: c u -> EQU u b -> c b -- c = EQU a
tran :: EQU a u -> EQU u b -> EQU a b
tran au ub = equ_cast ub au

newtype FS b a = FS {unFS :: EQU a b}

symm :: EQU a b -> EQU b a
symm equ = unFS . equ_cast equ . FS $ refl

newtype F1 t b a = F1 {unF1 :: EQU t (a->b)}
newtype F2 t a b = F2 {unF2 :: EQU t (a->b)}

eq_arr :: EQU a1 a2 -> EQU b1 b2 -> EQU (a1->b1) (a2->b2)
eq_arr a1a2 b1b2 =
  unF2 . equ_cast b1b2 . F2 . unF1 . equ_cast a1a2 . F1 $ refl

------------------------------------------------------------------------------
-- Constructive deconstructors

newtype AsInt a = AsInt (Maybe (EQU a Int))

instance TSYM AsInt where
  tint     = AsInt $ Just refl
  tarr _ _ = AsInt Nothing

as_int :: AsInt a -> c a -> Maybe (c Int)
as_int (AsInt (Just equ)) r = Just $ equ_cast equ r
as_int _ _                  = Nothing

data AsArrow a =
  forall b1 b2. AsArrow (TQ a) (Maybe (TQ b1, TQ b2, EQU a (b1->b2)))

instance TSYM AsArrow where
  tint                               = AsArrow tint Nothing
  tarr (AsArrow t1 _) (AsArrow t2 _) = AsArrow (tarr t1 t2) $ Just (t1,t2,refl)

as_arrow :: AsArrow a -> AsArrow a
as_arrow = id

-- more cases could be added ...

newtype SafeCast a = SafeCast (forall b. TQ b -> Maybe (EQU a b))

instance TSYM SafeCast where
  tint = SafeCast $ \tb ->
    case unTQ tb of AsInt eq -> fmap symm eq
  tarr (SafeCast t1) (SafeCast t2) = SafeCast  $ \tb -> do
    AsArrow _ (Just (b1,b2,equ_bb1b2)) <- return $ as_arrow (unTQ tb)
    equ_t1b1 <- t1 b1
    equ_t2b2 <- t2 b2
    return $ tran (eq_arr equ_t1b1 equ_t2b2) (symm equ_bb1b2)

safe_gcast :: TQ a -> c a -> TQ b -> Maybe (c b)
safe_gcast (TQ ta) ca tb = cast ta where
  cast (SafeCast f) = maybe Nothing (\equ -> Just (equ_cast equ ca)) $ f tb

------------------------------------------------------------------------------
-- Dynamic

data Dynamic = forall t. Dynamic (TQ t) t

instance Show Dynamic where
  show (Dynamic tr a) = show_as tr a

tdn_show (Dynamic tr a) = show_as tr a

tdn1 = Dynamic tint 5

tdn2 = Dynamic tt1 ($ 1)

tdn3 = Dynamic (tint `tarr` (tint `tarr` tint)) (*)

newtype Id a = Id a

tdn_eval1 (Dynamic tr d) x = do
  Id f <- safe_gcast tr (Id d) tt1
  return . show $ f x

tdn_eval2 (Dynamic tr d) x y = do
  Id f <- safe_gcast tr (Id d) tt2
  return . show $ f x y

tdn1_show = tdn_show tdn1
tdn2_show = tdn_show tdn2
tdn3_show = tdn_show tdn3

tdn1_eval = tdn_eval1 tdn1 (+4)
tdn2_eval = tdn_eval1 tdn2 (+4)
tdn2_eval' = tdn_eval2 tdn2 3 14
tdn3_eval = tdn_eval2 tdn3 3 14

tdn_b_eval1 (Dynamic tr d) x = do
  Id f <- safe_gcast tr (Id d) tb1
  return . show $ f x

tdn_b_eval2 (Dynamic tr d) x = do
  Id f <- safe_gcast tr (Id d) (tbool `tarr` tbool)
  return . show $ f x

-- With TBool

tdn_b1 = Dynamic tbool True
tdn_b2 = Dynamic tbool False
tdn_b3 = Dynamic tb1 (< 3)
tdn_b4 = Dynamic (tbool `tarr` tbool) not

tdn_b1_show = tdn_show tdn_b1
tdn_b2_show = tdn_show tdn_b2
tdn_b3_show = tdn_show tdn_b3
tdn_b4_show = tdn_show tdn_b4

tdn_b3_eval  = tdn_b_eval1 tdn_b3 2
tdn_b3_eval' = tdn_b_eval1 tdn_b3 4
tdn_b4_eval  = tdn_b_eval2 tdn_b4 True
tdn_b4_eval' = tdn_b_eval2 tdn_b4 False
