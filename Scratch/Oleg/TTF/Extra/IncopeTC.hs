{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/IncopeTypecheck.hs>

-}
module IncopeTC where

import ICP
import Data.Typeable
import Control.Monad
import Control.Monad.Instances
import Text.Show.Functions

data DynTerm r = ∀ a. (Show a, Typeable a) => DynTerm (r a)

data UExp
  = UInt Int
  | UBool Bool
  | UVar VarName
  | UL VarName UExp UExp
  | UApp UExp UExp
  | UAdd UExp UExp
  | UMul UExp UExp
  | ULeq UExp UExp
  | UIf UExp UExp UExp
  deriving (Show)

type VarName = String
type Gamma = [(VarName,TypeRep)]

ap2 f m1 m2 = do v1 <- m1; v2 <- m2; f v1 v2
ap3 f m1 m2 m3 = do v1 <- m1; v2 <- m2; v3 <- m3; f v1 v2 v3

typecheck :: ∀ r. Symantics r => UExp -> Gamma -> Either String (DynTerm r)
typecheck e g = case e of
  UInt x -> return $ weaken g $ DynTerm (int x)
  UBool x -> return $ weaken g $ DynTerm (bool x)
  UVar v
    | (g',((_,vt):g'')) <- break (\(v',_) -> v == v') g
    , DynTerm argt <- reflect vt
    -> return . weaken g' . weaken_under g'' $
        (DynTerm (lam (\x -> x `asTypeOf` argt)))
    | otherwise
    -> fail $ "unbound var: " ++ v
  UAdd e1 e2 -> ap2 (tfun2 add g) (typecheck e1 g) (typecheck e2 g)
  UMul e1 e2 -> ap2 (tfun2 mul g) (typecheck e1 g) (typecheck e2 g)
  ULeq e1 e2 -> ap2 (tfun2 leq g) (typecheck e1 g) (typecheck e2 g)
  UL v vt body
    | Right ((DynTerm vtt) :: DynTerm r) <- typecheck vt []
    -> typecheck body (g ++ [(v,typerep vtt)])
    | otherwise
    -> fail $ unwords ["term used for annotating lambda is ill-typed", show vt]
  UApp e1 e2 -> ap2 (tapp g) (typecheck e1 g) (typecheck e2 g)
  UIf e1 e2 e3 -> ap3 (tif g) (typecheck e1 g) (typecheck e2 g) (typecheck e3 g)
  where
    tfun2 f g (d1@(DynTerm e1)) (d2@(DynTerm e2)) = case g of
      [] | Just e1' <- gcast e1, Just e2' <- gcast e2 ->
           return $ DynTerm (f e1' e2')
         | otherwise -> fail "tfun2: args are not int exp"
      _  -> do
        f <- typecheck_app (weaken_fun2 f g) d1
        typecheck_app f d2
    weaken_fun2 f g
      | null g = DynTerm $ lam (\x -> lam (\y -> f x y))
      | otherwise = weaken_arg2 g (weaken_fun2 f [])
    tapp g d1@(DynTerm d1') d2
      | null g = typecheck_app d1 d2
      | Just tf <- typerep_without_hyp d1' g
      , Just wapp <- weaken_app g tf
      = typecheck_app wapp d1 >>= \f -> typecheck_app f d2
      | otherwise = fail $ "Bad app: " ++ show e
    tif g (DynTerm e1) (DynTerm e2) (DynTerm e3) = case g of
      [] | Just e1' <- gcast e1
         , Just e3' <- gcast e3
         -> return $ DynTerm (if_ e1' e2 e3')
      _  -> do
        error "XXX: Add weaken if"

typecheck_app ::
  Symantics r => DynTerm r -> DynTerm r -> Either String (DynTerm r)
typecheck_app (DynTerm e1) (DynTerm e2)
  | let tfun = typerep e1
  , let targ = typerep e2
  , Just tres <- funResultTy tfun targ
  , DynTerm b <- reflect tres
  , Just e1' <- gcast e1
  = return $ DynTerm (app e1' e2 `asTypeOf` b)
  | otherwise =
    Left $ unwords ["Bad App, of types", show (typerep e1)
                   ,"and", show (typerep e2)]

weaken :: Symantics r => Gamma -> DynTerm r -> DynTerm r
weaken g d = case g of
  []         -> d
  (_,typ):g' | DynTerm xt <- reflect typ, DynTerm d' <- weaken g' d ->
    let mkfun :: r a -> r (a->b)
        mkfun = undefined
    in  DynTerm (lam (\x -> d') `asTypeOf` mkfun xt)

weaken_under :: Symantics r => Gamma -> DynTerm r -> DynTerm r
weaken_under g d = case g of
  [] -> d
  ((_,typ):g')
    | DynTerm xt <- reflect typ
    , DynTerm d' <- weaken_under g' d
    , Just (DynTerm u, DynTerm t) <- reflect_fn $ typerep $ d'
    , Just d'' <- gcast d'
    -> let mkfun :: r a -> r b -> r c -> r (a->b->c)
           mkfun = undefined
       in  DynTerm $ lam (\u -> lam (\x -> app d'' u)) `asTypeOf` mkfun u xt t

weaken_arg2 :: Symantics r => Gamma -> DynTerm r -> DynTerm r
weaken_arg2 g d = case g of
  [] -> d
  (_,typ):g'
    | DynTerm xt <- reflect typ
    , DynTerm d' <- weaken_arg2 g' d
    , Just (DynTerm u,DynTerm tu) <- reflect_fn $ typerep d'
    , Just (DynTerm v,DynTerm t)  <- reflect_fn $ typerep tu
    , Just d'' <- gcast d'
    -> let mkfun :: r u -> r v -> r x -> r t -> r ((x->u) -> (x->v) -> x -> t)
           mkfun = undefined
       in  DynTerm (lam (\u -> lam (\v ->
                     lam (\x -> (d'' `app` (u `app` x)) `app` (v `app` x))))
                    `asTypeOf` mkfun u v xt t)

weaken_app :: Symantics r => Gamma -> TypeRep -> Maybe (DynTerm r)
weaken_app g tf = case g of
  [] | Just (DynTerm tx,DynTerm tb) <- reflect_fn tf
     -> let mkfun :: r x -> r b -> r ((x->b) -> x -> b)
            mkfun = undefined
        in  Just $ DynTerm (lam (\x -> lam (\y -> app x y))
                            `asTypeOf` mkfun tx tb)
     | otherwise -> Nothing
  _  -> liftM (weaken_arg2 g) (weaken_app [] tf)

typerep :: ∀ a r. Typeable a => r a -> TypeRep
typerep _ = typeOf (undefined :: a)

typerep_without_hyp :: ∀ a r. Typeable a => r a -> Gamma -> Maybe TypeRep
typerep_without_hyp t g = case g of
  []     -> Just $ typeOf (undefined :: a)
  (_:gs) | Just tr <- typerep_without_hyp t gs
         , Just (_,DynTerm tr') <- reflect_fn tr
         -> Just (typerep tr')
         | otherwise -> Nothing

reflect :: ∀ r. TypeRep -> DynTerm r
reflect x
  | x == typeOf (undefined :: Int) = DynTerm (undefined :: r Int)
  | x == typeOf (undefined :: Bool) = DynTerm (undefined :: r Bool)
  | otherwise = case reflect_fn x of
    Just (DynTerm e1, DynTerm e2) ->
      let mkfun :: r a -> r b -> r (a->b)
          mkfun = undefined
      in  DynTerm (mkfun e1 e2)

reflect_fn :: TypeRep -> Maybe (DynTerm r, DynTerm r)
reflect_fn tfun
  | (con,[arg1,arg2]) <- splitTyConApp tfun, con == arrowTyCon
  = Just (reflect arg1, reflect arg2)
  | otherwise = Nothing

-- XXX: Does this tycon supports other function than (Int -> Int)?
-- Can handle (Int -> Bool), nor (Bool -> Bool) ... etc?
arrowTyCon :: TyCon
arrowTyCon = typeRepTyCon (typeOf (undefined :: Int -> Int))

------------------------------------------------------------------------------
-- Tests

dynterm1 :: Symantics r => DynTerm r
dynterm1 = DynTerm (lam (\x -> x) `app` (bool True))

dynterm_R_bool :: Maybe Bool
dynterm_R_bool = case dynterm1 of DynTerm t -> cast (comp t)

from_dynterm_R_bool :: DynTerm R -> Maybe Bool
from_dynterm_R_bool (DynTerm t) = cast (comp t)

from_dynterm_R_int :: DynTerm R -> Maybe Int
from_dynterm_R_int (DynTerm t) = cast (comp t)

dynterm_R_show = case dynterm1 of DynTerm t -> show (comp t)
dynterm_S_show = case dynterm1 of DynTerm t -> view t
dynterm_L_show = case dynterm1 of DynTerm t -> show $ tsize t

tc :: Symantics r => UExp -> DynTerm r
tc exp = either error id (typecheck exp [])

rShow (DynTerm t) = show $ comp t
sShow (DynTerm t) = view t
lShow (DynTerm t) = show $ tsize t

dt1 = tc (UInt 1)
dt1r = rShow dt1
dt1s = sShow dt1
dt1l = lShow dt1

t2 = UAdd (UInt 2) (UInt 3)
t3 = UL "x0" (UInt 0) (UVar "x0")
t4 = UL "x" (UInt 1) (UL "y" (UBool True) (UL "z" (UInt 1) (UVar "y")))
t4a = UL "x" (UInt 1) (UL "y" (UInt 2) (UVar "y"))
t4b = UL "x" (UInt 1) (UL "y" (UInt 2) (UVar "x"))
t5 = UL "x" (UInt 100) (UAdd (UVar "x") (UInt 1))

------------------------------------------------------------------------------
-- Tests for exercises

u1 = UMul (UInt 2) (UInt 3)
u2 = UApp (UL "x" (UInt 1) (UMul (UVar "x") (UInt 10))) (UInt 3)
u3 = ULeq (UInt 1) (UInt 2)
u4 = UIf (UBool True) (UInt 1) (UInt 0)
u5 = UApp (UL "x" (UInt 0) (UIf (ULeq (UVar "x") (UInt 0)) (UInt 100) (UInt 200))) (UInt 8)

-- ghci> sShow (either error id (typecheck (UVar "x0") [("x0",typeOf (undefined :: Int))]))
-- "lam (\\x0 -> x0)"

-- ghci> sShow (either error id (typecheck (UVar "x0") [("x0",typeOf (undefined :: Bool))]))
-- "lam (\\x0 -> x0)"

-- ghci> sShow (either error id (typecheck (UVar "x0") [("x0",typeOf (undefined :: Int -> Int))]))
-- "lam (\\x0 -> x0)"

-- ghci> sShow $ tc (UL "x0" (UInt 0) (UVar "x0"))
-- "lam (\\x0 -> x0)"

{-
Exercises:

* Add typecheck for fix, if_, mul, and leq.

-}