{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 2.

-}
module Take02 where

import Data.Typeable

import Take01
  ( Sym(..)
  , R(..),toR,eval
  , S(..),toS,view
  , Tree(..),ppTree,safeRead
  )

-- te01 :: Sym r => r Int
te01 = add (int 1) (int 2)

-- te02 :: Sym r => r (Int -> Int)
te02 = lam (\x -> add x x)
te02' = app te02 (int 3)

-- te03 :: Sym r => r ((Int -> Int) -> Int)
te03 = lam (\x -> add (app x (int 1)) (int 2))
te04 = app te03 te02

-- data Term r = forall a. (Sym r, Show a, Typeable a) => Term (r a)
data Term r = forall a. (Show a, Typeable a) => Term (r a)

terms :: Sym r => [Term r]
terms = [Term te01, Term te02, Term te03, Term te04]

view_term :: Term S -> String
view_term (Term t) = view t

show_eval_term :: Term R -> String
show_eval_term (Term t) = show $ eval t

print_s_terms :: IO ()
print_s_terms = mapM_ (putStrLn . view_term) terms

print_r_terms :: IO ()
print_r_terms = mapM_ (putStrLn . show_eval_term) terms

class T t where
  tany :: t a
  tint :: t Int
  tarr :: t a -> t b -> t (a->b)

infixr 9 `tarr`

ty01 = tint
ty02 = tint `tarr` tint
ty03 = (tint `tarr` tint) `tarr` tint

data Ty t where
  TyAny :: Ty a
  TyInt :: Ty Int
  TyArr :: Ty a -> Ty b -> Ty (a->b)

instance T Ty where
  tany = TyAny
  tint = TyInt
  tarr = TyArr

toTy :: Ty t -> Ty t
toTy = id

instance Show (Ty a) where
  show t = case t of
    TyAny     -> "any"
    TyInt     -> "Int"
    TyArr a b -> concat ["(",show a," -> ",show b,")"]

data Equal a b where
  Equal :: Equal c c

instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy ta tb = case (ta,tb) of
  (TyInt,TyInt)             -> Just Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal
  _                         -> Nothing

newtype Ex a = Ex {unEx :: Int -> Tree}

toEx :: Ex a -> Ex a
toEx = id


instance Sym Ex where
  int x = Ex $ \_ -> Node "int" [Leaf (show x)]
  add a b = Ex $ \i -> Node "add" [unEx a i, unEx b i]
  lam f = Ex $ \h ->
    let x = Node "var" [Leaf (show h)]
        ty = ty2Tree (f (Ex $ const x))
        bdy = unEx (f (Ex (\_ -> x))) (succ h)
    in  Node "lam" [x,ty,bdy]
  app a b = Ex $ \h -> Node "app" [unEx a h, unEx b h]

eTree :: Ex a -> Tree
eTree e = unEx e 0

ty2Tree :: Ex a -> Tree
ty2Tree e = case unEx e 0 of
  Node "int" _          -> Leaf "TyInt"
  Node "add" [e1,e2]    -> Node "TyArr" [Leaf "TyInt", Leaf "TyInt"]
  t -> t

  -- XXX: TDODO
  -- Node "lam" [v,ty,bdy] -> Node "TyArr" [ty2Tree v, ty]
  -- Node "app" [e1,e2]    -> Node "TyArr" [ty2Tree e1, ty2Tree e2]
  -- Node "var" [Leaf v]   -> ???

{-

Need to preserve the type and value at the time of serialization.
Couple each variable and its type.

So the task is:

* Rewriting typecheck without using de Bruijn indice.
* Rewrite without using functions and types imported from Typ.hs.

... Suppose, found something.
The reason de Bruijn indice helps deserialization is that, class Sym contains
method to refer variables, s and z. These variable referencing functions may
necessary during the deseerialization. If not, how can the variable looked up
from abstract syntax representation, preserving itself as variable, not as
applyed value.

-}
newtype TyTree a = TyTree {unTyTree :: Int -> Tree}

instance Sym TyTree where
  int x = TyTree $ \h ->
    Node "int" [Leaf (show x)]
  add x y = TyTree $ \h ->
    let e1 = case unTyTree x h of
          Node "var" ls -> Node "var" (Leaf "TyInt":ls)
          x             -> x
        e2 = case unTyTree y h of
          Node "var" ls -> Node "var" (Leaf "TyInt":ls)
          x             -> x
    in   Node "add" [e1, e2]
  lam f = TyTree $ \h ->
    let v = Node "var" [Leaf $ show h]
        bdy = unTyTree (f (TyTree $ const v)) (succ h)
        ty = lamTy v bdy
        -- bdy' = bdy
        bdy' = removeVarTy bdy
    in  Node "lam" [v,ty,bdy']
  app e1 e2 = TyTree $ \h ->
    let e1' = unTyTree e1 h
        e2' = unTyTree e2 h
    in  Node "app" [e1',e2']

lamTy :: Tree -> Tree -> Tree
lamTy var bdy = case var of
  Node "var" (v:_) ->
    let f u b = case b of
          Node "var" ts | u `elem` ts -> head ts
          Node  _    ts -> head $ map (f u) ts
          _             -> Leaf "TyAny"
    in  f v bdy
  _ -> var

tyTree :: TyTree a -> Tree
tyTree t = unTyTree t 0

removeVarTy :: Tree -> Tree
removeVarTy t = case t of
  Leaf e -> t
  Node "var" [e1,e2] -> Node "var" [e2]
  Node n ts -> Node n (map removeVarTy ts)

data ExtTy where
  ExtTy :: Ty a -> ExtTy

instance Show ExtTy where
  show (ExtTy t) = "ExtTy " ++ show t

data Typed con where
  Typed :: Ty ty -> con ty -> Typed con

type Env r = Sym r => [(String,Typed r)]

--- fromTree :: Sym con => Tree -> Env con -> Either String (Typed con)
fromTree t d = case t of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Typed TyInt (int x')
  Node "add" [e1,e2] -> do
    Typed TyInt i1 <- fromTree e1 d
    Typed TyInt i2 <- fromTree e2 d
    return $ Typed TyInt $ add i1 i2
  Node "var" [Leaf v] -> do
    case lookup v d of
      -- XXX: What should be passed to second arg of Typed?
      -- When Sym class has z and s, we can use VarDesc and lookup tuple
      -- each by each. We need a method to denote the argument as var.
      -- How can we?
      --- Just (ExtTy t) -> return $ Typed t (error $ "value of var: " ++ show t)
      Just (ExtTy t) ->
        return $ Typed t (error $ "value of " ++ show t)
      Nothing ->
        Left $ "Unbound variable: " ++ v ++ " in env: " ++ show (map fst d)
  Node "lam" [v,ty,bdy] -> do
    let Node "var" [Leaf vname] = v
    ety@(ExtTy vty) <- tree2ty ty
    Typed bdyty ebdy <- fromTree bdy ((vname,ety):d)
    return $ Typed (vty `tarr` bdyty) (lam (const ebdy))
  Node "app" [e1,e2] -> do
    Typed (TyArr bndty bdyty) e1v <- fromTree e1 d
    Typed argty e2v <- fromTree e2 d
    case cmpTy argty bndty of
      Nothing    -> error "type mismatch in app"
      Just Equal -> return $ Typed bdyty (app e1v e2v)
  _ -> error $ "Unknown: " ++ show t

extTyTree :: ExtTy -> Tree
extTyTree (ExtTy ty) = case ty of
  TyInt       -> Leaf "TyInt"
  TyArr t1 t2 -> Node "TyArr" [extTyTree (ExtTy t1), extTyTree (ExtTy t2)]

tree2ty :: Tree -> Either String ExtTy
tree2ty t = case t of
  Leaf "TyAny"         -> return $ ExtTy TyAny
  Leaf "TyInt"         -> return $ ExtTy TyInt
  Node "TyArr" [e1,e2] -> do
    ExtTy t1 <- tree2ty e1
    ExtTy t2 <- tree2ty e2
    return $ ExtTy $ TyArr t1 t2
  _                    -> Left $ "Unknown type: " ++ show t

-- fromTree_lam :: Tree -> [(String,ExtTy)] -> Either String ExtTy
-- fromTree_lam e d = case e of
--   Node "lam" [v,ty,bdy] -> do
--     let Node "var" [Leaf idx] = v
--     ety@(ExtTy argty) <- tree2ty ty
--     Typed bdyty bdyval <- fromTree bdy ((idx,argty):d)
--     return $ ExtTy bdyty

newtype Tyty a = Tyty {unTyty :: ExtTy}

instance Sym Tyty where
  int _   = Tyty $ ExtTy TyInt
  add _ _ = Tyty $ ExtTy TyInt
  lam f   = Tyty $ undefined
  app (Tyty (ExtTy a)) (Tyty (ExtTy b)) = Tyty $ case a of
    TyArr _ a' -> ExtTy a'
    _          -> ExtTy a

tt01_tree = Node "TyArr" [Leaf "TyInt", Node "TyArr" [Leaf "TyInt", Leaf "TyInt"]]
tt01_ext = tree2ty tt01_tree

dynt_01 :: (String, String)
dynt_01 = case tint `tarr` tint `tarr` tint of
  ty -> case lam (\x -> lam (\y -> add x y)) of
    bdy -> case Typed ty bdy of
      Typed ty' s -> (view s, show ty)

-- This does not work:
--
-- > dyn_show ty bdy = (view bdy, show ty)
--
-- But below does.
--
dyn_show ty bdy = case Typed ty bdy of Typed ty' s -> (view s, show ty')

te01_dyn = dyn_show tint te01
te02_dyn = dyn_show ty02 te02
te03_dyn = dyn_show ty03 te03
te04_dyn = dyn_show tint te04

deserialize_tree e = do
  Typed ty val <- fromTree (tyTree e) []
  return (view val,show ty)

-- type Gamma = [(String,TypeRep)]

-- ap2 f m1 m2 = do v1 <- m1; v2 <- m2; f v1 v2
-- ap3 f m1 m2 m3 = do v1 <- m1; v2 <- m2; v3 <- m3; f v1 v2 v3

-- fromTreeG :: forall r. Sym r => Tree -> Gamma -> Either String (Term r)
-- fromTreeG e g = case e of
--   Leaf x -> case safeRead x of
--     Right i -> return . weaken g $ Term (int i)
--   Node "add" [e1,e2] ->
--     ap2 (tadd g) (fromTreeG e1 g) (fromTreeG e2 g)
--   _ -> undefined
--   where
--     tadd g (d1@(Term e1)) (d2@(Term e2))
--       | [] <- g, Just e1' <- gcast e1, Just e2' <- gcast e2
--       = return $ Term (add e1' e2')
--       | otherwise
--       = typecheck_app (weaken_add g) d1 >>= \f -> typecheck_app f d2

-- weaken :: Sym r => Gamma -> Term r -> Term r
-- weaken g d = case (g,d) of
--   ([],_) -> d
--   ((_,typ):g',_) | Term xt <- reflect typ, Term d' <- weaken g d ->
--      let mkfun :: repr a -> repr (a->b)
--          mkfun = undefined
--      in  Term (lam (\x -> d') `asTypeOf` mkfun xt)

-- -- weaken = undefined

-- typecheck_app = undefined
-- weaken_add = undefined

-- reflect :: forall r. TypeRep -> Term r
-- reflect x
--   | x == typeOf (undefined :: Int) = Term (undefined :: r Int)
--   | Just (Term e1, Term e2) <- reflect_fn x =
--     let mkfun :: r a -> r b -> r (a->b)
--         mkfun = undefined
--     in  Term (mkfun e1 e2)

-- reflect_fn :: TypeRep -> Maybe (Term r, Term r)
-- reflect_fn tfun
--   | (con,[arg1,arg2]) <- splitTyConApp tfun, con == arrowTyCon =
--     Just (reflect arg1, reflect arg2)
--   | otherwise = Nothing

-- -- XXX:
-- -- When primitive value increase, how to extend this function?
-- arrowTyCon = typeRepTyCon (typeOf (undefined :: Int -> Int))

-- ft :: Sym r => Tree -> Term r
-- ft exp = either error id (fromTreeG exp [])

-- data H h where
--   HInt :: Int -> H h
--   HVar :: String -> H h
--   HLam :: (H a -> H b) -> H (a->b)
--   HApp :: H (a->b) -> H a -> H b

-- instance Sym H where
--   int = HInt
--   add = undefined
--   lam = HLam
--   app = HApp

-- class LC lc where
--   lv :: a -> lc a
--   ll :: a -> lc a -> lc a
--   la :: lc a -> lc a -> lc a

-- data ILC v
--   = Var v
--   | Lam v (ILC v)
--   | App (ILC v) (ILC v)
--   deriving Show

-- instance LC ILC where
--   lv = Var
--   ll = Lam
--   la = App