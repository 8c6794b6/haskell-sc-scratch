{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Lecture: <http://okmij.org/ftp/tagless-final/course/TypeCheck.hs>

-}
module TypeCheck where

import Control.Monad
import Control.Monad.Error hiding (fix)

import Typ
import TTFdB

data Tree
  = Leaf String
  | Node String [Tree]
  deriving (Eq, Read, Show)

------------------------------------------------------------------------------
-- | Typing dyamic typing.
--
-- This needs ExistentialQuantification
--
data DynTerm repr h = forall a. DynTerm (TQ a) (repr h a)

tdyn1 = DynTerm tint (lam z `app` (int 1))

tdyn1_eval = case tdyn1 of DynTerm tr t -> show_as tr $ eval t
tdyn1_view = case tdyn1 of DynTerm _  t -> view t

-- tdyn1_tview = case tdyn1 of DynTerm tr _ -> view_t (unTQ tr)

-- XXX:
-- This function should have written in the open recursion style,
-- so that it would be extensible.
--
read_t :: Tree -> Either String Typ
read_t (Node "TInt" []) = return $ Typ tint
read_t (Node "TArr" [e1,e2]) = do
  Typ t1 <- read_t e1
  Typ t2 <- read_t e2
  return . Typ $ tarr t1 t2
read_t tree = fail $ "Bad type expression: " ++ show tree

------------------------------------------------------------------------------
-- Type checking environment

type VarName = String
data VarDesc t = VarDesc (TQ t) VarName -- Component of Gamma

-- Relating Gamma (compile-time env) to h (runtime env)
class Var gamma h | gamma -> h where
  findvar :: Symantics repr =>
             VarName -> gamma -> Either String (DynTerm repr h)

asTypeRepr :: t -> repr h t -> repr h t
asTypeRepr _ = id

instance Var () () where
  findvar name _ = fail $ "Unbound variable: " ++ name

instance Var gamma h => Var (VarDesc t, gamma) (t,h) where
  findvar name (VarDesc tr name',_) | name == name' = return $ DynTerm tr z
  findvar name (_,gamma) = do
    DynTerm tr v <- findvar name gamma
    return $ DynTerm tr (s v)

typecheck :: (Symantics repr, Var gamma h) =>
  Tree -> gamma -> Either String (DynTerm repr h)
typecheck n gamma = case n of
  Node "Int" [Leaf str] -> case reads str of
    [(i,[])] -> return $ DynTerm tint (int i)
    _        -> fail $ "Bad Int literal: " ++ str
  Node "Add" [e1,e2] -> do
    DynTerm (TQ t1) d1 <- typecheck e1 gamma
    DynTerm (TQ t2) d2 <- typecheck e2 gamma
    case (as_int t1 d1, as_int t2 d2) of
      (Just t1',Just t2') -> return . DynTerm tint $ add t1' t2'
      (Nothing,_)         -> fail $ "Bad type of a summand: " ++ view_t t1
      (_,Nothing)         -> fail $ "Bad type of a summand: " ++ view_t t2
  Node "Var" [Leaf name] -> findvar name gamma
  Node "Lam" [Leaf name, etyp,ebody] -> do
    Typ ta <- read_t etyp
    DynTerm tbody body <- typecheck ebody (VarDesc ta name, gamma)
    return $ DynTerm (tarr ta tbody) (lam body)
  Node "App" [e1,e2] -> do
    DynTerm (TQ t1) d1 <- typecheck e1 gamma
    DynTerm (TQ t2) d2 <- typecheck e2 gamma
    AsArrow _ arr_cast <- return $ as_arrow t1
    let errarr = fail $ "operator type is not an arrow: " ++ view_t t1
    (ta,tb,equ_t1ab) <- maybe errarr return arr_cast
    let df = equ_cast equ_t1ab d1
    case safe_gcast (TQ t2) d2 ta of
      Just da -> return . DynTerm tb $ app df da
      _       -> fail $ unwords ["Bad type of the application: ",
                                 view_t t1, "and", view_t t2]
  _ -> fail $ "Invalid Tree: " ++ show n

------------------------------------------------------------------------------
-- Simple terms

tx1 = typecheck
  (Node "Var" [Leaf "x"]) ()

tx2 = typecheck
  (Node "Lam" [Leaf "x", Node "TInt" [],
               Node "Var" [Leaf "x"]]) ()

tx3 = typecheck
  (Node "Lam" [Leaf "x", Node "TInt" [],
               Node "Lam" [Leaf "y", Node "TInt" [],
                           Node "Var" [Leaf "x"]]]) ()

tx4 = typecheck
  (Node "Lam" [Leaf "x", Node "TInt" [],
               Node "Lam" [Leaf "y", Node "TInt" [],
                           Node "Add" [Node "Int" [Leaf "10"],
                                       Node "Var" [Leaf "x"]]]]) ()

tx_view t = case t of
  Right (DynTerm _ t) -> view t
  Left err            -> error err

-- > tx_view tx4
-- "(\\x0 -> (\\x1 -> (10+x0)))"

------------------------------------------------------------------------------
-- Closed interpreter
--
-- To covert monomorphic binding to polymorphic in tc_evalview.
--

newtype CL h a = CL
  {unCL :: forall (repr :: * -> * -> *).
           (MulSYM repr, Symantics repr, BoolSYM repr) => repr h a}

instance Symantics CL where
  int x = CL $ int x
  add e1 e2 = CL $ add (unCL e1) (unCL e2)
  z = CL z
  s v = CL $ s (unCL v)
  lam e = CL $ lam (unCL e)
  app e1 e2 = CL $ app (unCL e1) (unCL e2)

tc_evalview :: Tree -> Either String (String, String)
tc_evalview exp = do
  DynTerm tr d <- typecheck exp ()
  let d' = unCL d
  return $ (show_as tr $ eval d', view d')

tr_int x = Node "Int" [Leaf $ show x]
tr_add e1 e2 = Node "Add" [e1,e2]
tr_app e1 e2 = Node "App" [e1,e2]
tr_lam v t e = Node "Lam" [Leaf v, t, e]
tr_var v = Node "Var" [Leaf v]

tr_tint = Node "TInt" []
tr_tarr t1 t2 = Node "TArr" [t1,t2]

dt1 = tc_evalview (tr_add (tr_int 1) (tr_int 2))
-- > dt1
-- Right ("3","(1+2)")

dt2 = tc_evalview (tr_app (tr_int 1) (tr_int 2))
-- > dt2
-- *** Exception: operator type is not an arrow: Int

dt4 = tc_evalview
  (tr_lam "x" tr_tint
   (tr_lam "y" (tr_tint `tr_tarr` tr_tint)
    (tr_lam "z" tr_tint (tr_var "y"))))

-- Untyped Church Numeral 2
exp_n2 =
  (tr_lam "f" (tr_tint `tr_tarr` tr_tint)
   (tr_lam "x" tr_tint
    (tr_app (tr_var "f") (tr_app (tr_var "f") (tr_var "x")))))

dt8 = tc_evalview
  (tr_app
   (tr_app exp_n2
    (tr_lam "x" tr_tint (tr_add (tr_var "x") (tr_var "x"))))
   (tr_int 11))

------------------------------------------------------------------------------
-- Exercise
--
-- * Add open recursion to typecheck
--
-- * Add Mul alternative
--
-- * Add booleans
--

------------------------------------------------------------------------------
-- Open recursion of typecheck

fix' f = f (fix' f)

read_t2 = fix' read_t2'

read_t2' :: (Tree -> Either String Typ) -> Tree -> Either String Typ
read_t2' f e = case e of
  Node "TInt" [] -> return $ Typ tint
  Node "TArr" [e1,e2] -> do
    Typ t1 <- read_t2' f e1
    Typ t2 <- read_t2' f e2
    return . Typ $ tarr t1 t2
  _ -> fail $ "Bad type expression: " ++ show e

read_t3 = fix' read_t3'

read_t3' :: (Tree -> Either String Typ) -> Tree -> Either String Typ
read_t3' f e = case e of
  Node "TBool" [] -> return $ Typ tbool
  _               -> read_t2' f e

{-|
Desired type for typecheck' is

> typecheck' ::
>  (Tree -> gamma -> Either String (Dynterm repr h))
>   Tree -> gamma -> Either String (Dynterm repr h)

So that we can write

> typecheck = fix typecheck'

Though, every call of findvar will require different type for gamma,
we need polymorphic function which could be applyed to all gammas.
Push the type signature for gamma inside the signature in G.

-}
type G =
  forall gamma repr h.
  (Var gamma h, Symantics repr, MulSYM repr, BoolSYM repr) =>
  (Tree, gamma) -> Either String (DynTerm repr h)

-- | Fix combinator for G.
fixG :: (G -> G) -> G
fixG f = f (fixG f)

typecheck' :: G -> G
typecheck' self (n,gamma) = case n of
  Node "Int" [Leaf str] -> case reads str of
    [(i,[])] -> return $ DynTerm tint (int i)
    _        -> fail $ "Bad Int literal: " ++ str
  Node "Add" [e1,e2] -> do
    DynTerm (TQ t1) d1 <- self (e1,gamma)
    DynTerm (TQ t2) d2 <- self (e2,gamma)
    case (as_int t1 d1, as_int t2 d2) of
      (Just t1',Just t2') -> return . DynTerm tint $ add t1' t2'
      (Nothing,_)         -> fail $ "Bad type of a summand: " ++ view_t t1
      (_,Nothing)         -> fail $ "Bad type of a summand: " ++ view_t t2
  Node "Var" [Leaf name] -> findvar name gamma
  Node "Lam" [Leaf name, etyp,ebody] -> do
    Typ ta <- read_t3 etyp
    DynTerm tbody body <- self (ebody,(VarDesc ta name, gamma))
    return $ DynTerm (tarr ta tbody) (lam body)
  Node "App" [e1,e2] -> do
    DynTerm (TQ t1) d1 <- self (e1,gamma)
    DynTerm (TQ t2) d2 <- self (e2,gamma)
    AsArrow _ arr_cast <- return $ as_arrow t1
    let errarr = fail $ "operator type is not an arrow: " ++ view_t t1
    (ta,tb,equ_t1ab) <- maybe errarr return arr_cast
    let df = equ_cast equ_t1ab d1
    case safe_gcast (TQ t2) d2 ta of
      Just da -> return . DynTerm tb $ app df da
      _       -> fail $ unwords ["Bad type of the application: ",
                                 view_t t1, "and", view_t t2]
  _ -> fail $ "Invalid Tree: " ++ show n

------------------------------------------------------------------------------
-- MulSYM

instance MulSYM CL where
  mul e1 e2 = CL $ mul (unCL e1) (unCL e2)

tr_mul e1 e2 = Node "Mul" [e1,e2]

typecheckMul :: G -> G
typecheckMul self (n,gamma) = case n of
  Node "Mul" [e1,e2] -> do
    DynTerm (TQ t1) d1 <- self (e1,gamma)
    DynTerm (TQ t2) d2 <- self (e2,gamma)
    case (as_int t1 d1, as_int t2 d2) of
      (Just t1',Just t2') -> return . DynTerm tint $ mul t1' t2'
      (Nothing,_) -> fail $ "Bad type of mul: " ++ view_t t1
      (_,Nothing) -> fail $ "Bad type of mul: " ++ view_t t2
  _ -> typecheck' self (n,gamma)

-- | Tying the knot.
typecheck2 :: G
typecheck2 = fixG typecheckMul

tc_evalview2 :: Tree -> Either String (String, String)
tc_evalview2 exp = do
  DynTerm tr d <- typecheck2 (exp,())
  let d' = unCL d
  return $ (show_as tr $ eval d', view d')

tc_evalint :: Tree -> Either String Int
tc_evalint exp = do
  DynTerm (TQ tr) d <- typecheck2 (exp,())
  case as_int tr (unCL d) of
    Just i  -> return $ eval i
    Nothing -> Left $ "Result type was not Int"

do1 = tc_evalview2 (tr_add (tr_int 1) (tr_int 2))
-- > do1
-- Right ("3","(1+2)")

do2 = tc_evalview2 (tr_app (tr_int 1) (tr_int 2))
-- > do2
-- *** Exception: operator type is not an arrow: Int

do4 = tc_evalview2
  (tr_lam "x" tr_tint
   (tr_lam "y" (tr_tint `tr_tarr` tr_tint)
    (tr_lam "z" tr_tint (tr_var "y"))))

-- Untyped Church Numeral 2
do8 = tc_evalview2
  (tr_app
   (tr_app exp_n2
    (tr_lam "x" tr_tint (tr_add (tr_var "x") (tr_var "x"))))
   (tr_int 11))

dm1 = tc_evalview2
  (tr_add (tr_mul (tr_int 2) (tr_int 3)) (tr_int 10))
-- > dm1
-- Right ("16","((2*3)+10)")

------------------------------------------------------------------------------
-- Adding BoolSYM

instance BoolSYM CL where
  bool b = CL $ bool b
  if_ pe te fe = CL $ if_ (unCL pe) (unCL te) (unCL fe)
  leq e1 e2 = CL $ leq (unCL e1) (unCL e2)

tr_bool b = Node "Bool" [Leaf (show b)]
tr_if e1 e2 e3 = Node "If" [e1,e2,e3]
tr_leq e1 e2 = Node "Leq" [e1,e2]

typecheckBool :: G -> G
typecheckBool self (n,gamma) = case n of
  Node "Bool" [Leaf str] -> case reads str of
    [(b,[])] -> return $ DynTerm tbool (bool b)
    _        -> fail $ "Bad Bool literal: " ++ str
  -- Node "If" [e1,e2,e3] -> do
  --   DynTerm (TQ tp) dp <- self (e1,gamma)
  --   DynTerm (TQ tt) dt <- self (e2,gamma)
  --   DynTerm (TQ tf) df <- self (e3,gamma)
  --   case as_bool tp dp of
  --     Just tp' -> return $ DynTerm (TQ tt) $ if_ dp dt df
  --     Nothing  -> fail $ "Bad If literal: " ++ view_t tp
  Node "Leq" [e1,e2] -> do
    undefined
  _ -> typecheckMul self (n,gamma)
