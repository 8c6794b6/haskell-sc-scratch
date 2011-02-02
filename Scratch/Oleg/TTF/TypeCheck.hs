{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/TypeCheck.hs>
--
module TypeCheck where

import Typ
import Control.Monad
import "mtl" Control.Monad.Error

import TTFdB -- hiding (main)

data Tree = Leaf String
          | Node String [Tree]
            deriving (Eq, Read, Show)

-- Old approach, could not find reference code for this, yet.
--
-- data DynTerm repr h = forall a. (Show a, Typeable a) => DynTerm (repr h a)

data DynTerm repr h = forall a. DynTerm (TQ a) (repr h a)

tdyn1 = DynTerm tint (lam z `app` (int 1))

tdyn1_eval = case tdyn1 of DynTerm tr t -> show_as tr $ eval t

tdyn1_view = case tdyn1 of DynTerm _ t -> view t

-- 
-- read_t should have written in the open recursion style, so that it would be 
-- extensible
 
read_t :: Tree -> Either String Typ
read_t (Node "TInt" []) = return $ Typ tint
read_t (Node "TArr" [e1,e2]) = do
  Typ t1 <- read_t e1
  Typ t2 <- read_t e2
  return . Typ $ tarr t1 t2
read_t tree = fail $ "Bad type expression: " ++ show tree  

type VarName = String
data VarDesc t = VarDesc (TQ t) VarName

class Var gamma h | gamma -> h where
  findvar :: Symantics repr => 
             VarName -> gamma -> Either String (DynTerm repr h)
  
asTypeRepr :: t -> repr h t -> repr h t
asTypeRepr _ = id

instance Var () () where 
  findvar name _ = fail $ "Unbound variable: " ++ name
  
instance Var gamma h => Var (VarDesc t, gamma) (t, h) where  
  findvar name (VarDesc tr name',_) | name == name' = 
    return $ DynTerm tr z
  findvar name (_,gamma) = do
    DynTerm tr v <- findvar name gamma
    return $ DynTerm tr (s v)

typecheck :: (Symantics repr, Var gamma h) =>
             Tree -> gamma -> Either String (DynTerm repr h)
typecheck (Node "Int" [Leaf str]) gamma =             
  case reads str of 
    [(n,[])] -> return $ DynTerm tint (int n)
    _        -> fail $ "Bad Int literal: " ++ str
    
typecheck (Node "Add" [e1, e2]) gamma = do    
  DynTerm (TQ t1) d1 <- typecheck e1 gamma
  DynTerm (TQ t2) d2 <- typecheck e2 gamma
  case (as_int t1 d1, as_int t2 d2) of
    (Just t1', Just t2') -> return . DynTerm tint $ add t1' t2'
    (Nothing,_)          -> fail $ "Bad type of a summand: " ++ view_t t1
    (_,Nothing)          -> fail $ "Bad type of a summand: " ++ view_t t2
    
typecheck (Node "Var" [Leaf name]) gamma = findvar name gamma    

typecheck (Node "Lam" [Leaf name, etyp, ebody]) gamma = do
  Typ ta <- read_t etyp
  DynTerm tbody body <- typecheck ebody (VarDesc ta name, gamma)
  return $ DynTerm (tarr ta tbody) (lam body)
  
typecheck (Node "App" [e1,e2]) gamma = do  
  DynTerm (TQ t1) d1 <- typecheck e1 gamma
  DynTerm (TQ t2) d2 <- typecheck e2 gamma
  AsArrow _ arr_cast <- return $ as_arrow t1
  let errarr = fail $ "operator type is not an arrow: " ++ view_t t1
  (ta,tb,equ_t1ab) <- maybe errarr return arr_cast
  let df = equ_cast equ_t1ab d1
  case safe_gcast (TQ t2) d2 ta of
    Just da -> return . DynTerm tb $ app df da
    _ -> fail $ unwords ["Bad types of the application:"
                        ,view_t t1, "and", view_t t2]
         
typecheck e gamma = fail $ "Invalid Tree: " ++ show e

tx1 = typecheck 
       (Node "Var" [Leaf "x"]) ()

tx2 = typecheck 
       (Node "Lam" [Leaf "x",Node "TInt" [],
		    Node "Var" [Leaf "x"]]) ()
tx3 = typecheck 
       (Node "Lam" [Leaf "x",Node "TInt" [],
	  Node "Lam" [Leaf "y",Node "TInt" [],
		    Node "Var" [Leaf "x"]]]) ()

tx4 = typecheck 
       (Node "Lam" [Leaf "x",Node "TInt" [],
	  Node "Lam" [Leaf "y",Node "TInt" [],
	    Node "Add" [Node "Int" [Leaf "10"],
			Node "Var" [Leaf "x"]]]]) ()

tx_view t = case t of
  Right (DynTerm _ t) -> view t
  Left err            -> error err

tx1_view = tx_view tx1

tx4_view = tx_view tx4
-- "(\\x0 -> (\\x1 -> (10+x0)))"

newtype CL h a = CL {unCL :: forall repr. Symantics repr => repr h a}

instance Symantics CL where
  int x     = CL $ int x
  add e1 e2 = CL $ add (unCL e1) (unCL e2)
  z         = CL z
  s v       = CL $ s (unCL v)
  lam e     = CL $ lam (unCL e)
  app e1 e2 = CL $ app (unCL e1) (unCL e2)
  
tc_evalview exp = do  
  DynTerm tr d <- typecheck exp ()
  let d' = unCL d
  return (show_as tr $ eval d', view d')
  
tr_int x = Node "Int" [Leaf $ show x]  
tr_add e1 e2 = Node "Add" [e1, e2]
tr_app e1 e2 = Node "App" [e1, e2]
tr_lam v t e = Node "Lam" [Leaf v, t, e]
tr_var v = Node "Var" [Leaf v]

tr_tint = Node "TInt" []
tr_tarr t1 t2 = Node "TArr" [t1,t2]

dt1 = tc_evalview (tr_add (tr_int 1) (tr_int 2))
dt2 = tc_evalview (tr_app (tr_int 1) (tr_int 2))
