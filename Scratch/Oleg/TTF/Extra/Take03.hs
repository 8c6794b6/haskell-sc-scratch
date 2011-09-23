{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 3.

Need a method in symantic class that can refer to particular variable.
That's one of the differences between TTF and TTFdB.
We need a haskell representation to denote that
`this thing X is a variable, which could be referrenced by Y'.

Is there any other approach than using de Bruijn indice? There exists:

* Nominal terms 
  <http://en.wikipedia.org/wiki/Nominal_terms_%28computer_science%29>

* Higher order representation
  <http://en.wikipedia.org/wiki/Higher-order_abstract_syntax>

See also De Bruijn index in wikipedia:

* <http://en.wikipedia.org/wiki/De_Bruijn_index>

-}
module Take03 where

import Take01 (Tree(..),ppTree,safeRead)
-- import Take02 (T(..),Ty(..),ExtTy(..),Equal(..),cmpTy)

class Sym r where
  int :: Int -> r h Int
  add :: r h Int -> r h Int -> r h Int
  z   :: r (a,h) a
  s   :: r h a -> r (any,h) a
  lam :: r (a,h) b -> r h (a->b)
  app :: r h (a->b) -> r h a -> r h b
  
se01 = add (int 1) (int 2)
se02 = lam (add z z)
se03 = lam (add (app z (int 1)) (int 2))
se04 = app se03 se02
se05 = lam (lam (add z (s z)))

newtype R h a = R {unR :: h -> a}
  
instance Sym R where  
  int x     = R $ const x
  add a b   = R $ \h -> unR a h + unR b h
  z         = R $ \(a,_) -> a
  s x       = R $ \(_,h) -> unR x h
  lam e     = R $ \h -> \x -> unR e (x,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)
  
eval :: R () a -> a
eval e = unR e ()

newtype S h a = S {unS :: Int -> String}

instance Sym S where
  int x     = S $ \_ -> show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " + " ++ unS e2 h ++ ")"
  z         = S $ \h -> "x" ++ show (pred h)
  s v       = S $ \h -> unS v (pred h)
  lam e     = S $ \h ->
    let x   = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS e (succ h) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++  unS e2 h ++ ")"
  
view :: S () a -> String
view e = unS e 0

------------------------------------------------------------------------------
-- Relating value and type of variable..

data Ty t where
  TyAny :: Ty a
  TyInt :: Ty Int
  TyArr :: Ty a -> Ty b -> Ty (a->b)
  
instance Show (Ty a) where
  show ty = case ty of
    TyAny -> "any"
    TyInt -> "Int"
    TyArr a b -> show a ++ " -> " ++ show b
    
data Equal a b where
  Equal :: Equal c c
  
instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy ta tb = case (ta,tb) of
  (TyAny,TyAny)             -> Just Equal
  (TyInt,TyInt)             -> Just Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1 
    Equal <- cmpTy a2 b2
    return Equal
  _                         -> Nothing
  
data ExtTy where
  ExtTy :: forall ty. Show ty => Ty ty -> ExtTy
  
instance Show ExtTy where
  show (ExtTy ty) = "ExtTy " ++ show ty

data Term con where
  Term :: Sym con => Ty ty -> con h ty -> Term con
  -- Term :: Sym con => ExtTy -> con h ty -> Term con h

data TyEnv g where                 
  Nil  :: TyEnv g
  Cons :: String -> Ty t -> TyEnv h -> TyEnv (h,t)
  
findv :: Sym r => String -> TyEnv g -> Term r
findv n e = case e of 
  Nil -> error $ "Variable not found: " ++ n
  Cons n' ty g
    | n == n'   -> Term ty z
    | otherwise -> case findv n g of Term ty v -> Term ty (s v)
                                     
-- class Var g h | g -> h where
--   findvar :: Sym r => String -> g -> Either String (Term r h)  
  
-- instance Var () () where
--   findvar name _ = fail $ "Unbound variable: " ++ name
                                     
-- data VarDesc t = VarDesc (Ty t) String

-- instance Var g h => Var (VarDesc t,g) (t,h) where  
--   findvar n (VarDesc ty n',_) | n == n' = return $ Term ty z
--   findvar n (_,g) = do
--     Term ty val <- findvar n g
--     return $ Term ty (s val)

------------------------------------------------------------------------------
-- Serializer
  
newtype Exp h a = Exp {unExp :: Int -> Tree}

toExp :: Exp h a -> Exp h a
toExp = id

instance Sym Exp where
  int x     = Exp $ \_ -> Node "int" [Leaf $ show x] 
  add e1 e2 = Exp $ \h -> Node "add" [unExp e1 h, unExp e2 h]
  z         = Exp $ \h -> Node "var" [Leaf $ show (pred h)]
  s x       = Exp $ \h -> unExp x (pred h)
  lam e     = Exp $ \h -> 
    let ty = Node "TyInt" []
    in  Node "lam" [Leaf (show h),ty,unExp e (succ h)]
  app e1 e2 = Exp $ \h -> Node "app" [unExp e1 h, unExp e2 h]

expr :: Exp h a -> Tree
expr e = unExp e 0

------------------------------------------------------------------------------
-- Deserializer

tree2ty :: Tree -> Either String ExtTy
tree2ty t = case t of
  Node "TyInt" [] -> return $ ExtTy TyInt
  Node "TyArr" [t1,t2] -> do
    ExtTy ty1 <- tree2ty t1
    ExtTy ty2 <- tree2ty t2
    return $ ExtTy (TyArr ty1 ty2)
    
-- findty :: String -> [(String,ExtTy)] -> forall ty h. Term ty h
-- findty n ((n',ExtTy ty):tys) 
--   | n == n'   = Term ty z
--   | otherwise = case findty n tys of Term ty v -> Term ty s 
    
-- data Typed thing = forall ty. Typed (Ty ty) (thing ty)
    
-- fromTree :: (Sym con, Var g h) => Tree -> g -> Either String (Term con)
fromTree t g = case t of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Term TyInt (int x')
    Left err -> Left $ "Error in reading int: " ++ err
  -- Node "add" [e1,e2] -> do 
  --   Term TyInt val1 <- fromTree e1 g
  --   Term TyInt val2 <- fromTree e2 g
  --   return $ Term TyInt (add val1 val2)
  -- Node "var" [Leaf n] -> findvar n g
  -- Node "var" [Leaf n] -> findvar n g
  -- Node "lam" [Leaf v,ty,body] -> do
  --   e@(ExtTy argty) <- tree2ty ty
  --   -- XXX: Not working.
  --   -- Term bodyty body' <- fromTree body ((VarDesc argty v),g)
  --   error $ show e
  --   -- return $ Term (TyArr argty bodyty) (lam body')

-- Works:
--   
-- > case fromTree (expr se01) () of Right (Term ty val) -> (view val, show ty)
-- ("(1 + 2)","Int")
-- 
    
-- from_tree_s :: Exp h a -> IO ()
-- from_tree_s t =
--   case fromTree (expr t) () of
--   -- case fromTree (expr t) [] of
--     Right (Term ty val)) -> putStrLn $ view val ++ " :: " ++ show ty 
  