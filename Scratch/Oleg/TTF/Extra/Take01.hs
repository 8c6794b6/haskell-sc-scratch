{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

-}
module Take01 where

import Control.Applicative
import Control.Monad.State
import Data.Data
import Data.Typeable
import Text.Show.Functions
import Text.PrettyPrint hiding (int)
import qualified Data.Map as M

class Sym r where
  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  lam :: (r a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b

ea01 = add (int 1) (int 2)
ea02 = lam (\x -> add x x)
ea03 = lam (\x -> add (app x (int 1)) (int 2))
ea04 = app ea03 (lam (\x -> (add (add x x) x)))

------------------------------------------------------------------------------
-- Eval interpreter

newtype R a = R {unR :: a} deriving (Typeable)

toR :: R a -> R a
toR = id

eval :: R a -> a
eval = unR

instance Sym R where
  int x = R x
  add a b = R $ unR a + unR b
  lam f = R $ \x -> unR (f (R x))
  app f a = R $ (unR f) (unR a)

------------------------------------------------------------------------------
-- View interpreter

newtype S a = S {unS :: Int -> String} deriving (Typeable)

instance Show (S a) where
  show s = view s
  
view :: S a -> String
view e = unS e 0

toS :: S a -> S a
toS = id

instance Sym S where
  int x = S $ \_ -> show x
  add a b = S $ \h -> unwords ["("++unS a h,"+",unS b h++")"]
  lam e = S $ \h ->
    let x = "x" ++ show h
    in  unwords ["(\\"++x,"->",unS (e (S (\_ -> x))) (succ h) ++ ")"]
  app f e = S $ \h ->
    unwords ["("++unS f h, unS e h++")"]

------------------------------------------------------------------------------
-- Serialize

data Tree 
  = Leaf String 
  | Node String [Tree] 
  deriving (Data,Typeable,Eq,Show)
           
ppTree :: Tree -> Doc           
ppTree t = case t of
  Leaf s    -> text s 
  Node s ts -> text s <+> (vcat $ map (parens . ppTree) ts)


newtype E s = E {unE :: Int -> Tree} deriving (Typeable)

toE :: E a -> E a
toE = id

tree e = unE e 0

instance Sym E where
  int x = E $ \_ -> Leaf (show x)
  add a b = E $ \h -> Node "add" [unE a h,unE b h]
  lam e = E $ \h ->
    let x = Node "var" [Leaf (show h)]
    in  Node "lam" [x,unE (e (E (\_ -> x))) (succ h)]
  app f e = E $ \h -> Node "app" [unE f h, unE e h]

------------------------------------------------------------------------------
-- Deserialize, not working.

safeRead :: Read b => String -> Either String b
safeRead s = case reads s of
  [(x,"")] -> Right x
  _        -> Left $ "Failed reading: " ++ s

evalTree :: Sym r => Tree -> Either String (r Int)
evalTree t = evalStateT (fromTree t) M.empty

fromTree ::
  Sym r => Tree -> StateT (M.Map String (r Int)) (Either String) (r Int)
fromTree e = case e of
  Leaf x -> case safeRead x of
    Right x' -> return (int x')
    Left err -> error err
  Node "add" [e1,e2] -> do
    e1 <- fromTree e1
    e2 <- fromTree e2
    return $ add e1 e2
  Node "var" [Leaf v] -> do
    env <- get
    let unbound = error ("Unbound variable: " ++ v)
        val = maybe unbound id (M.lookup v env)
    return val
    
  -- XXX: Get stucked.
  -- Return type has `r (a -> b)`, instead of `r Int`.
  --
  -- Node "lam" [e] -> do
  --   e' <- fromTree e
  --   return $ \x -> e' x
    
-- fromTreeFun :: (Monad m, Sym r) => Tree -> m (r (a -> b))
fromTreeFun e = case e of    
  Node "lam" [v,b] -> do
    v' <- fromTreeFun v
    b' <- fromTreeFun b
    return $ lam undefined
    -- return $ lam (CL . b' . unCL)

{-

Then, how it will be when returned type was wrapped in concrete type?

-}
-- data SymTy h r
--   = TyInt (r Int)
--   | forall a. TyAny (r a)
--   | forall a b. TyLam (r (a -> b))
--   | forall a b. TyApp (r b)
--   | TyVar (SymTy h r)
    
data SymTy r h where    
  TyInt :: r Int -> SymTy r h
  TyLam :: r (a -> b) -> SymTy r h
  TyApp :: r b -> SymTy r h
  TyVar :: SymTy r h -> SymTy r h
  TyCL  :: CL h -> SymTy r h
  
-- data SymTy h r where
--   TyInt :: (r Int) -> SymTy h r
--   TyAny :: (r a) -> SymTy h r
--   TyLam :: (r (a -> b)) -> SymTy h r
--   TyApp :: (r b) -> SymTy h r
--   TyVar :: (SymTy h r) -> SymTy h r
    
{-

TyInt (int 3) :: Sym r => SymTy r h
TyLam (lam (\x -> x)) :: Sym r => SymTy r h
TyLam (lam (\x -> add x x)) :: Sym r => SymTy r h
TyApp (app (lam (\x -> x)) (int 10)) :: Sym r => SymTy r h
TyVar (TyInt (int 3)) :: Sym r => SymTy r h
TyVar (TyLam (lam (\x -> x))) :: Sym r => SymTy r h

-}

type Ty2E a = StateT (M.Map String a) (Either String) a

-- fromTree2 :: Sym r => Tree -> Ty2E (SymTy r h)
fromTree2 e = case e of
  Leaf x -> case safeRead x of
    Right x'  -> return (TyInt (int x'))
    Left err  -> error err
  Node "add" [e1,e2] -> do
    TyInt e1' <- fromTree2 e1
    TyInt e2' <- fromTree2 e2
    return $ TyInt $ add e1' e2'
  Node "var" [Leaf v] -> do
    env <- get
    let unbound = error ("Unbound variable:" ++ v)
        val = maybe unbound id (M.lookup v env)
    return $ TyVar val
  Node "lam" [Node "var" [Leaf v],e] -> do
    return $ TyLam (lam $ resolve (fromTree2 e) v)
    
  -- XXX: Get stucked.
  -- GHC complain with `Could not deduce (a1 ~ a)` for type of v'.  
  --   
  -- Node "app" [f,v] -> do
  --   f' <- fromTree2 f
  --   v' <- fromTree2 v
  --   case (f',v') of 
  --     (TyLam e1,TyAny e2) -> return $ TyApp $ app e1 e2

-- resolve :: (MonadState m, Sym r) => foralll a b. m (SymTy r h) -> r (a -> b)
resolve = undefined

data Sym r => SymTr r              
  = TrInt (r Int)
  | TrLam (r (SymTr r) -> r (SymTr r))
             
fromTree25 e = case e of             
  Leaf x -> case safeRead x of
    Right x' -> return $ TrInt (int x')
    Left  err -> error $ "Leaf: " ++ err
  Node "add" [e1,e2] -> do
    TrInt i1 <- fromTree25 e1
    TrInt i2 <- fromTree25 e2
    return $ TrInt (add i1 i2)
  Node "var" [Leaf v] -> do
    env <- get
    let unbound = error ("Unbound variable:" ++ v)
        val = maybe unbound id (M.lookup v env)
    return $ val
  -- Node "lam" [v,b] -> do
  --   val <- fromTree25 v
  --   bdy <- fromTree25 b
  --   return $ TrLam (lam bdy)
    
data SymT
  = TInt Int
  | TLam (SymT -> SymT)
  | TApp SymT SymT
    deriving Show
             
nfh :: SymT -> SymT
nfh e = case e of
  TInt _   -> e
  TLam f   -> TLam (nfh . f)
  TApp f a -> case whnf f of
    TLam f' -> nfh (f' a)
    f'      -> TApp (nfh f') (nfh a)

whnf e = case e of
  TInt _ -> e
  TLam _ -> e
  TApp f a -> case whnf f of
    TLam g -> whnf (g a)
    f'     -> TApp f' a

{- How about GADTs ? -}

data SymG where
  GInt :: Int -> SymG
  GVar :: String -> SymG
  GLam :: (forall (r :: * -> *) a b. r a -> r b) -> SymG
  GApp :: (forall (r :: * -> *) a b. (r (a->b),(r a))) -> SymG

g2int g = case g of
  GInt i -> return $ int i
  _      -> Left "Not a GInt"

g2lam g = case g of
  GLam f -> return $ lam f
  _      -> Left $ "Not a GLam"

g2app g = case g of
  GApp (f,a) -> return $ app f a
  _ -> Left "Not a GApp"
  
{- How about a newtype wrapper, with type rep data? -}  

newtype CL a = CL {unCL :: forall (r :: * -> *). Sym r => r a} deriving (Typeable)

instance Sym CL where
  int x = CL $ int x
  add a b = CL $ add (unCL a) (unCL b)
  -- XXX: lam :: (CL a -> CL b) -> CL (a -> b)
  lam e = lam e
  app f v = CL $ app (unCL f) (unCL v)

data Term r = forall a. (Show a, Sym r) => Term (TRep r) (r a)
  
data TRep r
  = TyI (r Int)
  | TyA (TRep r) (TRep r)
  
fromTree3 e = case e of  
  Leaf x -> case safeRead x of
    Right x'  -> return (Term (TyI (int x')) (int x'))
    Left err  -> error err
    
  Node "add" [e1,e2] -> do
    Term (TyI r1) e1' <- fromTree3 e1
    Term (TyI r2) e2' <- fromTree3 e2
    return $ Term (TyI (add r1 r2)) (add r1 r2)
    
  Node "var" [Leaf v] -> do
    env <- get
    let unbound = error ("Unbound variable: " ++ v)
        val = maybe unbound id (M.lookup v env)
    return $ val
    
  -- Node "lam" [Node "var" [Leaf v],bdy] -> do
  Node "lam" [var,bdy] -> do
    e1@(Term ty1 val1) <- fromTree3 var -- (Node "var" [Leaf v])
    e2@(Term ty2 val2) <- fromTree3 bdy
    -- let bdy' = lam (\x -> modify (M.insert v x) >> fromTree3 bdy)
    return $ Term (TyA ty1 ty2) $ lam undefined
    
  Node "app" [f,v] -> do
    return undefined
  
term_s :: Term S -> String
term_s (Term _ s) = view s

term_r_print :: Term R -> IO ()
term_r_print (Term _ r) = print $ eval r

from_term_rs e = evalState go M.empty where 
  go = do
    Term _ t <- fromTree3 e
    let t' = unCL t
        t'' = unCL t
    return $ (view t'', show $ eval t')
    
{- 
How about initial form? 
Can this help the deserialization?
-}
    
data IR h t where
  IInt :: Int -> IR h Int
  IAdd :: IR h Int -> IR h Int -> IR h Int
  IVar :: h t -> IR h t
  ILam :: (IR h t1 -> IR h t2) -> IR h (t1->t2)
  IApp :: IR h (t1->t2) -> IR h t1 -> IR h t2
  
eb01 = IAdd (IInt 1) (IInt 2)
eb02 = ILam (\x -> IAdd x x)
eb03 = ILam (\x -> IAdd (IApp x (IInt 1)) (IInt 2))

evalI :: IR R t -> t
evalI e = case e of
 IInt n     -> n
 IAdd e1 e2 -> evalI e1 + evalI e2
 IVar v     -> unR v
 ILam b     -> \x -> evalI (b . IVar . R $ x)
 IApp e1 e2 -> (evalI e1) (evalI e2)

viewI :: IR S t -> String
viewI e = viewI' e 0

viewI' :: IR S t -> Int -> String
viewI' e = case e of
  IInt n -> const $ show n
  IAdd e1 e2 -> \h ->
    "(" ++ viewI' e1 h ++ " + " ++ viewI' e2 h ++ ")"
  IVar v -> unS v 
  ILam e -> \h -> 
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ viewI' (e (IVar $ S (const x))) (succ h) ++ ")"
  IApp e1 e2 -> \h -> "(" ++ viewI' e1 h ++ " " ++ viewI' e2 h ++ ")"

instance Sym (IR h) where
  int = IInt
  add = IAdd
  lam = ILam
  app = IApp
  
f2i :: IR h t -> IR h t  
f2i = id

i2f :: Sym r => IR r t -> r t
i2f e = case e of
  IInt x     -> int x
  IAdd e1 e2 -> add (i2f e1) (i2f e2)
  IVar v     -> v
  ILam b     -> lam (\x -> i2f (b (IVar x)))
  IApp e1 e2 -> app (i2f e1) (i2f e2)
  
data IRes r
   = RInt (r Int)
   | RFun (IRes r) (IRes r)
     
class Ty t where     
  tint :: t Int
  tarr :: t a -> t b -> t (a->b)
  
newtype TyName s = TyName {unTyName :: String}

instance Ty TyName where
  tint = TyName "Int"
  tarr (TyName a) (TyName b) = TyName $ "(" ++ a ++ " -> " ++ b ++ ")"
  
newtype TyQ a = TyQ {unTyQ :: forall t. Ty t => t a}

instance Ty TyQ where
  tint = TyQ tint
  tarr (TyQ a) (TyQ b) = TyQ (tarr a b)
     
data ITerm r = forall t. (Sym r, Typeable1 r, Typeable t, Show t) => ITerm (r t)

tree_rs :: Tree -> (String, String)
tree_rs e = evalState go M.empty where
  go = do
    ITerm e' <- fromTreeI e
    let r = unCL e'
        s = unCL e'
    return (view s,show $ eval r)
    
iterm_s = case iterm1 () of ITerm t -> view t
iterm_r = case iterm1 () of ITerm t -> show $ eval t
    
-- iterm1 () = ITerm $ lam (\x -> x) `app` (int 1)
iterm1 () = ITerm (lam (\x -> x) `app` (int 1))

{-

Still, the problems are:

* Return type of t from lam is in higher order, (a->b), ((a->b)->c), etc.
* Reading from int fixing result type to Int. 

Need to avoid these in somewhat way. TypeCheck source code uses Typ
module and functional dependency to relate the compile time type
environment and runtime values. Can we do similar thing here, with IR
and TypeRep?

-}
  
-- fromTreeI
--   :: (Typeable1 h, Typeable (h t), Sym h)
--      => Tree -> (StateT (M.Map String (ITerm h)) (Either String) (ITerm h))
fromTreeI e = case e of
  Leaf x -> case safeRead x of
    Right x'  -> return (ITerm $ int x')
    Left err  -> error err
    
  Node "add" [e1,e2] -> do
    ITerm e1' <- fromTreeI e1
    ITerm e2' <- fromTreeI e2
    let Just g1 = gcast e1'
        Just g2 = gcast e2'
    return $ ITerm $ add g1 g2
  -- Node "var" [Leaf v] -> do
  --   env <- get
  --   let unbound = error ("Unbound variable:" ++ v)
  --       val = maybe unbound id (M.lookup v env)
  --   return $ val
  -- Node "lam" [v,e] -> do
  --   ITerm e' <- fromTreeI e
  --   let Just g = gcast2 e'
  --   return $ (ITerm $ lam g)
  
fromTreeI2 e = case e of
  Leaf x -> case safeRead x of
    Right x' -> return (IInt x')
    Left err -> error $ "Error in Leaf: " ++ show x
    
-- Incope style approach    
data UR 
  = UInt Int
  | UAdd UR UR
  | ULam String UR
  | UApp UR UR
    deriving (Show)    
             
-- newtype C t = forall 
-- newtype CL a = CL {unCL :: forall (r :: * -> *). Sym r => r a} 
--              deriving (Typeable)

data P t where
  VI :: Int -> P Int
  VF :: (P a -> P b) -> P (a->b)
  VE :: CL t -> P t
  
instance Sym P where  
  int x = VI x
  add (VI 0) e = e
  add e (VI 0) = e
  add (VI n1) (VI n2) = VI (n1+n2)
  add e1 e2 = VE (add (abstr e1) (abstr e2))
  lam = VF
  app (VF f) ea = f ea
  app (VE f) ea = VE (app f (abstr ea))
  
abstr :: P t -> CL t
abstr e = case e of
  VI i -> int i
  VF f -> lam (abstr . f . VE)
  VE x -> x
  
data DP = forall t. DP {unDP :: (P t)}

-- fromTreeP e = case e of
--   Leaf x -> case safeRead x of
--     Right x' -> return $ DP $ VI x'
--     Left err -> error $ "Failed in Leaf: " ++ err
--   Node "add" [e1,e2] -> do
--     f1' <- fromTreeP e1
--     f2' <- fromTreeP e2
--     return $ case (f1',f2') of
--       (DP (VI 0),g) -> g
--       (g,DP (VI 0)) -> g
--       (DP (VI g1),DP (VI g2)) -> DP $ VI (g1+g2)
--       (DP g1,DP g2) -> DP $ VE (add (abstr g1) (abstr g2))
