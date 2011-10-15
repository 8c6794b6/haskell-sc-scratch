{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch written while reading:
/A tutorial implementation of a dependently typed lambda calculus/.

-}
module Scratch01 where

import Control.Monad (unless)

-- | `Term↑' replaced with TermI (inferable), and `Term↓' replaced with TermC
-- (checkable).
data TermI
  = Ann TermC Type  -- ^ Annotated terms
  | Bound Int       -- ^ Bound variables
  | Free Name       -- ^ Free variables
  | TermI :@: TermC -- ^ Application
  deriving (Show, Eq)

data TermC
  = Inf TermI
  | Lam TermC
  deriving (Show, Eq)

data Name
  = Global String
  | Local Int
  | Quote Int
    deriving (Show, Eq)

data Type
  = TFree Name
  | Fun Type Type
    deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

vfree :: Name -> Value
vfree = VNeutral . NFree

type Env = [Value]

evalI :: TermI -> Env -> Value
evalI t d = case t of
  Ann e _  -> evalC e d
  Free x   -> vfree x
  Bound i  -> d !! i
  e :@: e' -> vapp (evalI e d) (evalC e' d)

vapp :: Value -> Value -> Value
vapp v v' = case v of
  VLam f     -> f v'
  VNeutral n -> VNeutral (NApp n v')

evalC t d = case t of
  Inf i -> evalI i d
  Lam e -> VLam (\x -> evalC e (x:d))

data Kind = Star deriving (Show)

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name,Info)]

type Result a = Either String a

throwError :: String -> Result a
throwError = Left

kindC :: Context -> Type -> Kind -> Result ()
kindC g t Star = case t of
  TFree x -> case lookup x g of
    Just (HasKind Star) -> return ()
    Nothing             -> throwError "unknown identifier"
  Fun k k' -> do
    kindC g k Star
    kindC g k' Star

typeI0 :: Context -> TermI -> Result Type
typeI0 = typeI 0

typeI :: Int -> Context -> TermI -> Result Type
typeI i g t = case t of
  Ann e l -> do
    kindC g l Star
    typeC i g e l
    return l
  Free x -> case lookup x g of
    Just (HasType l) -> return l
    Nothing          -> throwError "unknown identifier"
  e :@: e' -> do
    g' <- typeI i g e
    case g' of
      Fun l l' -> do typeC i g e' l
                     return l'
      _        -> throwError "illegal application"

typeC :: Int -> Context -> TermC -> Type -> Result ()
typeC i g t l = case (t,l) of
  (Inf e,_)  -> do
    l' <- typeI i g e
    unless (l == l') (throwError "type mismatch")
  (Lam e,Fun f f') -> typeC (i+1) ((Local i, HasType f):g)
                      (substC 0 (Free (Local i)) e) f'
  _ -> throwError "type mismatch"

substI :: Int -> TermI -> TermI -> TermI
substI i r t = case t of
  Ann e l -> Ann (substC i r e) l
  Bound j -> if i == j then r else Bound j
  Free y  -> t
  e :@: e' -> substI i r e :@: substC i r e'

substC :: Int -> TermI -> TermC -> TermC
substC i r t = case t of
  Inf e -> Inf (substI i r e)
  Lam e -> Lam (substC (i+1) r e)

quote0 :: Value -> TermC
quote0 = quote 0

quote :: Int -> Value -> TermC
quote i v = case v of
  VLam f -> Lam (quote (i+1) (f (vfree (Quote i))))
  VNeutral n -> Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> TermI
neutralQuote i n = case n of
  NFree x  -> boundfree i x
  NApp n v -> neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> TermI
boundfree i n = case n of
  Quote k -> Bound (i - k - 1)
  x       -> Free x

-- | Same as 'id' function in Prelude
-- > id' == quote0 $ VLam (\x -> x)
id' = Lam (Inf (Bound 0))

-- | Same as 'const' function.
--
-- > const' == quote0 $ VLam (\x -> VLam (\y -> x))
const' = Lam (Lam (Inf (Bound 1)))

tfree = TFree . Global

free = Inf . Free . Global

term1 = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"
term2 = Ann const' (Fun (Fun (tfree "b") (tfree "b"))
                        (Fun (tfree "a")
                             (Fun (tfree "b") (tfree "b"))))
        :@: id' :@: free "y"

env1 = [ (Global "y", HasType (tfree "a"))
       , (Global "a", HasKind Star) ]

env2 = [ (Global "b", HasKind Star) ] ++ env1