{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Parsing patterns, take 7.
Rewriting pattern classes to take extra argument.
-}

module Parse7 where

import Control.Applicative
import System.Random
import System.Random.Shuffle

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Interpreter.R (gens,initialT,shiftT)
import Sound.SC3.Lepton.Pattern.ToOSC

import Scratch.Parse5 (E(..), Etree(..), etree, toE)
import Scratch.L

import qualified Data.Binary as Bin
import qualified Data.Map as M
import qualified Data.Traversable as T

------------------------------------------------------------------------------
-- Pattern classes with kind (p :: * -> * -> *)

class Pval p where
  pval :: (Bin.Binary a, Show a) => a -> p h a

class Plist p where
  plist :: (Bin.Binary a, Show a) => [a] -> p h a

class Prepeat p where
  prepeat :: (Bin.Binary a, Show a) => a -> p h a

class Pempty p where
  pempty :: p h a

class Pprim p where
  pdouble :: Double -> p h Double
  pint    :: Int -> p h Int
  pbool   :: Bool -> p h Bool

class Pappend p where
  pappend :: p h a -> p h a -> p h a

class Pconcat p where
  pconcat :: [p h a] -> p h a

class Prand p where
  prand :: p h Int -> [p h a] -> p h a

class Prange p where
  prange :: Random a => p h a -> p h a -> p h a

class Preplicate p where
  preplicate :: p h Int -> p h a -> p h a

class Pseq p where
  pseq :: p h Int -> [p h a] -> p h a

class Pforever p where
  pforever :: p h a -> p h a

class Pcycle p where
  pcycle :: [p h a] -> p h a

class Plc p where
  pz  :: p (a,h) a
  ps  :: p h a -> p (any,h) a
  lam :: p (a,h) b -> p h (a->[b])
  app :: p h (a->[b]) -> p h a -> p h b

class Psnew p where
  psnew :: String -> Maybe Int -> AddAction -> Int -> [(String, p r Double)]
           -> p r (ToOSC Double)

------------------------------------------------------------------------------
-- Sample terms

t01 = pval 1
t02 = plist [2,3,4,5]
t04 = pappend t01 t02
t05 = prand (pval 5) [t02,t01]
t06 = lam (pappend pz pz) `app` t05

------------------------------------------------------------------------------
-- For list pattern

instance Pval L where pval x = L $ \_ _ -> [x]

instance Plist L where plist xs = L $ \_ _ -> xs

instance Prepeat L where prepeat x = L $ \_ _ -> repeat x

instance Pempty L where pempty = L $ \_ _ -> []

instance Pprim L where
  pint x = L $ \_ _ -> [x]
  pdouble x = L $ \_ _ -> [x]
  pbool x = L $ \_ _ -> [x]

instance Pappend L where
  pappend a b = L $ \h g -> unL a h g ++ unL b h (snd (next g))

instance Pconcat L where
  pconcat xs = foldr1 pappend xs

instance Prand L where
  prand i xs = L $ \h g0 ->
    let g1 = snd (next g0)
        gs = take (sum $ unL i h g0) (gens g1)
        f x = let (j,_) = randomR (0,length xs-1) x in unL (xs!!j) h x
    in  concatMap f gs

instance Prange L where
  prange a b = L $ \h g0 ->
    let g1 = snd (next g0)
        g2 = snd (next g1)
    in  zipWith3 (\lo hi g' -> fst (randomR (lo,hi) g'))
        (unL a h g0) (unL b h g1) (gens g2)

instance Preplicate L where
  preplicate n p = L $ \h g ->
    let p' = concatMap (`replicate` p) (unL n h g)
    in  concat $ zipWith (\q g' -> unL q h g') p' (gens g)

instance Pseq L where
  pseq n = preplicate n . pconcat

instance Pforever L where
  pforever p = L $ \h g -> concatMap (unL p h) (gens g)

instance Pcycle L where
  pcycle ps = case ps of
    [] -> pempty
    _  -> pforever $ pconcat ps

instance Plc L where
  pz = L $ \(a,_) _ -> [a]
  ps v = L $ \(_,h) g -> unL v h g
  lam k = L $ \h g -> repeat (\x -> unL k (x,h) g)
  app e1 e2 = L $ \h g -> concat $ zipWith ($) (unL e1 h g) (unL e2 h g)

instance Psnew L where
  psnew def nid aa tid ms = ToOSC sn <$> ms' where
    sn = Snew def nid aa tid
    ms' = L $ \h g ->
      tail $ shiftT 0 $
      unL (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) h g

------------------------------------------------------------------------------
-- For viewing

newtype S h a = S {unS :: Int -> String}

toS :: S h a -> S h a
toS = id

view :: S h a -> String
view e = unS e 0

viewSs :: [S h a] -> Int -> String
viewSs ss n = case ss of
  [] -> "[]"; (t:ts) -> '[': unS t n ++ go ts
  where go us = case us of [] -> "]"; (v:vs) -> ',' : unS v n ++ go vs

instance Show (S h a) where show = view

instance Pval S where pval x = S $ \_ -> "pval " ++ show x
instance Plist S where plist xs = S $ \_ -> "plist " ++ showList xs ""
instance Pempty S where pempty = S $ \_ -> "pempty"
instance Prepeat S where prepeat x = S $ \_ -> "prepeat " ++ show x
instance Pprim S where
  pint x = S $ \_ -> "pint " ++ if x<0 then '(':show x++")" else show x
  pdouble x = S $ \_ -> "pdouble " ++ if x<0 then '(':show x++")" else show x
  pbool x = S $ \_ -> "pbool " ++ show x

instance Pappend S where
  pappend a b = S $ \h -> "pappend (" ++ unS a h ++ ") (" ++ unS b h ++ ")"

instance Pconcat S where
  pconcat xs = S $ \h -> "pconcat " ++ viewSs xs h

instance Preplicate S where
  preplicate n p = S $ \h -> concat ["preplicate (",unS n h,") (",unS p h,")"]

instance Pseq S where
  pseq n ps = S $ \h -> concat ["pseq (",unS n h,") ",viewSs ps h]

instance Pforever S where
  pforever p = S $ \h -> "pforever (" ++ unS p h ++ ")"

instance Pcycle S where
  pcycle ps = S $ \h -> "pcycle " ++ viewSs ps h

instance Prand S where
  prand i xs = S $ \h -> "prand (" ++ unS i h ++ ") " ++ viewSs xs h

instance Prange S where
  prange a b = S $ \h -> concat ["prange (",unS a h,") (",unS b h,")"]

instance Plc S where
  pz = S $ \h -> "x" ++ show (pred h)
  ps v = S $ \h -> unS v (pred h)
  lam k = S $ \h ->
    let x = "x" ++ show h
    in  "lam (\\" ++ x ++ " -> " ++ unS k (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

instance Psnew S where
  psnew def nid aa tid ms = S $ \_ ->
    "psnew " ++ unwords [show def,show nid,show aa,show tid] ++ " " ++ show ms

------------------------------------------------------------------------------
-- For expression tree

constE :: ByteString -> E h a
constE str = E $ \_ -> Node str []

primE :: Bin.Binary a => ByteString -> a -> E h b
primE str x = E $ \_ -> Node str [Leaf $ Bin.encode x]

unaryE :: ByteString -> E h a -> E h a
unaryE str e = E $ \h -> Node str [unE e h]

binaryE :: ByteString -> E h a -> E h a -> E h a
binaryE str e1 e2 = E $ \h -> Node str [unE e1 h, unE e2 h]

listE :: ByteString -> [E h a] -> E h a
listE str es = E $ \h -> Node str (map (flip unE h) es)

instance Pval E where pval = primE "pval"

instance Plist E where plist xs = primE "plist" xs

instance Pempty E where pempty = constE "pempty"

instance Prepeat E where prepeat = primE "prepeat"

instance Pprim E where
  pint x = primE "pint" x
  pdouble x = primE "pdouble" x
  pbool x = primE "pbool" x

instance Pappend E where pappend x y = binaryE "pappend" x y

instance Pconcat E where pconcat = listE "pconcat"

instance Prand E where
  prand i xs = E $ \h -> Node "prand" (unE i h:map (flip unE h) xs)

instance Preplicate E where
  preplicate n x = E $ \h -> Node "preplicate" [unE n h, unE x h]

instance Pseq E where
  pseq n xs = E $ \h -> Node "pseq" (unE n h:map (flip unE h) xs)

instance Pcycle E where pcycle = listE "pcycle"

instance Prange E where prange = binaryE "prange"

instance Plc E where
  pz = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  ps v = E $ \h -> unE v (pred h)
  lam k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = Node "TyDouble" []
        body = unE k (succ h)
    in  Node "lam" [v,ty,body]
  app x y = E $ \h -> Node "app" [unE x h, unE y h]

instance Psnew E where
  psnew def nid aa tid ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "psnew" $
        Leaf (Bin.encode def):Leaf (Bin.encode nid):
        Leaf (Bin.encode aa):Leaf (Bin.encode tid):ps'

mkParams :: Bin.Binary a => Int -> [(a, E h e)] -> [Etree]
mkParams h ps = case ps of
  []         -> []
  ((k,v):qs) -> Leaf (Bin.encode k):unE v h:mkParams h qs

-- instance (VarEnv g, h ~ Value g)=> Num (E h a) where
instance Num (E h a) where
  (+) = binaryE "+"
  (*) = binaryE "*"
  (-) = binaryE "-"
  negate = unaryE "negate"
  abs = unaryE "abs"
  signum = unaryE "signum"
  fromInteger a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromInteger a :: Double))]

-- instance (VarEnv g, h ~ Value g) => Fractional (E h a) where
instance Fractional (E h a) where
  (/) = binaryE "/"
  recip = unaryE "recip"
  fromRational a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromRational a :: Double))]

-- instance (VarEnv g, h ~ Value g) => Floating (E h a) where
instance Floating (E h a) where
  pi = constE "pi"
  exp = unaryE "exp"
  sqrt = unaryE "sqrt"
  log = unaryE "log"
  (**) = binaryE "**"
  logBase = binaryE "logBase"
  sin = unaryE "sin"
  tan = unaryE "tan"
  cos = unaryE "cos"
  asin = unaryE "asin"
  atan = unaryE "atan"
  acos = unaryE "acos"
  sinh = unaryE "sinh"
  tanh = unaryE "tanh"
  cosh = unaryE "cosh"
  asinh = unaryE "asinh"
  atanh = unaryE "atanh"
  acosh = unaryE "acosh"

-- instance (VarEnv g, h ~ Value g) => UnaryOp (E h a) where
instance UnaryOp (E h a) where
  ampDb = unaryE "ampDb"
  asFloat = unaryE "asFloat"
  asInt = unaryE "asInt"
  bitNot = unaryE "bitNot"
  cpsMIDI = unaryE "cpsMIDI"
  cpsOct = unaryE "cpsOct"
  cubed = unaryE "cubed"
  dbAmp = unaryE "dbAmp"
  distort = unaryE "distort"
  frac = unaryE "frac"
  isNil = unaryE "isNil"
  log10 = unaryE "log10"
  log2 = unaryE "log2"
  midiCPS = unaryE "midiCPS"
  midiRatio = unaryE "midiRatio"
  notE = unaryE "notE"
  notNil = unaryE "notNil"
  octCPS = unaryE "octCPS"
  ramp_ = unaryE "ramp_"
  ratioMIDI = unaryE "ratioMIDI"
  softClip = unaryE "softClip"
  squared = unaryE "squared"

------------------------------------------------------------------------------
-- Types and variable environments

-- XXX: Add r and h used in FromTree as parameter for Ty,
-- Make
--
-- > data Ty t where
-- >   TyDouble :: Num (r h Double) => Ty r h Double
-- >   TyInt    :: Num (r h Int) => Ty r h Int
-- >   ...
--
--
data Ty t where
  TyInt    :: Ty Int
  TyDouble :: Ty Double
  TyBool   :: Ty Bool
  TyToOSC  :: Ty a -> Ty (ToOSC a)
  TyArr    :: Ty a -> Ty b -> Ty (a->b)
  TyList   :: Ty a -> Ty [a]
  TyAny    :: a -> Ty a

data ExtTy = forall t. ExtTy (Ty t)

instance Show ExtTy where
  show (ExtTy ty) = "ExtTy (" ++ show ty ++ ")"

instance Show (Ty t) where
  show t = "Ty " ++ go t where
    go :: Ty a -> String
    go u = case u of
      TyInt     -> "Int"
      TyDouble  -> "Double"
      TyBool    -> "Bool"
      TyToOSC a -> "ToOSC (" ++ go a ++ ")"
      TyArr a b -> '(' : go a ++ " -> " ++ go b ++ ")"
      TyList a  -> '[' : go a ++ "]"
      _         -> "Other"

data Term r h where
  Term :: Ty t -> r h t -> Term r h

data Equal a b where
  Equal :: forall c. Equal c c

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy a b = case (a,b) of
  (TyInt,TyInt)             -> return Equal
  (TyDouble,TyDouble)       -> return Equal
  (TyBool,TyBool)           -> return Equal
  (TyToOSC a, TyToOSC b)    -> cmpTy a b >>= \Equal -> return Equal
  (TyArr a1 b1,TyArr a2 b2) -> do
    Equal <- cmpTy a1 a2
    Equal <- cmpTy b1 b2
    return Equal
  _                         -> Nothing

{-
-- VarEnv using TypeFamilies.

class VarEnv g where
  type Value g :: *
  findvar :: Plc r => Int -> g -> Either String (Term r (Value g))

data VarDesc t = VarDesc Int (Ty t)

instance VarEnv () where
  type Value () = ()
  findvar _ _ = Left "Variable unbound"

instance VarEnv g => VarEnv (VarDesc t,g) where
  type Value (VarDesc t,g) = (t,Value g)
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

instance VarEnv g => VarEnv ((Int,Ty t),g) where
  type Value ((Int,Ty t),g) = (t,Value g)
  findvar i ((j,t),g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term u v) -> return $ Term u (ps v)

type FromTree r =
  forall t g h.
  ( Pval r, Plist r, Pempty r, Prepeat r, Pprim r
  , Pappend r, Pconcat r, Preplicate r, Pseq r, Pforever r, Pcycle r
  , Prand r, Prange r
  , Psnew r
  , Plc r
  , VarEnv g, h ~ Value g
  , Bin.Binary t, Show t, Random t
  , Num (r h t), Num (r h Int), Num (r h Double), Num (r h (ToOSC Double))
  ) => (Etree,Ty t,g) -> Either String (Term r h)
  -- (
  -- , VarEnv g, h ~ Value g)
  -- => (Etree,g) -> Either String (Term r h)

-}

-- VarEnv using FunctionalDependency.
-- Rank-N type level recursion only works with FunctionalDependency version?

class VarEnv g h | g -> h where
  findvar :: (Plc r) => Int -> g -> Either String (Term r h)

data VarDesc t = VarDesc Int (Ty t)

instance VarEnv () () where
  findvar _ _ = Left "Variable unbound"

instance VarEnv g h => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

------------------------------------------------------------------------------
-- Deserializer

type FromTree r =
  forall g h t any.
  ( Pval r, Plist r, Pempty r, Prepeat r, Pprim r
  , Pappend r, Pconcat r, Preplicate r, Pseq r, Pforever r, Pcycle r
  , Prand r, Prange r
  , Psnew r
  , Plc r, VarEnv g h
  , Bin.Binary t, Show t, Random t
  --, Num (r h t), Num (r h Int), Num (r h Double), Num (r h (ToOSC Double))
  ) => (Etree,Ty t,g) -> Either String (Term r h)

fixFT :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixFT f = f (fixFT f)

-- fromTree :: forall r h g. VarEnv g h => FromTree r h g
-- fromTree = fixFT fromTreeO

fromTreeO :: forall r.FromTree r -> FromTree r
fromTreeO self (e,t,g) = case e of
  Node "psnew" (Leaf def:Leaf nid:Leaf aa:Leaf tid:ps) -> do
    let def' = Bin.decode def
        nid' = Bin.decode nid
        aa'  = Bin.decode aa
        tid' = Bin.decode tid
    ps' <- unParams ps
    return $ Term (TyToOSC TyDouble) $ psnew def' nid' aa' tid' ps'
  _ -> fromTreeE self (e,t,g)
  where
    unParams ps = case ps of
      []            -> return []
      (Leaf k:v:qs) -> do
        (Term TyDouble r :: Term r any) <- fixFT fromTreeE (v,TyDouble,g)
        rest <- unParams qs
        return $ (Bin.decode k,r):rest

fromTreeE :: forall r. FromTree r -> FromTree r
fromTreeE self (e,t,g) = case e of
  Node "pval" [Leaf x] -> return $ Term t (pval $ Bin.decode x)
  Node "plist" [Leaf x] -> return $ Term t (plist $ Bin.decode x)
  Node "pempty" [] -> return $ Term t pempty
  Node "prepeat" [Leaf x] -> return $ Term t (prepeat $ Bin.decode x)
  Node "pint" [Leaf x] -> return $ Term TyInt (pint (Bin.decode x))
  Node "pdouble" [Leaf x] -> return $ Term TyDouble (pdouble (Bin.decode x))
  Node "pbool" [Leaf x] -> return $ Term TyBool (pbool (Bin.decode x))
  Node "pappend" [e1,e2] -> do
    Term t1 v1 <- self (e1,t,g)
    Term t2 v2 <- self (e2,t,g)
    case (cmpTy t t1,cmpTy t t2) of
      (Just Equal,Just Equal) -> return $ Term t (pappend v1 v2)
  Node "pconcat" es -> do
    ts <- fromTreeEList (es,t,g)
    return $ Term t (pconcat ts)
  Node "preplicate" [e1,e2] -> do
    Term TyInt v1 <- self (e1,TyInt,g)
    Term t v2 <- self (e2,t,g)
    return $ Term t (preplicate v1 v2)
  Node "pseq" (e1:es) -> do
    Term TyInt v1 <- self (e1,TyInt,g)
    vs <- fromTreeEList (es,t,g)
    return $ Term t (pseq v1 vs)
  Node "pforever" [e1] -> do
    Term t v1 <- self (e1,t,g)
    return $ Term t (pforever v1)
  Node "pcycle" es -> do
    vs <- fromTreeEList (es,t,g)
    return $ Term t (pcycle vs)
  Node "prand" (e1:es) -> do
    Term TyInt v1 <- self (e1,TyInt,g)
    vs <- fromTreeEList (es,t,g)
    return $ Term t (prand v1 vs)
  Node "prange" [e1,e2] -> do
    Term t1 v1 <- self (e1,t,g)
    Term t2 v2 <- self (e2,t,g)
    case (cmpTy t t1,cmpTy t t2) of
      (Just Equal,Just Equal) -> return $ Term t (prange v1 v2)
  Node "var" [Leaf x] -> findvar (Bin.decode x) g
  Node "lam" [Leaf v,ty,body] -> do
    let v' = Bin.decode v :: Int
    ExtTy argty <- treeToTy ty
    Term bodyty bodyval <- self (body,t,((VarDesc v' argty),g))
    return $ Term (TyArr argty (TyList bodyty)) (lam bodyval)
  Node "app" [e1,e2] -> do
    Term (TyArr bndty (TyList bodyty)) e1' <- self (e1,t,g)
    Term argty e2' <- self (e2,t,g)
    case cmpTy bndty argty of
      Just Equal -> return $ Term bodyty (app e1' e2')
  -- _ -> fromTreeN self (e,t,g)
  where
    fromTreeEList (es,t,d) = case es of
      []     -> return []
      (x:xs) -> do
        Term t1 r1 <- self (x,t,d)
        case cmpTy t t1 of
          Just Equal -> (r1:) <$> fromTreeEList (xs,t,d)

-- fromTreeN :: forall r. FromTree r -> FromTree r
-- fromTreeN self (e,t,g) = case e of
--   Node "+" [e1,e2] -> do
--     Term t1 v1 <- self (e1,t,g)
--     Term t2 v2 <- self (e2,t,g)
--     case (cmpTy t t1, cmpTy t t2) of
--       (Just Equal,Just Equal) -> return $ Term t (v1 + v2)

treeToTy :: Etree -> Either String ExtTy
treeToTy e = case e of
  Node "TyInt" [] -> return $ ExtTy TyInt
  Node "TyDouble" [] -> return $ ExtTy TyDouble
  Node "TyArr" [e1,e2] -> do
    ExtTy t1 <- treeToTy e1
    ExtTy t2 <- treeToTy e2
    return $ ExtTy $ TyArr t1 t2

------------------------------------------------------------------------------
-- Tests

-- t2s :: Etree -> String
-- t2s e = case fromTree (e,TyToOSC TyDouble,()) of
--   Right (Term _ e') -> view e'
--   Left err          -> err

-- t2sD :: Etree -> String
-- t2sD e = case fixFT fromTreeE (e,TyDouble,()) of
--   Right (Term _ e') -> view e'
--   Left err          -> err

-- Use of scoped type variable make difference to dumped core.
-- t2rio :: Etree -> IO ()
-- t2rio e = case fromTree (e,TyDouble,()) of
--   Right (Term TyDouble e' :: Term L h) -> mapM_ print =<< runLIO e'
--   Left err -> error err

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [pval 60, prand (pval 1) [pval 63, pval 65]
       ,pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("amp", prepeat 0.1)
  ,("freq", midiCPS pspeFreq)]

pspe2 =
  let dt = 0.52 in
  lam (psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat (dt/4))
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS (pconcat [pz-12,pz,pz+12,pz]))])
  `app` pspeFreq
