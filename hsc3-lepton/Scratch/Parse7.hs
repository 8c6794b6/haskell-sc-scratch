{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Scratch.Parse7 where

import Control.Applicative
import System.Random
import System.Random.Shuffle

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Interpreter.R (gens,initialT,shiftT)
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.L
import Scratch.Etree (Etree(..))
import Scratch.Parse5 (E(..), etree, toE)
import Scratch.Type00

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

class Pnum p where
  padd :: Num a => p h a -> p h a -> p h a
  pmul :: Num a => p h a -> p h a -> p h a
  psub :: Num a => p h a -> p h a -> p h a
  pnegate :: Num a => p h a -> p h a
  pabs :: Num a => p h a -> p h a
  psignum :: Num a => p h a -> p h a
  pfromInteger :: Num a => Integer -> p h a

class Pfractional p where
  pdiv :: Fractional a => p h a -> p h a -> p h a
  precip :: Fractional a => p h a -> p h a
  pfromRational :: Fractional a => Rational -> p h a

class Pfloating p where
  pexp :: Floating a => p h a -> p h a
  psqrt :: Floating a => p h a -> p h a
  plog :: Floating a => p h a -> p h a

class Punary p where
  pmidiCPS :: UnaryOp a => p h a -> p h a

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

instance Pnum L where
  padd = (+)
  pmul = (*)
  psub = (-)
  pnegate = negate
  pabs = abs
  psignum = signum
  pfromInteger = fromInteger

instance Punary L where
  pmidiCPS = midiCPS

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
  psnew def nid aa tid ms = S $ \h ->
    "psnew " ++ unwords [show def,show nid,show aa,show tid] ++ " " ++ unSs ms h
    where
      unSs ns i = case ns of
        [] -> "[]"
        ((k,v):ns') ->
          '[' : ("(" ++ show k ++ "," ++ unS v i ++ ")" ++ unSs' ns' i)
      unSs' os j = case os of
        [] -> "]"
        ((k,v):ps) -> ",(" ++ show k ++ "," ++ unS v j ++ ")" ++ unSs' ps j

instance Eq (S h a) where
  a == b = unS a 0 == unS b 0

instance Num (S h a) where
  a + b = S $ \h -> concat ["(",unS a h,") + (",unS b h,")"]
  a * b = S $ \h -> concat ["(",unS a h,") * (",unS b h,")"]
  a - b = S $ \h -> concat ["(",unS a h,") - (",unS b h,")"]
  negate e = S $ \h -> "negate (" ++ unS e h ++ ")"
  abs e = S $ \h -> "abs (" ++ unS e h ++ ")"
  signum e = S $ \h -> "signum (" ++ unS e h ++ ")"
  fromInteger n = S $ \_ -> "pval " ++ show (fromInteger n :: Int)

showFloating :: Show a => String -> a -> S h s
showFloating f x = S (const $ f ++ " (" ++ show x ++ ")")

instance Ord (S h a) where
  compare _ _ = EQ

instance Fractional (S h a) where
  a / b = S $ \h -> concat ["(",unS a h,") / (",unS b h,")"]
  recip = showFloating "recip"
  fromRational n = S $ \_ -> "pval " ++ show (fromRational n :: Double)

instance Floating (S h a) where
  pi = S (const "pi")
  exp = showFloating "exp"
  log = showFloating "log"
  sqrt = showFloating "sqrt"
  a ** b = S (const $ "(" ++ show a ++ ") ** (" ++ show b ++ ")")
  sin = showFloating "sin"
  tan = showFloating "tan"
  cos = showFloating "cos"
  asin = showFloating "asin"
  atan = showFloating "atan"
  acos = showFloating "acos"
  sinh = showFloating "sinh"
  tanh = showFloating "tanh"
  cosh = showFloating "cosh"
  asinh = showFloating "asinh"
  atanh = showFloating "atanh"
  acosh = showFloating "acosh"

instance UnaryOp (S h a) where
  ampDb = showFloating "ampDb"
  asFloat = showFloating "asFloat"
  asInt = showFloating "asInt"
  bitNot = showFloating "bitNot"
  cpsMIDI = showFloating "cpsMIDI"
  cpsOct = showFloating "cpsOct"
  cubed = showFloating "cubed"
  dbAmp = showFloating "dbAmp"
  distort = showFloating "distort"
  frac = showFloating "frac"
  isNil = showFloating "isNil"
  log10 = showFloating "log10"
  log2 = showFloating "log2"
  midiCPS = showFloating "midiCPS"
  midiRatio = showFloating "midiRatio"
  notE = showFloating "notE"
  notNil = showFloating "notNil"
  octCPS = showFloating "octCPS"
  ramp_ = showFloating "ramp_"
  ratioMIDI = showFloating "ratioMIDI"
  softClip = showFloating "softClip"
  squared = showFloating "squared"

instance Pnum S where
  padd = (+)
  pmul = (*)
  psub = (-)
  pnegate = negate
  pabs = abs
  psignum = signum
  pfromInteger = fromInteger

instance Punary S where
  pmidiCPS x = S $ \h -> "midiCPS (" ++ unS x h ++ ")"

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

instance Pnum E where
  padd = (+)
  pmul = (*)
  psub = (-)
  pnegate = negate
  pabs = abs
  psignum = signum
  pfromInteger = undefined

instance Pfractional E where
  pdiv = (/)
  precip = recip
  pfromRational = fromRational

instance Punary E where
  pmidiCPS = midiCPS

instance Num (E h a) where
  (+) = binaryE "+"
  (*) = binaryE "*"
  (-) = binaryE "-"
  negate = unaryE "negate"
  abs = unaryE "abs"
  signum = unaryE "signum"
  fromInteger a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromInteger a :: Double))]

instance Fractional (E h a) where
  (/) = binaryE "/"
  recip = unaryE "recip"
  fromRational a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromRational a :: Double))]

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
-- Variable environment for lambda calculus with de Bruijn indice

-- VarEnv using FunctionalDependency.
-- Rank-N type level recursion only works with FunctionalDependency version?

data Term r h where
  Term :: Ty t -> r h t -> Term r h

class VarEnv g h | g -> h where
  findvar :: (Plc r) => Int -> g -> Either String (Term r h)

data VarDesc t = VarDesc Int (Ty t)

instance VarEnv () () where
  findvar _ _ = Left "Variable unbound"

instance (VarEnv g h) => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

{-
-- VarEnv using TypeFamilies.

type family Vars g :: *
type instance Vars () = ()
type instance Vars (VarDesc t,g) = (t,Vars g)

-- ghci> :t undefined :: Vars (VarDesc t2,(VarDesc t1,(VarDesc t0,())))
-- undefined :: Vars (VarDesc t2,(VarDesc t1,(VarDesc t0,())))
--   :: (t2, (t1, (t0, ())))

class VarEnv g where
  findvar :: (Plc r, h ~ Vars g) => Int -> g -> Either String (Term r h)

data VarDesc t = VarDesc Int (Ty t)

instance VarEnv () where
  findvar _ _ = Left "Variable unbound"

instance (VarEnv g) => VarEnv (VarDesc t,g) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

type FromTree r =
  forall g h t.
  ( Pval r, Plist r, Pempty r, Prepeat r, Pprim r
  , Pappend r, Pconcat r, Preplicate r, Pseq r, Pforever r, Pcycle r
  , Prand r, Prange r
  , Psnew r
  , Plc r
  , VarEnv g, h ~ Vars g
  , Bin.Binary t, Show t, Random t
  -- , Num (r h t), Num (r h Int), Num (r h Double), Num (r h (ToOSC Double))
  ) => (Etree,Ty t,g) -> Either String (Term r h)

-}

------------------------------------------------------------------------------
-- Deserializer

type FromTree r =
  forall g h t.
  ( Pval r, Plist r, Pempty r, Prepeat r, Pprim r
  , Pappend r, Pconcat r, Preplicate r, Pseq r, Pforever r, Pcycle r
  , Prand r, Prange r
  , Psnew r
  , Plc r, VarEnv g h
  , Bin.Binary t, Show t, Random t, Num t, UnaryOp t
  , Pnum r, Punary r
  ) => (Etree,Ty t,g) -> Either String (Term r h)

fixFT :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixFT f = f (fixFT f)

fromTree :: forall r. FromTree r
fromTree = fixFT fromTreeO

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

fromTreeE :: FromTree r -> FromTree r
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
    -- XXX: Type of argument may vary.
    --
    -- For instance, when function was applyed from outside of psnew and
    -- pz, ps was used inside parameter, body is expecting Double.
    -- When pz, ps was used in ppar, argument type will be (ToOSC Double).
    -- Where to couple the type of argument and body?
    -- Or will it be better to remove this type passing from caller entirely?
    --
    -- Term argty e2' <- self (e2,t,g)
    Term argty e2' <- self (e2,TyDouble,g)
    case cmpTy bndty argty of
      Just Equal -> return $ Term bodyty (app e1' e2')
  _ -> fromTreeN self (e,t,g)
  where
    treeToTy e = case e of
      Node "TyInt" [] -> return $ ExtTy TyInt
      Node "TyDouble" [] -> return $ ExtTy TyDouble
      Node "TyArr" [e1,e2] -> do
        ExtTy t1 <- treeToTy e1
        ExtTy t2 <- treeToTy e2
        return $ ExtTy $ TyArr t1 t2
    fromTreeEList (es,t,d) = case es of
      []     -> return []
      (x:xs) -> do
        Term t1 r1 <- self (x,t,d)
        case cmpTy t t1 of
          Just Equal -> (r1:) <$> fromTreeEList (xs,t,d)

fromTreeN :: forall r. FromTree r -> FromTree r
fromTreeN self (e,t,g) = case e of
  Node "+" [e1,e2] -> do
    Term t1 v1 <- self (e1,t,g)
    Term t2 v2 <- self (e2,t,g)
    case (cmpTy t t1, cmpTy t t2) of
      (Just Equal,Just Equal) -> return $ Term t (padd v1 v2)
  Node "-" [e1,e2] -> do
    Term t1 v1 <- self (e1,t,g)
    Term t2 v2 <- self (e2,t,g)
    case (cmpTy t t1, cmpTy t t2) of
      (Just Equal,Just Equal) -> return $ Term t (psub v1 v2)
  Node "midiCPS" [e1] -> do
    Term t1 v1 <- self (e1,t,g)
    case cmpTy t t1 of
      Just Equal -> return $ Term t $ pmidiCPS v1

------------------------------------------------------------------------------
-- Tests

t2s :: Etree -> String
t2s e = case fromTree (e,TyToOSC TyDouble,()) of
  Right (Term _ e') -> view e'
  Left err          -> err

t2sD :: Etree -> String
t2sD e = case fixFT fromTreeE (e,TyDouble,()) of
  Right (Term _ e') -> view e'
  Left err          -> err

-- Use of scoped type variable make difference to dumped core.
t2rio :: Etree -> IO ()
t2rio e = case fromTree (e,TyToOSC TyDouble,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L h) -> mapM_ print =<< runLIO e'
  Left err -> error err

t2l :: Etree -> Either String (L () (ToOSC Double))
t2l e = case fromTree (e,TyToOSC TyDouble,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L ()) -> return e'
  Left err -> Left err

playE :: E () (ToOSC Double) -> IO ()
playE e = do
  let et = etree e
  print et
  case fromTree (et,TyToOSC TyDouble,()) of
    Right (Term (TyToOSC TyDouble) e' :: Term L ()) -> audition e'
    Left err -> putStrLn err

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
  lam (psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat 0.13)
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS (pconcat [pz-12,pz]))])
  `app` pspeFreq

p01 =
  psnew "speSynth" Nothing AddToTail 1
  [("dur",prepeat 0.0917)
  ,("amp",prepeat 0.1)
  ,("freq", lam (midiCPS (preplicate (pval 3) pz)) `app` pspeFreq)]

p02 =
  lam (psnew "speSynth" Nothing AddToTail 1
        [("dur",prepeat 0.0917)
        ,("amp",prepeat 0.1)
        ,("freq", midiCPS (preplicate (pval 3) pz))])
 `app` pspeFreq
