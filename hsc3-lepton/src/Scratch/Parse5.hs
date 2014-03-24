{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns, take 5.

Attampt to add lambda calculus class in deserializer without modifying
existing classes for pattern expressions.

Most pattern expression classes has kind (p :: * -> *) in its constraings.
Lambda calculus pattern, Plc has (p :: * -> * -> *), to hold extra environment
for holding variable environment that could looked up later with de buruijn
indice.  This module tries to extend deserializer written in Parse4, with
mixing class instance constraints for pattern expressinons and Plc to same
term.

-}
module Scratch.Parse5 where

import Data.ByteString.Lazy (ByteString)
import Data.Data
import Data.Word (Word8)

import Control.Applicative
import System.Random

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.Etree
import Scratch.L

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Binary as Bin

-- | Newtype wrapper for converting to expression tree.
newtype E h a = E {unE :: Int -> Etree}

toE :: E h a -> E h a
toE = id

etree :: E h a -> Etree
etree e = unE e 0

------------------------------------------------------------------------------
-- Pattern instances

instance Pval (E h) where pval = primE "pval"

instance Plist (E h) where
  plist xs = E $ \_ -> Node "plist" [Leaf $ Bin.encode xs]

instance Prepeat (E h) where prepeat = primE "prepeat"

instance Pempty (E h) where pempty = constE "pempty"

instance Pappend (E h) where pappend = binE "pappend"

instance Pconcat (E h) where pconcat = listE "pconcat"

instance Preplicate (E h) where
  preplicate p1 p2 = E $ \h -> Node "preplicate" [unE p1 h, unE p2 h]

instance Pseq (E h) where
  pseq pn ps = E $ \h -> Node "pseq" (unE pn h:map (flip unE h) ps)

instance Pcycle (E h) where pcycle = listE "pcycle"

instance Pforever (E h) where pforever = unaryE "pforever"

instance Prandom (E h) where prandom = constE "prandom"

instance Prange (E h) where prange = binE "prange"

instance Prand (E h) where
  prand pn ps = E $ \h -> Node "prand" (unE pn h:map (flip unE h) ps)

instance Pchoose (E h) where
  pchoose pn ps = E $ \h -> Node "pchoose" (unE pn h:map (flip unE h) ps)

instance Pshuffle (E h) where pshuffle = listE "pshuffle"

-- XXX: Add ptakeT, pdropT.
-- XXX: Add plam and papp, rewrite with de bruijn?

instance Psnew (E h) where
  psnew d n a t ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "psnew" $
        Leaf (Bin.encode d):Leaf (Bin.encode n):
        Leaf (Bin.encode a):Leaf (Bin.encode t):ps'

instance Pnset (E h) where
  pnset t ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "pnset" $ Leaf (Bin.encode t):ps'

instance Mergable (E h a) where merge = binE "merge"

instance Pmerge (E h) where pmerge = binE "pmerge"

instance Ppar (E h) where ppar = listE "ppar"

instance Pfsm (E h) where
  pfsm is ps = E $ \h ->
    let ps' = f ps
        f qs = case qs of
          []     -> []
          ((r,js):rest) -> unE r h : Leaf (Bin.encode js) : f rest
    in  Node "pfsm" (Leaf (Bin.encode is):ps')

-- XXX: Fixed to TyDouble
instance Plc E where
  pz   = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  ps v = E $ \h -> unE v (pred h)
  lam k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = Node "TyDouble" []
        body = unE k (succ h)
    in  Node "lam" [v,ty,body]
  app e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]

-- XXX: Fixed to TyDouble here also.
instance Plc2 E where
  z2   = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  s2 v = E $ \h -> unE v (pred h)
  lam2 k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = Node "TyDouble" []
        body = unE k (succ h)
    in  Node "lam" [v,ty,body]
  app2 e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]

mkParams :: Bin.Binary a => Int -> [(a, E h e)] -> [Etree]
mkParams h ps = case ps of
  []         -> []
  ((k,v):qs) -> Leaf (Bin.encode k):unE v h:mkParams h qs

------------------------------------------------------------------------------
-- Helper functions

constE :: ByteString -> E h a
constE str = E $ \_ -> Node str []

primE :: Bin.Binary a => ByteString -> a -> E h a
primE str a = E $ \_ -> Node str [Leaf $ Bin.encode a]

binE :: ByteString -> E h a -> E h a -> E h a
binE op e1 e2 = E $ \h -> Node op [unE e1 h, unE e2 h]

unaryE :: ByteString -> E h a -> E h a
unaryE op e = E $ \h -> Node op [unE e h]

listE :: ByteString -> [E h a] -> E h a
listE str es = E $ \h -> Node str (map (flip unE h) es)

------------------------------------------------------------------------------
-- Base classes

instance Show (E h a) where
  show e = show $ etree e

instance Eq (E h a) where
  e1 == e2 = show e1 == show e2

instance Ord (E h a) where
  compare _ _ = EQ

------------------------------------------------------------------------------
-- Numeric classes

-- instance Num (E h a) where
--   (+) = binE "+"
--   (*) = binE "*"
--   (-) = binE "-"
--   negate = unaryE "negate"
--   abs = unaryE "abs"
--   signum = unaryE "signum"
--   fromInteger a = E $ \h ->
--     Node "pval" [Leaf (Bin.encode (fromInteger a :: Double))]

-- instance Fractional (E h a) where
--   (/) = binE "/"
--   recip = unaryE "recip"
--   fromRational a = E $ \h ->
--     Node "pval" [Leaf (Bin.encode (fromRational a :: Double))]

-- instance Floating (E h a) where
--   pi = constE "pi"
--   exp = unaryE "exp"
--   sqrt = unaryE "sqrt"
--   log = unaryE "log"
--   (**) = binE "**"
--   logBase = binE "logBase"
--   sin = unaryE "sin"
--   tan = unaryE "tan"
--   cos = unaryE "cos"
--   asin = unaryE "asin"
--   atan = unaryE "atan"
--   acos = unaryE "acos"
--   sinh = unaryE "sinh"
--   tanh = unaryE "tanh"
--   cosh = unaryE "cosh"
--   asinh = unaryE "asinh"
--   atanh = unaryE "atanh"
--   acosh = unaryE "acosh"

-- instance UnaryOp (E h a) where
--   ampDb = unaryE "ampDb"
--   asFloat = unaryE "asFloat"
--   asInt = unaryE "asInt"
--   bitNot = unaryE "bitNot"
--   cpsMIDI = unaryE "cpsMIDI"
--   cpsOct = unaryE "cpsOct"
--   cubed = unaryE "cubed"
--   dbAmp = unaryE "dbAmp"
--   distort = unaryE "distort"
--   frac = unaryE "frac"
--   isNil = unaryE "isNil"
--   log10 = unaryE "log10"
--   log2 = unaryE "log2"
--   midiCPS = unaryE "midiCPS"
--   midiRatio = unaryE "midiRatio"
--   notE = unaryE "notE"
--   notNil = unaryE "notNil"
--   octCPS = unaryE "octCPS"
--   ramp_ = unaryE "ramp_"
--   ratioMIDI = unaryE "ratioMIDI"
--   softClip = unaryE "softClip"
--   squared = unaryE "squared"

------------------------------------------------------------------------------
-- Type representations

data Ty t where
  TyInt    :: Ty Int
  TyDouble :: Ty Double
  TyToOSC  :: Ty a -> Ty (ToOSC a)
  TyArr    :: Ty a -> Ty b -> Ty (a->b)
  TyList   :: Ty a -> Ty [a]

instance Show (Ty t) where
  show t = "Ty " ++ go t where
    go :: Ty a -> String
    go u = case u of
      TyInt -> "Int"
      TyDouble -> "Double"
      TyToOSC a -> "ToOSC (" ++ go a ++ ")"
      TyArr a b -> "(" ++ go a ++ " -> " ++ go b ++ ")"
      TyList a  -> '[' : go a ++ "]"

data Equal a b where
  Equal :: Equal c c

instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy t1 t2 = case (t1,t2) of
  (TyInt,TyInt)       -> Just Equal
  (TyDouble,TyDouble) -> Just Equal
  (TyToOSC a, TyToOSC b) -> do
    Equal <- cmpTy a b
    return Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal
  _ -> Nothing

------------------------------------------------------------------------------
-- Deserialize

class Pprim p where
  pdouble :: Double -> p Double
  pint    :: Int -> p Int
  pbool   :: Bool -> p Bool

data Term r h where
  Term :: Ty t -> r h t -> Term r h

data ExtTy = forall t. ExtTy (Ty t)

class Plc2 p where
  z2 :: p (a,h) a
  s2 :: p h a -> p (any, h) a
  lam2 :: p (a,h) b -> p h (a->[b])
  app2 :: p h (a->[b]) -> p h a -> p h b

type FromTree r =
  forall g h t.
  ( VarEnv g h , Plc2 r -- , Pappend (r h)
  , Show t, Bin.Binary t
  ) => (Etree,Ty t,g) -> Either String (Term r h)

class VarEnv g h | g -> h where
  findvar :: (Plc2 r) => Int -> g -> Either String (Term r h)

data VarDesc t = VarDesc Int (Ty t)

instance VarEnv () () where
  findvar _ _ = Left "Unbound variable"

instance VarEnv g h => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j    = return $ Term t z2
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (s2 v)

-- instance Pval (W h) where pval x = W $ pval x
-- instance Pappend (W h) where pappend x y = W $ pappend (unW x) (unW y)

-- | Fix function for FromTree.
-- fixE :: forall r h g. VarEnv g h => (FromTree r g h -> FromTree r g h) -> FromTree r g h
-- fixE f = f (fixE f)

fromTreeV :: forall r. FromTree r -> FromTree r
fromTreeV self (e,t,d) = case e of
  --
  -- XXX: Got stuck.
  --
  -- How to add Pappend constraints to (r h) in FromTree?
  -- h is varying environment used for VarEnv. Class constraint
  -- for Pappend needs to be defined for all possible VarEnv, (t,(t1,(t2...())
  --
  -- Node "pappend" [e1,e2] -> do
  --   Term t1 v1 <- self (e1,t,d)
  --   Term t2 v2 <- self (e2,t,d)
  --   case (cmpTy t t1,cmpTy t1 t2) of
  --     (Just Equal,Just Equal) -> return $ Term t (pappend v1 v2)
  --
  Node "var" [Leaf x] -> let x' = Bin.decode x :: Int in findvar x' d
  Node "lam" [Leaf x,ty,body] -> do
    let x' = Bin.decode x
    ExtTy argty <- tree2Ty ty
    Term bodyty body' <- self (body,t,((VarDesc x' argty),d))
    case cmpTy t bodyty of
      Just Equal -> return $ Term (TyArr argty (TyList bodyty)) (lam2 body')
  Node "app" [e1,e2] -> do
    (Term (TyArr bndty (TyList bodyty)) e1' :: Term r a) <- self (e1,t,d)
    (Term argty e2' :: Term r b) <- self (e2,t,d)
    case cmpTy argty bndty of
      Just Equal -> Right $ Term bodyty (app2 e1' e2')

fromTreeA self (e,t,d) = case e of
  Node "app" [e1,e2] -> do
    Term (TyArr bndty (TyList bodyty)) e1' <- self (e1,t,d)
    Term argty e2' <- self (e2,t,d)
    case cmpTy argty bndty of
      Just Equal -> Right $ Term bodyty (app2 e1' e2')

tree2Ty :: Etree -> Either String ExtTy
tree2Ty e = case e of
  Node "TyInt" [] -> return $ ExtTy TyInt
  Node "TyDouble" [] -> return $ ExtTy TyDouble

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [pval 60, prand (pval 1) [pval 63, pval 65]
       ,pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]
