{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Parsing patterns, take 8.
Using fixed types in pattern expression classes.
Result values are not having type class constraints.

At last, this approach seems working well enough for use in implementing
audible pattern sent to sequence server. So here's couple TODOs:

* Add TemplateHaskell helper for:
  * FromTree class constraints
  * Node expression matching

* Implement rest of pattern expressions (Pint, Pdouble, Pnset, etc)

* Move to Lepton and replace current pattern classes

-}

module Scratch.Parse8 where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import System.Random (Random)

import Data.Binary
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.EInstance2 (tyTree)
import Scratch.Etree (Etree(..),ppTree)
import Scratch.L
import Scratch.LInstance2 ()
import Scratch.PC02
import Scratch.Parse5 (E(..), etree, toE)
import Scratch.S
import Scratch.SInstance2 ()
import Scratch.Type00

data Term r h where
  Term :: Ty t -> r h t -> Term r h

class VarEnv g h | g -> h where
  findvar :: Plambda r => Int -> g -> Either String (Term r h)

data VarDesc t where
  VarDesc :: Int -> Ty t -> VarDesc t

instance VarEnv () () where
  findvar _ _ = Left "Variable unbound"

instance (VarEnv g h) => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

type FromTree r =
  forall g h.
  ( Pint r, Pdouble r, Pappend r, Pconcat r
  , Preplicate r, Pseq r, Pforever r,Pcycle r
  , Prand r, Pshuffle r, Ptuple r
  , Plambda r, VarEnv g h
  , Psnew r, Pmerge r, Ppar r
  ) => (Etree,g) -> Either String (Term r h)

{-|
When 'h' type variable in (Term r h) is not exposed in FromTree,
we need to write this recursion in let expression, when this recursion
was moved out to where clause, it need to use h for scoped type variable.
Can we avoid writing pattern class constraints? TemplateHaskell?
-}
type FromTreeList r =
  forall g h t.
  ( Pint r, Pdouble r, Pappend r, Pconcat r
  , Preplicate r, Pseq r, Pforever r,Pcycle r
  , Prand r, Pshuffle r, Ptuple r
  , Plambda r, VarEnv g h
  , Psnew r, Pmerge r, Ppar r
  ) => FromTree r -> Ty t -> ([Etree],g) -> Either String [r h t]

-- | Higher rank fixed point combinator for FromTree.
fixFT :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixFT f = f (fixFT f)

{-|

Using fixed type in result type of pattern expression classes.

How to add (Random t) constraint for t used in all 'Ty t'?
(Ty t) might be referencing function type when combined with lam.
In general, it is difficult to add class constraing to t, since
it is not exposed in type signature of FromTree type synonym.
Removed Random class constraints from pattern expression,
add separate class member functions for Int and Double types.

> Node "prange" [e1,e2] -> do
>   Term t1 v1 <- self (e1,g)
>   Term t2 v2 <- self (e2,g)
>   case cmpTy t1 t2 of
>     -- Need to ensure Random t1 => t1 here.
>     Just Equal -> return $ Term t1 (prange v1 v2)

At the moment, could not find out a way to add class constraints
to result type in target DSL expressions.

> fromTreeN :: forall r. FromTree r -> FromTree r
> fromTreeN self (e,g) = case e of
>   Node "padd" [e1,e2] -> do
>     Term t1 v1 <- self (e1,g)
>     Term t2 v2 <- self (e2,g)
>     case cmpTy t1 t2 of
>       -- v1 and v2 needs Num class constraints
>       Just Equal -> return $ Term t1 (v1 + v2)

In this fromTreeE function, all types are fixed, no explicit
type parameter passed to function.

-}
fromTree :: forall r. FromTree r
fromTree = fixFT fromTreeO

-- | Is there a way to avoid specifying type?
fromTreeList :: FromTreeList r
fromTreeList self t (xs,g) = case xs of
  [] -> return []
  (y:ys) -> do
    Term t' v <- self (y,g)
    case cmpTy t t' of
      Just Equal -> (v:) `fmap` fromTreeList self t (ys,g)

-- | Node pattern matcher for OSC patterns.
fromTreeO :: forall r. FromTree r -> FromTree r
fromTreeO self (e,g) = case e of
  Node "psnew" (Leaf def:Leaf nid:Leaf aa:Leaf tid:ps) -> do
    let def' = decode def
        nid' = decode nid
        aa'  = decode aa
        tid' = decode tid
    ps' <- unParams ps
    return $ Term (TyToOSC TyDouble) $ psnew def' nid' aa' tid' ps'
  Node "pmerge" [e1,e2] -> do
    Term (TyToOSC TyDouble) v1 <- self (e1,g)
    Term (TyToOSC TyDouble) v2 <- self (e2,g)
    return $ Term (TyToOSC TyDouble) $ pmerge v1 v2
  Node "ppar" es -> do
    vs <- fromTreeList self (TyToOSC TyDouble) (es,g)
    return $ Term (TyToOSC TyDouble) $ ppar vs
  _ -> fromTreeE self (e,g)
  where
    unParams ps = case ps of
      []            -> return []
      (Leaf k:v:qs) -> do
        (Term TyDouble r :: Term r any) <- fixFT fromTreeE (v,g)
        rest <- unParams qs
        return $ (decode k,r):rest

fromTreeE :: forall r. FromTree r -> FromTree r
fromTreeE self (e,g) = case e of
  -- List patterns
  Node "pappend" [e1,e2] -> do
    Term t1 v1 <- self (e1,g)
    Term t2 v2 <- self (e2,g)
    case cmpTy t1 t2 of
      Just Equal -> return $ Term t1 (pappend v1 v2)
  Node "pconcat" (e1:es) -> do
    Term t1 v1 <- self (e1,g)
    -- XXX: Explicit recursion without calling fromTreeList.
    -- let go xs = case xs of
    --       []     -> return []
    --       (y:ys) -> self (y,g) >>= \(Term t2 v2) -> case cmpTy t1 t2 of
    --         Just Equal -> (v2:) `fmap` go ys
    -- vs <- go es
    vs <- fromTreeList self t1 (es,g)
    return $ Term t1 $ pconcat (v1:vs)
  Node "preplicate" [e1,e2] -> do
    Term t1 v1 <- self (e1,g)
    Term t2 v2 <- self (e2,g)
    case cmpTy t1 tint of
      Just Equal -> return $ Term t2 $ preplicate v1 v2
  Node "pseq" (e0:e1:es) -> do
    Term t0 v0 <- self (e0,g)
    Term t1 v1 <- self (e1,g)
    vs <- fromTreeList self t1 (es,g)
    case cmpTy t0 tint of
      Just Equal -> return $ Term t1 $ pseq v0 (v1:vs)
  Node "pforever" [e1] -> do
    Term t1 v1 <- self (e1,g)
    return $ Term t1 $ pforever v1
  Node "pcycle" (e1:es) -> do
    Term  t1 v1 <- self (e1,g)
    vs <- fromTreeList self t1 (es,g)
    return $ Term t1 $ pcycle (v1:vs)
  -- Random patterns
  Node "prand" (e0:e1:es) -> do
    Term t0 v0 <- self (e0,g)
    Term t1 v1 <- self (e1,g)
    vs <- fromTreeList self t1 (es,g)
    case cmpTy t0 tint of
      Just Equal -> return $ Term t1 $ prand v0 (v1:vs)
  -- Composition patterns
  Node "pzip" [e1,e2] -> do
    Term t1 v1 <- self (e1,g)
    Term t2 v2 <- self (e2,g)
    return $ Term (ttup t1 t2) (pzip v1 v2)
  Node "pfst" [e1] -> do
    Term (TyTup t1 _) v1 <- self (e1,g)
    return (Term t1 (pfst v1))
  Node "psnd" [e1] -> do
    Term (TyTup _ t1) v1 <- self (e1,g)
    return $ Term t1 (psnd v1)
  Node "var" [Leaf x] -> findvar (decode x) g
  Node "plam" [Leaf v,ty,body] -> do
    let v' = decode v
    ExtTy argty <- treeToTy ty
    Term bty bval <- self (body,(VarDesc v' argty,g))
    return $ Term (tarr argty (tlist bty)) (plam argty bval)
  Node "papp" [e1,e2] -> do
    Term (TyArr bndty (TyList bodyty)) bval <- self (e1,g)
    Term aty aval <- self (e2,g)
    case cmpTy bndty aty of
      Just Equal -> return $ Term bodyty (papp bval aval)
  _ -> fromTreeD self (e,g)

fromTreeD :: forall r. FromTree r -> FromTree r
fromTreeD self (e,g) = case e of
  Node "pdouble" [Leaf x] ->
    return $ Term tdouble (pdouble $ decode x)
  Node "*@" [e1,e2] -> do
    Term TyDouble v1 <- self (e1,g)
    Term TyDouble v2 <- self (e2,g)
    return $ Term TyDouble $ v1 *@ v2
  Node "pdrange" [e1,e2] -> do
    Term TyDouble v1 <- self (e1,g)
    Term TyDouble v2 <- self (e2,g)
    return $ Term tdouble $ pdrange v1 v2
  Node "pmidiCPS" [e1] -> do
    Term TyDouble v1 <- self (e1,g)
    return $ Term TyDouble $ pmidiCPS v1
  _ -> fromTreeI self (e,g)

fromTreeI :: forall r. FromTree r -> FromTree r
fromTreeI self (e,g) = case e of
  Node "pint" [Leaf x] -> return $ Term tint (pint $ decode x)
  Node "+!" [e1,e2] -> do
    Term TyInt v1 <- self (e1,g)
    Term TyInt v2 <- self (e2,g)
    return $ Term TyInt $ v1 +! v2
  Node "pirange" [e1,e2] -> do
    Term TyInt v1 <- self (e1,g)
    Term TyInt v2 <- self (e2,g)
    return $ Term TyInt (pirange v1 v2)

treeToTy :: Etree -> Either String ExtTy
treeToTy t = case t of
  Leaf "Int" -> return $ ExtTy tint
  Leaf "Double" -> return $ ExtTy tdouble
  Node "List" [e] -> do
    ExtTy t <- treeToTy e
    return $ ExtTy $ tlist t
  Node "ToOSC" [e] -> do
    ExtTy t <- treeToTy e
    return $ ExtTy $ ttoosc t
  Node "Tup" [e1,e2] -> do
    ExtTy t1 <- treeToTy e1
    ExtTy t2 <- treeToTy e2
    return $ ExtTy $ ttup t1 t2
  Node "Arr" [e1,e2] -> do
    ExtTy t1 <- treeToTy e1
    ExtTy t2 <- treeToTy e2
    return $ ExtTy $ tarr t1 t2

------------------------------------------------------------------------------
-- Deserializer with fixed types.

t2s :: Etree -> String
t2s e = case fromTree (e,()) of Right (Term _ e') -> view e'

e2s :: E h a -> String
e2s e = case fromTree (etree e,()) of Right (Term _ e') -> view e'

t2l :: Etree -> Either String (L () (ToOSC Double))
t2l e = case fromTree (e,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L ()) -> Right e'

t2lio :: Etree -> IO ()
t2lio e = case fromTree (e,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L h) -> mapLIO_ print e'

------------------------------------------------------------------------------
-- Sample terms

prettyE = ppTree . etree

lam01 = papp (plam tdouble pz) (pdouble 1)
lam02 = papp (plam tint pz) (pint 1)

{-
Type of pdouble inside lambda expression differs from pdouble written
outside, defining twice, or we could have wrote pdouble directly,
without let bindings.
-}
p01 =
  let d=pdouble; i=pint
  in  plam tdouble (pseq (i 8) [pz, d 2, d 3])
  `papp`
  let d=pdouble
  in  pdrange (d 1) (d 10)

p02 =
  let x1=pfst pz; x2=pfst (psnd pz); x3=psnd (psnd pz)
      ty = ttup tdouble (ttup tdouble tdouble)
  in  plam ty (pconcat [x1,x2,x3])
  `papp`
  let d=pdouble; i=pint
  in pzip (pconcat (map d [1..4])) .
     pzip (pseq (i 4) [pdrange (d 1) (d 10)]) $
     pconcat (map d [100,75..0])

pspeFreq =
  let d=pdouble; i=pint in
  pcycle
    [pseq (pirange (i 0) (i 1))
       [pconcat $ map d [24,31,36,43,48,55]]
    ,pseq (pirange (i 2) (i 5))
       [d 60, prand (i 1) [d 63,d 65]
       ,d 67, prand (i 1) [d 70,d 72,d 74]]
    ,prand (pirange (i 3) (i 9)) (map d [74,75,77,79,81])]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever (pdouble 0.13))
  ,("amp", pforever (pdouble 0.1))
  ,("freq", pmidiCPS pspeFreq)]

pspe2 =
  let mkSpe f =
        psnew "speSynth" Nothing AddToTail 1
         [("dur", pforever (pdouble 0.13))
         ,("amp", pforever (pdouble 0.1))
         ,("freq", f pspeFreq)]
  in  ppar
       [mkSpe pmidiCPS
       ,mkSpe (\x -> pmidiCPS x *@ pforever (pdouble 0.25))
       ,mkSpe (\x -> pmidiCPS x *@ pforever (pdouble 3))]
