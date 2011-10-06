{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
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

* Implement rest of pattern expressions (Pint, Pdouble, Pnset, etc)

* Move to Lepton and replace current pattern classes

-}
module Scratch.Parse8 where

import Data.ByteString.Lazy (ByteString)

import Data.Binary
import Sound.SC3

import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.E
import Scratch.EInstance2 (tyTree)
import Scratch.Etree (Etree(..),ppTree)
-- import Scratch.L2
import Scratch.L3
-- import Scratch.LInstance2 ()
import Scratch.PC02
import Scratch.S
import Scratch.SInstance2 ()
import Scratch.Term00
import Scratch.Type00
import Scratch.THHelper

{-|
Type synonym for expression deserializing function, for tying the knot
of higher-rank open recursion functions.
-}
type FromTree r =
  forall g h.
  ( Pint r, Pdouble r, Pappend r, Pconcat r
  , Preplicate r, Pseq r, Pforever r,Pcycle r
  , Prand r, Pshuffle r
  , Ptuple r, Plambda r, VarEnv g h, Pfsm r
  , Psnew r, Pnset r, Pmerge r, Ppar r, Pdrop r, Ptake r
  ) => (Etree,g) -> Either String (Term r h)

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

-- | Node matcher for OSC patterns.
fromTreeO :: forall r. FromTree r -> FromTree r
fromTreeO self (e,g) = case e of
  Node "psnew" (Leaf def:Leaf nid:Leaf aa:Leaf tid:ps) -> do
    let def' = decode def
        nid' = decode nid
        aa'  = decode aa
        tid' = decode tid
    ps' <- unParams ps
    return $ Term toscd $ psnew def' nid' aa' tid' ps'
  Node "pnset" (Leaf nid:ps) -> do
    let nid' = decode nid
    ps' <- unParams ps
    return $ Term toscd $ pnset nid' ps'
  Node "pmerge" [e1,e2] -> do
    Term (TyToOSC TyDouble) v1 <- self (e1,g)
    Term (TyToOSC TyDouble) v2 <- self (e2,g)
    return $ Term toscd $ pmerge v1 v2
  Node "ppar" es -> do
    vs <- $(listRec 'self 'toscd 'g) es
    return $ Term toscd $ ppar vs
  Node "ptakeT" [e1,e2] -> do
    Term TyDouble v1 <- self (e1,g)
    Term (TyToOSC TyDouble) v2 <- self (e2,g)
    return $ Term toscd $ ptakeT v1 v2
  Node "pdropT" [e1,e2] -> do
    Term TyDouble v1 <- self (e1,g)
    Term (TyToOSC TyDouble) v2 <- self (e2,g)
    return $ Term toscd $ pdropT v1 v2
  _ -> fromTreeE self (e,g)
  where
    toscd = TyToOSC TyDouble
    unParams ps = case ps of
      []            -> return []
      (Leaf k:v:qs) -> do
        (Term TyDouble r :: Term r any) <- fixFT fromTreeE (v,g)
        rest <- unParams qs
        return $ (decode k,r):rest

-- | Node matcher for composition patterns.
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
    vs <- $(listRec 'self 't1 'g) es
    return $ Term t1 $ pconcat (v1:vs)
  Node "preplicate" [e1,e2] -> do
    Term TyInt v1 <- self (e1,g)
    Term t2 v2 <- self (e2,g)
    return $ Term t2 $ preplicate v1 v2
  Node "pseq" (e0:e1:es) -> do
    Term TyInt v0 <- self (e0,g)
    Term t1 v1 <- self (e1,g)
    vs <- $(listRec 'self 't1 'g) es
    return $ Term t1 $ pseq v0 (v1:vs)
  Node "pforever" [e1] -> do
    Term t1 v1 <- self (e1,g)
    return $ Term t1 $ pforever v1
  Node "pcycle" (e1:es) -> do
    Term  t1 v1 <- self (e1,g)
    vs <- $(listRec 'self 't1 'g) es
    return $ Term t1 $ pcycle (v1:vs)

  -- Random patterns
  Node "prand" (e0:e1:es) -> do
    Term TyInt v0 <- self (e0,g)
    Term t1 v1 <- self (e1,g)
    vs <- $(listRec 'self 't1 'g) es
    return $ Term t1 $ prand v0 (v1:vs)
  Node "pshuffle" (e1:es) -> do
    Term t1 v1 <- self (e1,g)
    vs <- $(listRec 'self 't1 'g) es
    return $ Term t1 $ pshuffle (v1:vs)

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

  Node "pfsm" (Leaf is:e1:es) -> do
    let is' = decode is
    Term t1 v1 <- self (e1,g)
    let go zs = case zs of
           []             -> return []
           (x:Leaf js:xs) -> do
              let js' = decode js :: [Int]
              Term t2 v2 <- self (x,g)
              case cmpTy t1 t2 of
                Just Equal -> ((v2,js'):) `fmap` go xs
    vs <- go (e1:es)
    return $ Term t1 $ pfsm is' vs

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

-- | Node matcher for Double expressions.
fromTreeD :: forall r. FromTree r -> FromTree r
fromTreeD self (e,g) = case e of
  Node "pdouble" [Leaf x] ->
    return $ Term tdouble (pdouble $ decode x)
  Node "ppi" [] ->
    return $ Term tdouble ppi
  Node name [e1] ->
    $(dmatch1s 'name
      ["pdnegate","pdabs","pdsignum","precip","pexp","psqrt","plog"
      ,"psin","ptan","pcos","pasin","patan","pacos","psinh","pcosh","ptanh"
      ,"pasinh","pacosh","patanh","pampDb","pasFloat","pasInt","pbitNot"
      ,"pcpsMIDI","pcpsOct","pdbAmp","pdistort","pfrac","pisNil","plog10"
      ,"plog2","pmidiCPS","pmidiRatio","pnotE","pnotNil","poctCPS","pramp_"
      ,"pratioMIDI","psoftClip","psquared"]
      'self 'e1 'g 'delegate)
  Node name [e1,e2] -> do
    $(dmatch2s 'name
      ["+@","*@","-@","pdrange","/@","**@","plogBase"]
      'self 'e1 'e2 'g 'delegate)
  _ -> delegate
  where
    delegate = fromTreeI self (e,g)

-- | Node matcher for Int expressions.
fromTreeI :: forall r. FromTree r -> FromTree r
fromTreeI self (e,g) = case e of
  Node "pint" [Leaf x] -> return $ Term tint (pint $ decode x)
  Node name [e1] -> do
    case name of
      "pinegate" -> $(imatch1 'self 'e1 'g 'pinegate)
      "piabs" -> $(imatch1 'self 'e1 'g 'piabs)
      "pisignum" -> $(imatch1 'self 'e1 'g 'pisignum)
  Node name [e1,e2] -> do
    case name of
      "+!" -> $(imatch2 'self 'e1 'e2 'g '(+!))
      "*!" -> $(imatch2 'self 'e1 'e2 'g '(*!))
      "-!" -> $(imatch2 'self 'e1 'e2 'g '(-!))
      "pirange" -> $(imatch2 'self 'e1 'e2 'g 'pirange)

-- | Parse Node tree to type.
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
  Node "Any" [] -> return $ ExtTy TyAny

------------------------------------------------------------------------------
-- Deserializer with fixed types.

t2s :: Etree -> String
t2s e = case fromTree (e,()) of Right (Term _ e') -> view e'

e2s :: E h a -> String
e2s e = case fromTree (etree e,()) of Right (Term _ e') -> view e'

t2l :: Etree -> Either String (L () (ToOSC Double))
t2l e = case fromTree (e,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L ()) -> Right e'

e2l :: E () (ToOSC Double) -> Either String (L () (ToOSC Double))
e2l e = case fromTree (etree e,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L ()) -> Right e'

playT :: Etree -> IO ()
playT e = case fromTree (e,()) of
  Right (Term (TyToOSC TyDouble) e' :: Term L h) -> audition $ toL e'

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
       ,mkSpe (\x -> pmidiCPS x *@ pforever (pdouble 0.5))
       ,mkSpe (\x -> pmidiCPS x *@ pforever (pdouble 1.5))]

pspe3 =
  let d = pdouble
      mkspe f =
        psnew "speSynth" Nothing AddToTail 1
        [("dur", pforever (d 0.13))
        ,("amp", pforever (d 0.1))
        ,("freq", f pz)]
  in plam tdouble
     (ppar
      [mkspe pmidiCPS
      ,mkspe (\x -> pmidiCPS x *@ d 0.5)
      ,mkspe (\x -> pmidiCPS x *@ d 1.5)])
     `papp` pspeFreq
