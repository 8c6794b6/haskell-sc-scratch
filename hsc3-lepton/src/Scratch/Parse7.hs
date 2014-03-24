{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
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

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.L
import Scratch.LInstance ()
import Scratch.S
import Scratch.Etree (Etree(..))
import Scratch.EInstance ()
import Scratch.Parse5 (E(..), etree, toE)
import Scratch.PC01
import Scratch.Type00

import qualified Data.Binary as Bin

------------------------------------------------------------------------------
-- Variable environment for lambda calculus with de Bruijn indice

-- VarEnv using FunctionalDependency.
-- Rank-N type level recursion only works with FunctionalDependency version?
--

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
  , Pnum r, Punary r
  , Plc r, VarEnv g h
  , Bin.Binary t, Show t, Random t, Num t, UnaryOp t
  ) => (Etree,Ty t,g) -> Either String (Term r h)

fixFT :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixFT f = f (fixFT f)

fromTree :: forall r. FromTree r
fromTree = fixFT fromTreeO

fromTreeO :: forall r. FromTree r -> FromTree r
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
    case cmpTy t1 t2 of
      Just Equal -> return $ Term t1 (pappend v1 v2)
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
    --
    -- XXX: Type of argument is not fixed.
    --
    -- For instance, when function was applyed from outside of psnew and
    -- pz, ps was used inside parameter, body is expecting Double.
    -- When pz, ps was used as one of the elements in ppar, argument
    -- type will be (ToOSC Double). Where to couple the type of argument
    -- and body? Or will it be better to remove this type passing from
    -- caller entirely?
    --
    Term (TyArr bndty (TyList bodyty)) e1' <- self (e1,t,g)
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

------------------------------------------------------------------------------
-- Sample terms

t01 = pval 1
t02 = plist [2,3,4,5]
t04 = pappend t01 t02
t05 = prand (pval 5) [t02,t01]
t06 = lam (pappend pz pz) `app` t05

-- pspeFreq =
--   pcycle
--     [prand (pval 1)
--        [pempty, plist [24,31,36,43,48,55]]
--     ,pseq (prange (pval 2) (pval 5))
--        [pval 60, prand (pval 1) [pval 63, pval 65]
--        ,pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
--     ,prand (prange (pval 3) (pval 9))
--        [pval 74,pval 75,pval 77,pval 79,pval 81]]

pspeFreq =
  let d=pdouble; i=pint in
  pcycle
    [prand (i 1)
       [pempty, pconcat $ map d [24,31,36,43,48,55]]
    ,pseq (prange (i 2) (i 5))
       [d 60, prand (i 1) [d 63,d 65]
       ,d 67, prand (i 1) [d 70,d 72,d 74]]
    ,prand (prange (i 3) (i 9)) (map d [74,75,77,79,81])]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("amp", prepeat 0.1)
  ,("freq", midiCPS pspeFreq)]

pspe2 =
  lam (psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat 0.13)
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS (pconcat [pz-12,pz,pz+12]))])
  `app` pspeFreq

p01 =
  psnew "speSynth" Nothing AddToTail 1
  [("dur",prepeat 0.0917)
  ,("amp",prepeat 0.1)
  ,("freq", lam (midiCPS (preplicate (pval 3) pz)) `app` pspeFreq)]

p02 =
  lam (psnew "speSynth" Nothing AddToTail 1
        [("dur",prepeat 0.082)
        ,("amp",pforever (prange (pdouble 0.075) (pdouble 0.125)))
        ,("freq",
          midiCPS (pseq (pint 4)
                   [pz-12,pz-5,pz,pz+7,pz+12,pz+7,pz,pz-5,pz-12]))])
 `app` pspeFreq
