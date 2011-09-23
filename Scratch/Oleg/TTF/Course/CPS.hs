{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/course/CPS.hs>

-}
module CPS where

import qualified TTF as Sym

type family Arr (repr :: * -> *) (a :: *) (b :: *) :: *

class ESymantics repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  lam :: (repr a -> repr b) -> repr (Arr repr a b)
  app :: repr (Arr repr a b) -> repr a -> repr b
  
type family GenArr (repr :: * -> *) a :: *
type instance GenArr repr Int = Int
type instance GenArr repr (a->b) = Arr repr (GenArr repr a) (GenArr repr b)

newtype ExtSym repr a = ExtSym {unExtSym :: repr (GenArr repr a)}

instance ESymantics repr => Sym.Symantics (ExtSym repr) where
  int = ExtSym . int
  add e1 e2 = ExtSym $ add (unExtSym e1) (unExtSym e2)
  lam e = ExtSym $ lam (unExtSym . e . ExtSym)
  app e1 e2 = ExtSym $ app (unExtSym e1) (unExtSym e2)
  
te1 = unExtSym Sym.th1  
te2 = unExtSym Sym.th2
te3 = unExtSym Sym.th3

te4 = let c3 = lam (\f -> lam (\x -> f `app` (f `app` (f `app` x))))
      in  (c3 `app` (lam (\x -> x `add` int 14))) `app` (int 0)
          
newtype Lg repr a = Lg {unLg :: repr a}          

type instance Arr (Lg repr) a b = a -> b

instance Sym.Symantics repr => ESymantics (Lg repr) where
  int = Lg . Sym.int
  add e1 e2 = Lg $ Sym.add (unLg e1) (unLg e2)
  lam e = Lg $ Sym.lam (unLg . e . Lg)
  app e1 e2 = Lg $ Sym.app (unLg e1) (unLg e2)
  
legacy :: Sym.Symantics repr => (repr a -> b) -> Lg repr a -> b  
legacy int e = int (unLg e)

te3_eval = legacy Sym.eval te3
te3_eval' = te3_eval id
te3_view = legacy Sym.view te3

te4_eval = legacy Sym.eval te4
te4_view = legacy Sym.view te4

newtype CPS repr w a = CPS {cpsr :: repr (Arr repr (Arr repr a w) w)}

type instance Arr (CPS repr w) a b = Arr repr a (Arr repr (Arr repr b w) w)

cpsk :: ESymantics repr => (repr (Arr repr a w) -> repr w) -> CPS repr w a
cpsk = CPS . lam

appk :: ESymantics repr => CPS repr w a -> (repr a -> repr w) -> repr w
appk (CPS e) f = app e $ lam f

cpsv :: ESymantics repr => repr a -> CPS repr w a
cpsv v = cpsk $ \k -> app k v

instance ESymantics repr => ESymantics (CPS repr w) where
  int x = cpsv $ int x
  add e1 e2 = cpsk $ \k -> 
    appk e1 $ \v1 ->
    appk e2 $ \v2 ->
    app k (add v1 v2)
  lam e = cpsv $ lam (\x -> cpsr $ e (cpsv x))
  app ef ea = cpsk $ \k -> 
    appk ef $ \vf ->
    appk ea $ \va ->
    app (app vf va) k
    
tec1 = cpsr te1
tec1_eval = legacy Sym.eval tec1 id
tec1_view = legacy Sym.view tec1

tec2 = cpsr te2
tec2_eval = legacy Sym.eval tec2
tec2_eval' = \a -> tec2_eval (\k -> k a id)
tec2_eval'' = tec2_eval' 21 -- 42
tec2_view = legacy Sym.view tec2

tec4 = cpsr te4 
tec4_eval = legacy Sym.eval tec4 id
tec4_view = legacy Sym.view tec4 -- wall paper pattern.

tecc1 = cpsr tec1
tecc1_eval = legacy Sym.eval tecc1
tecc1_eval' = tecc1_eval (\k -> k (flip ($)) id)
tecc1_view = legacy Sym.view tecc1

------------------------------------------------------------------------------
-- One-pass CPS transform

newtype CPS1 repr w a = CPS1 {cps1r :: (repr a -> repr w) -> repr w}

reflect :: ESymantics repr 
           => ((repr a -> repr w) -> repr w) -> repr (Arr repr (Arr repr a w) w)
reflect e = lam (\k -> e (\v -> app k v))

type instance Arr (CPS1 repr w) a b = Arr repr a (Arr repr (Arr repr b w) w)

cps1v :: ESymantics repr => repr a -> CPS1 repr w a
cps1v v = CPS1 $ \k -> k v

instance ESymantics repr => ESymantics (CPS1 repr w) where
  int x = cps1v $ int x
  add e1 e2 = CPS1 $ \k ->
    cps1r e1 $ \v1 ->
    cps1r e2 $ \v2 ->
    k (add v1 v2)
  lam e = cps1v $ lam $ reflect . cps1r . e . cps1v
  app ef ea = CPS1 $ \k ->
    cps1r ef $ \vf ->
    cps1r ea $ \va ->
    app (app vf va) (lam k)
    
cps1 = reflect . cps1r    

tek1 = cps1 te1
tek1_eval = legacy Sym.eval tek1 id
tek1_view = legacy Sym.view tek1

tek2 = cps1 te2
tek2_eval = legacy Sym.eval tek2
tek2_eval'' = tek2_eval (\k -> k 21 id)
tek2_view = legacy Sym.view tek2

tek4 = cps1 te4
tek4_eval = legacy Sym.eval tek4 id
tek4_view = legacy Sym.view tek4

------------------------------------------------------------------------------
-- Doing CPS twice

tekk1 = cps1 tek1
tekk1_eval = legacy Sym.eval tekk1
tekk1_eval' = tekk1_eval (\k -> k (flip ($)) id)
tekk1_view = legacy Sym.view tekk1