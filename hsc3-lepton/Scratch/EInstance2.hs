{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

E Instances of classes in PC01
-}
module Scratch.EInstance2 where

import Data.ByteString.Lazy (ByteString)

import Sound.SC3

import qualified Data.Binary as Bin

import Scratch.PC02
import Scratch.Etree (Etree(..))
import Scratch.Parse5 (E(..), etree, toE)
import Scratch.Type00

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

mkParams :: Bin.Binary a => Int -> [(a, E h e)] -> [Etree]
mkParams h ps = case ps of
  []         -> []
  ((k,v):qs) -> Leaf (Bin.encode k):unE v h:mkParams h qs

instance Pappend E where pappend x y = binaryE "pappend" x y

instance Pconcat E where pconcat = listE "pconcat"

instance Prand E where
  prand i xs = E $ \h -> Node "prand" (unE i h:map (flip unE h) xs)

instance Preplicate E where
  preplicate n x = E $ \h -> Node "preplicate" [unE n h, unE x h]

instance Pseq E where
  pseq n xs = E $ \h -> Node "pseq" (unE n h:map (flip unE h) xs)

instance Pcycle E where pcycle = listE "pcycle"

instance Pforever E where pforever = unaryE "pforever"

instance Ptuple E where
  pzip a b = E $ \h -> Node "pzip" [unE a h,unE b h]
  pfst e = E $ \h -> Node "pfst" [unE e h]
  psnd e = E $ \h -> Node "psnd" [unE e h]

instance Plambda E where
  pz = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  ps v = E $ \h -> unE v (pred h)
  plam t k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = tyTree t
        body = unE k (succ h)
    in  Node "plam" [v,ty,body]
  papp x y = E $ \h -> Node "papp" [unE x h, unE y h]

tyTree :: forall t. Ty t -> Etree
tyTree t = case t of
  TyDouble  -> Leaf "Double"
  TyInt     -> Leaf "Int"
  TyList a  -> Node "List" [tyTree a]
  TyToOSC a -> Node "ToOSC" [tyTree a]
  TyArr a b -> Node "Arr" [tyTree a,tyTree b]
  TyTup a b -> Node "Tup" [tyTree a,tyTree b]
  TyAny     -> Node "Any" []

instance Psnew E where
  psnew def nid aa tid ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "psnew" $
        Leaf (Bin.encode def):Leaf (Bin.encode nid):
        Leaf (Bin.encode aa):Leaf (Bin.encode tid):ps'

instance Pmerge E where
  pmerge = binaryE "pmerge"

instance Ppar E where
  ppar = listE "ppar"

instance Pint E where
  pint = primE "pint"
  (+!) = binaryE "+!"
  (*!) = binaryE "*!"
  (-!) = binaryE "-!"
  pinegate = unaryE "pinegate"
  piabs = unaryE "piabs"
  pisignum = unaryE "pisignum"
  pirange = binaryE "pirange"

instance Pdouble E where
  pdouble = primE "pdouble"
  (+@) = binaryE "+@"
  (*@) = binaryE "*@"
  (-@) = binaryE "-@"
  pdnegate = unaryE "pdnegate"
  pdabs = unaryE "pdabs"
  pdsignum = unaryE "pdsignum"
  pdrange = binaryE "pdrange"
  (/@) = binaryE "/@"
  precip = unaryE "recip"
  ppi = constE "ppi"
  pexp = unaryE "pexp"
  psqrt = unaryE "psqrt"
  plog = unaryE "plog"
  (**@) = binaryE "**@"
  plogBase = binaryE "plogBase"
  psin = unaryE "psin"
  ptan = unaryE "ptan"
  pcos = unaryE "pcos"
  pasin = unaryE "pasin"
  patan = unaryE "patan"
  pacos = unaryE "pacos"
  psinh = unaryE "psinh"
  ptanh = unaryE "ptanh"
  pcosh = unaryE "pcosh"
  pasinh = unaryE "pasinh"
  patanh = unaryE "patanh"
  pacosh = unaryE "pacosh"
  pampDb = unaryE "pampDb"
  pasFloat = unaryE "pasFloat"
  pasInt = unaryE "pasInt"
  pbitNot = unaryE "pbitNot"
  pcpsMIDI = unaryE "pcpsMIDI"
  pcpsOct = unaryE "pcpsOct"
  pcubed = unaryE "pcubed"
  pdbAmp = unaryE "pdbAmp"
  pdistort = unaryE "pdistort"
  pfrac = unaryE "pfrac"
  pisNil = unaryE "pisNil"
  plog10 = unaryE "plog10"
  plog2 = unaryE "plog2"
  pmidiCPS = unaryE "pmidiCPS"
  pmidiRatio = unaryE "pmidiRatio"
  pnotE = unaryE "pnotE"
  pnotNil = unaryE "pnotNil"
  poctCPS = unaryE "poctCPS"
  pramp_ = unaryE "pramp_"
  pratioMIDI = unaryE "pratioMIDI"
  psoftClip = unaryE "psoftClip"
  psquared = unaryE "psquared"
