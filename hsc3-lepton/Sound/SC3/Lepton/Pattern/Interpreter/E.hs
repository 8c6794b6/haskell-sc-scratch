{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

E, for serializing/deserializing expression tree.
-}
module Sound.SC3.Lepton.Pattern.Interpreter.E where

import Data.ByteString.Lazy (ByteString)
import Text.PrettyPrint (Doc)

import Sound.SC3.Lepton.Pattern.Expression.Class
import Sound.SC3.Lepton.Pattern.Expression.Etree
import Sound.SC3.Lepton.Pattern.Expression.Type

import qualified Data.Binary as Bin

-- | Newtype wrapper for converting to expression tree.
newtype E h a = E {unE :: Int -> Etree}

toE :: E h a -> E h a
toE = id

etree :: E h a -> Etree
etree e = unE e 0

prettyE :: E h a -> Doc
prettyE = ppTree . etree

------------------------------------------------------------------------------
-- Helper functions

constE :: ByteString -> E h a
constE str = E $ \_ -> Node str []

primE :: Bin.Binary a => ByteString -> a -> E h b
primE str x = E $ \_ -> Node str [Leaf $ Bin.encode x]

unaryE :: ByteString -> E h1 a1 -> E h2 a2
unaryE str e = E $ \h -> Node str [unE e h]

binaryE :: ByteString -> E h1 a1 -> E h2 a2 -> E h3 a3
binaryE str e1 e2 = E $ \h -> Node str [unE e1 h, unE e2 h]

listE :: ByteString -> [E h a] -> E h a
listE str es = E $ \h -> Node str (map (flip unE h) es)

mkParams :: Bin.Binary a => Int -> [(a, E h e)] -> [Etree]
mkParams h es = case es of
  []         -> []
  ((k,v):qs) -> Leaf (Bin.encode k):unE v h:mkParams h qs

tyTree :: Ty t -> Etree
tyTree t = case t of
  TyDouble  -> Leaf "Double"
  TyInt     -> Leaf "Int"
  TyBool    -> Leaf "Bool"
  TyList a  -> Node "List" [tyTree a]
  TyToOSC a -> Node "ToOSC" [tyTree a]
  TyArr a b -> Node "Arr" [tyTree a,tyTree b]
  TyTup a b -> Node "Tup" [tyTree a,tyTree b]
  TyAny     -> Node "Any" []

------------------------------------------------------------------------------
-- Base classes

instance Show (E h a) where
  show e = show $ etree e

instance Eq (E h a) where
  e1 == e2 = show e1 == show e2

instance Ord (E h a) where
  compare _ _ = EQ

instance Num a => Num (E h a) where
  (+) = error "Umbiguous use of '+' in E"
  (*) = error "Umbiguous use of '*' in E"
  (-) = error "Umbiguous use of '-' in E"
  abs = error "Umbiguous use of 'abs' in E"
  negate = error "Umbiguous use of 'negate' in E"
  signum = error "Umbiguous use of 'signum' in E"
  fromInteger x = E $ \_ ->
    Node "pint" [Leaf (Bin.encode (fromInteger x :: Int))]

instance Fractional a => Fractional (E h a) where
  (/) = error "Umbiguous use of '/' in E"
  recip = error "Umbiguous use of 'recip' in E"
  fromRational x = E $ \_ ->
    Node "pdouble" [Leaf (Bin.encode (fromRational x :: Double))]

------------------------------------------------------------------------------
-- Pattern classes

instance Pappend E where
  pappend x y = binaryE "pappend" x y

instance Pconcat E where
  pconcat = listE "pconcat"

instance Prand E where
  prand i xs = E $ \h -> Node "prand" (unE i h:map (flip unE h) xs)

instance Preplicate E where
  preplicate n x = E $ \h -> Node "preplicate" [unE n h, unE x h]

instance Pseq E where
  pseq n xs = E $ \h -> Node "pseq" (unE n h:map (flip unE h) xs)

instance Pcycle E where
  pcycle = listE "pcycle"

instance Pforever E where
  pforever = unaryE "pforever"

instance Ptuple E where
  pzip = binaryE "pzip"
  pfst = unaryE "pfst"
  psnd = unaryE "psnd"

instance Pfsm E where
  pfsm is es = E $ \h ->
    let is' = Bin.encode is
        ps' = mkChoices es
        mkChoices xs = case xs of
          [] -> []
          ((y,js):ys) -> unE y h:Leaf (Bin.encode js):mkChoices ys
    in  Node "pfsm" (Leaf is':ps')

instance Plambda E where
  pz = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  ps v = E $ \h -> unE v (pred h)
  plam t k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = tyTree t
        body = unE k (succ h)
    in  Node "plam" [v,ty,body]
  papp x y = E $ \h -> Node "papp" [unE x h, unE y h]

instance Psnew E where
  psnew def nid aa tid es = E $ \h ->
    let ps' = mkParams h es
    in  Node "psnew" $
        Leaf (Bin.encode def):Leaf (Bin.encode nid):
        Leaf (Bin.encode aa):Leaf (Bin.encode tid):ps'

instance Pnset E where
  pnset nid es = E $ \h ->
    let ps' = mkParams h es
    in  Node "pnset" $ Leaf (Bin.encode nid):ps'

instance Pmerge E where
  pmerge = binaryE "pmerge"

instance Ppar E where
  ppar = listE "ppar"

instance Ptake E where
  ptakeT = binaryE "ptakeT"

instance Pdrop E where
  pdropT = binaryE "pdropT"

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
