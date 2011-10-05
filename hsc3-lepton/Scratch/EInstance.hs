{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

E Instances of classes in PC01
-}
module Scratch.EInstance where

import Data.ByteString.Lazy (ByteString)

import Sound.SC3

import qualified Data.Binary as Bin

import Scratch.PC01
import Scratch.Etree (Etree(..))
import Scratch.Parse5 (E(..), etree, toE)

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

instance Pforever E where pforever = unaryE "pforever"

instance Prange E where prange = binaryE "prange"

instance Prng E where
  pirange = binaryE "pirange"
  pdrange = binaryE "pdrange"

instance Ptuple E where
  pzip a b = E $ \h -> Node "ptuple" [unE a h,unE b h]
  pfst e = E $ \h -> Node "pfst" [unE e h]
  psnd e = E $ \h -> Node "psnd" [unE e h]

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
