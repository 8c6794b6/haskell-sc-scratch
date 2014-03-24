{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

S Instances of classes in PC01
-}
module Scratch.SInstance1 where

import Scratch.PC01
import Scratch.S

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

instance Prng S where
  pirange a b = S $ \h -> concat ["pirange (",unS a h,") (",unS b h,")"]
  pdrange a b = S $ \h -> concat ["pdrange (",unS a h,") (",unS b h,")"]

instance Ptuple S where
  pzip a b = S $ \h -> concat ["ptuple (",unS a h,") (",unS b h,")"]
  pfst a = S $ \h -> concat ["pfst (",unS a h,")"]
  psnd a = S $ \h -> concat ["psnd (",unS a h,")"]

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
