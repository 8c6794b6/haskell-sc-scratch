{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Simple string representation for pattern.
-}
module Sound.SC3.Lepton.Pattern.Interpreter.S
  ( S(..)
  , toS
  , view
  ) where

import Sound.SC3.Lepton.Pattern.Expression.Class

newtype S h a = S {unS :: Int -> String}

toS :: S h a -> S h a
toS = id

view :: S h a -> String
view e = unS e 0

viewSs :: [S h a] -> Int -> String
viewSs ss n = case ss of
  [] -> "[]"; (t:ts) -> '[': unS t n ++ go ts
  where go us = case us of [] -> "]"; (v:vs) -> ',' : unS v n ++ go vs

instance Show (S h a) where
  show = view

instance Eq (S h a) where
  a == b = unS a 0 == unS b 0

------------------------------------------------------------------------------
-- Helper functions

constS :: Show a => String -> a -> S h b
constS str a = S $ \_ -> str ++ " " ++ show a

litS :: String -> S h a
litS str = S $ \_ -> str

binS :: String -> S h1 a1 -> S h2 a2 -> S h a
binS str a b = S $ \h -> concat ["(",unS a h,") ",str," (",unS b h,")"]

liftS :: String -> S h1 a1 -> S h2 a2
liftS str a = S $ \h -> concat [str," (",unS a h,")"]

liftS2 :: String -> S h1 a1 -> S h2 a2 -> S h3 a3
liftS2 str a b = S $ \h -> concat [str," (",unS a h,") (",unS b h,")"]

liftSs :: String -> [S h1 a1] -> S h2 a2
liftSs str ss = S $ \h -> concat [str ++ " " ++ viewSs ss h]

liftS1s :: String -> S h1 a1 -> [S h2 a2] -> S h3 a3
liftS1s str s ss = S $ \h -> concat [str," (",unS s h,") ",viewSs ss h]

------------------------------------------------------------------------------
-- Instances

instance Pint S where
  pint = constS "pint"
  (+!) = binS "+!"
  (*!) = binS "*!"
  (-!) = binS "-!"
  pinegate = liftS "pinegate"
  piabs = liftS "piabs"
  pisignum = liftS "pisignum"
  pirange = liftS2 "pirange"

instance Pdouble S where
  pdouble = constS "pdouble"
  (+@) = binS "+@"
  (*@) = binS "*@"
  (-@) = binS "-@"
  pdnegate = liftS "pdnegate"
  pdabs = liftS "pdabs"
  pdsignum = liftS "pdsignum"
  pdrange = liftS2 "pdrange"
  (/@) = binS "/@"
  precip = liftS "precip"
  ppi = litS "ppi"
  pexp = liftS "pexp"
  psqrt = liftS "psqrt"
  plog = liftS "plog"
  (**@) = binS "**@"
  plogBase = liftS2 "plogBase"
  psin = liftS "psin"
  ptan = liftS "ptan"
  pcos = liftS "pcos"
  pasin = liftS "pasin"
  patan = liftS "patan"
  pacos = liftS "pacos"
  psinh = liftS "psinh"
  ptanh = liftS "ptanh"
  pcosh = liftS "pcosh"
  pasinh = liftS "pasinh"
  patanh = liftS "patanh"
  pacosh = liftS "pacosh"
  pampDb = liftS "pampDb"
  pasFloat = liftS "pasFloat"
  pasInt = liftS "pasInt"
  pbitNot = liftS "pbitNot"
  pcpsMIDI = liftS "pcpsMIDI"
  pcpsOct = liftS "pcpsOct"
  pcubed = liftS "pcubed"
  pdbAmp = liftS "pdbAmp"
  pdistort = liftS "pdistort"
  pfrac = liftS "pfrac"
  pisNil = liftS "pisNil"
  plog10 = liftS "plog10"
  plog2 = liftS "plog2"
  pmidiCPS = liftS "pmidiCPS"
  pmidiRatio = liftS "pmidiRatio"
  pnotE = liftS "pnotE"
  pnotNil = liftS "pnotNil"
  poctCPS = liftS "poctCPS"
  pramp_ = liftS "pramp_"
  pratioMIDI = liftS "pratioMIDI"
  psoftClip = liftS "psoftClip"
  psquared = liftS "psquared"

instance Pappend S where
  pappend = liftS2 "pappend"

instance Pconcat S where
  pconcat = liftSs "pconcat"

instance Preplicate S where
  preplicate = liftS2 "preplicate"

instance Pseq S where
  pseq = liftS1s "pseq"

instance Pforever S where
  pforever = liftS "pforever"

instance Pcycle S where
  pcycle = liftSs "pcycle"

instance Prand S where
  prand = liftS1s "prand"

instance Pshuffle S where
  pshuffle = liftSs "prand"

instance Ptuple S where
  pzip = liftS2 "pzip"
  pfst = liftS "pfst"
  psnd = liftS "psnd"

instance Pfsm S where
  pfsm is ss = S $ \h ->
    let f xs = case xs of
          []            -> ""
          ((p,js):rest) -> concat ["[(",unS p h,",",show js,")",g rest]
        g xs = case xs of
          []            -> "]"
          ((p,js):rest) -> concat [",(",unS p h,",",show js,")",g rest]
    in  "pfsm " ++ show is ++ " " ++ f ss

instance Plambda S where
  pz = S $ \h -> "x" ++ show (pred h)
  ps v = S $ \h -> unS v (pred h)
  plam t k = S $ \h ->
    let x = "x" ++ show h
    in  "plam (\\" ++ x ++ " :: " ++ show t ++ " -> " ++ unS k (succ h) ++ ")"
  papp = liftS2 "papp"

instance Psnew S where
  psnew def nid aa tid ms = S $ \h ->
    "psnew " ++ unwords [show def,show nid,show aa,show tid] ++ " " ++ unParams ms h

instance Pnset S where
  pnset nid ms = S $ \h ->
    "pnset " ++ show nid ++ " " ++ unParams ms h

instance Ptake S where
  ptakeT n p = S $ \h -> "ptakeT (" ++ unS n h ++ ") (" ++ unS p h ++ ")"

instance Pdrop S where
  pdropT n p = S $ \h -> "pdropT (" ++ unS n h ++ ") (" ++ unS p h ++ ")"

unParams :: Show a => [(a, S h a1)] -> Int -> String
unParams ns i = case ns of
  [] -> "[]"
  ((k,v):ns') ->
    '[' : ("(" ++ show k ++ "," ++ unS v i ++ ")" ++ unSs' ns' i)
  where
    unSs' os j = case os of
      [] -> "]"
      ((k,v):xs) -> ",(" ++ show k ++ "," ++ unS v j ++ ")" ++ unSs' xs j

instance Pmerge S where
  pmerge = liftS2 "pmerge"

instance Ppar S where
  ppar = liftSs "ppar"
