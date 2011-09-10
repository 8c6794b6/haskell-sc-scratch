{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Syntax of patterns for serialization and deserialization, take 2.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.E where

import Control.Applicative
import Data.Data
import System.Random

import Sound.SC3
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.ParseP

import qualified Data.Binary as Bin

data E a
  = Leaf String
  | Node String [E a]
  | NodeI String (E Int) [E a]
  | NodeO MsgType [(String,E Double)]
  deriving (Eq, Show, Data, Typeable)

instance Bin.Binary (E a) where
  put e = case e of
    Leaf x       -> Bin.putWord8 0 >> Bin.put x
    Node n es    -> Bin.putWord8 1 >> Bin.put n >> Bin.put es
    NodeI n e es -> Bin.putWord8 2 >> Bin.put n >> Bin.put e >> Bin.put es
    NodeO m ps   -> Bin.putWord8 3 >> Bin.put m >> Bin.put ps
  get = do
    idx <- Bin.getWord8
    case idx of
      0 -> Leaf <$> Bin.get
      1 -> Node <$> Bin.get <*> Bin.get
      2 -> NodeI <$> Bin.get <*> Bin.get <*> Bin.get
      3 -> NodeO <$> Bin.get <*> Bin.get
      _ -> error $ "Unknown index in E get: " ++ show idx

toE :: E a -> E a
toE = id

unLeaf (Leaf s) = s
unLeaf _        = error "Not a leaf"

fix f = f (fix f)
fix2 f g = f (fix2 f g) g
fix3 f g h = f (fix3 f g h) g h
fix3' f g h = f (fix3' f g h) (g h) h

-- f' = fix3 fromE' (fix (fix2 (fix3 fromE'))) id
-- f = fix2 fromNodeO (fix2 fromNodeI (fix (fix2 fromNodeI)))
-- fromEa = fix (fix2 (fix3 fromE'))
-- fromEb = fix (fix3' fromE' (fix (fix3' fromE')))

fI = fix2 fromNodeI (fix (fix2 fromNodeI))
fII = fix2 fromNodeI (fix fromNodePattern)

fromE = fix3 fromNodeO fI fI 

{-
XXX: Wrap with Either, use safeRead.
-}

fromNodeO f1 f2 f3 e = case e of
  NodeO (Snew def nid aa tid) ps -> psnew def nid aa tid (f2' ps)
  NodeO (Nset tid) ps            -> pnset tid (f2' ps)
  Node "pmerge" [p1,p2]          -> pmerge (f1 p1) (f1 p2)
  Node "ppar" ps                 -> ppar (map f1 ps)
  _ -> fromNodeI f1 f3 e
  where
    f2' = map (\(k,p) -> (k, f2 p))

fromNodeI fg fi e = case e of
  NodeI "preplicate" n [p] -> preplicate (fi n) (fg p)
  NodeI "pseq" n ps        -> pseq (fi n) (map fg ps)
  NodeI "prand" n ps       -> prand (fi n) (map fg ps)
  NodeI "pchoose" n ps     -> pchoose (fi n) (map fg ps)
  _ -> fromNodePattern fg e
  -- _ -> fg e

fromNodePattern f e = case e of
  Node "pval" [Leaf x]     -> pval (read x)
  Node "plist" ps          -> plist (map (read . unLeaf) ps)
  Node "prepeat" [Leaf x]  -> prepeat (read x)
  Node "pempty" []         -> pempty
  Node "pappend" [p1,p2]   -> pappend (f p1) (f p2)
  Node "pconcat" ps        -> pconcat (map f ps)
  Node "pcycle" ps         -> pcycle (map f ps)
  Node "pforever" [p]      -> pforever (f p)
  Node "prange" [p1,p2]    -> prange (f p1) (f p2)
  Node "prandom" []        -> prandom
  Node "pshuffle" ps       -> pshuffle (map f ps)
  _                        -> fromNum f e

fromNum f e = case e of
  Node "+" [a,b]    -> f a + f b
  Node "*" [a,b]    -> f a * f b
  Node "-" [a,b]    -> f a - f b
  Node "negate" [a] -> negate (f a)
  Node "abs" [a]    -> abs (f a)
  Node "signum" [a] -> signum (f a)
  _                 -> fromFractional f e

fromFractional f e = case e of
  Node "/" [a,b]   -> f a + f b
  Node "recip" [a] -> recip (f a)
  _                -> fromFloating f e

fromFloating f e = case e of
  Node "pi" []         -> pi
  Node "exp" [a]       -> exp (f a)
  Node "sqrt" [a]      -> sqrt (f a)
  Node "log" [a]       -> log (f a)
  Node "**" [a,b]      -> f a ** f b
  Node "logBase" [a,b] -> logBase (f a) (f b)
  Node "sin" [a]       -> sin (f a)
  Node "tan" [a]       -> tan (f a)
  Node "cos" [a]       -> cos (f a)
  Node "asin" [a]      -> asin (f a)
  Node "atan" [a]      -> atan (f a)
  Node "acos" [a]      -> acos (f a)
  Node "sinh" [a]      -> sinh (f a)
  Node "tanh" [a]      -> tanh (f a)
  Node "cosh" [a]      -> cosh (f a)
  Node "asinh" [a]     -> asinh (f a)
  Node "atanh" [a]     -> atanh (f a)
  Node "acosh" [a]     -> acosh (f a)
  _                    -> fromUnary f e

fromUnary f e = case e of
  Node "ampDb" [a]     -> ampDb (f a)
  Node "asFloat" [a]   -> asFloat (f a)
  Node "asInt" [a]     -> asInt (f a)
  Node "bitNot" [a]    -> bitNot (f a)
  Node "cpsMIDI" [a]   -> cpsMIDI (f a)
  Node "cpsOct" [a]    -> cpsOct (f a)
  Node "cubed" [a]     -> cubed (f a)
  Node "dbAmp" [a]     -> dbAmp (f a)
  Node "distort" [a]   -> distort (f a)
  Node "frac" [a]      -> frac (f a)
  Node "isNil" [a]     -> isNil (f a)
  Node "log10" [a]     -> log10 (f a)
  Node "log2" [a]      -> log2 (f a)
  Node "midiCPS" [a]   -> midiCPS (f a)
  Node "midiRatio" [a] -> midiRatio (f a)
  Node "notE" [a]      -> notE (f a)
  Node "notNil" [a]    -> notNil (f a)
  Node "octCPS" [a]    -> octCPS (f a)
  Node "ramp_" [a]     -> ramp_ (f a)
  Node "ratioMIDI" [a] -> ratioMIDI (f a)
  Node "softClip" [a]  -> softClip (f a)
  Node "squared" [a]   -> squared (f a)
  _                    -> error $ "Unknown: " ++ show e

instance Pval E where pval a = Node "pval" [Leaf $ show a]
instance Plist E where plist as = Node "plist" (map (Leaf . show) as)
instance Prepeat E where prepeat a = Node "prepeat" [Leaf $ show a]

instance Pempty E where pempty = Node "pempty" []
instance Pappend E where pappend p1 p2 = Node "pappend" [p1,p2]
instance Pconcat E where pconcat ps = Node "pconcat" ps
instance Pcycle E where pcycle ps = Node "pcycle" ps
instance Prange E where prange p1 p2 = Node "prange" [p1,p2]
instance Pforever E where pforever p = Node "pforever" [p]
instance Prandom E where prandom = Node "prandom" []
instance Pshuffle E where pshuffle ps = Node "pshuffle" ps

instance Mergable (E a) where merge a b = Node "merge" [a,b]
instance Pmerge E where pmerge a b = Node "pmerge" [a,b]
instance Ppar E where ppar ps = Node "ppar" ps

instance Pseq E where pseq pn ps = NodeI "pseq" pn ps
instance Prand E where prand pn ps = NodeI "prand" pn ps
instance Pchoose E where pchoose pn ps = NodeI "pchoose" pn ps
instance Preplicate E where preplicate pn p = NodeI "preplicate" pn [p]

instance Psnew E where psnew def nid aa tid ps = NodeO (Snew def nid aa tid) ps
instance Pnset E where pnset tid ps = NodeO (Nset tid) ps

instance Functor E where
  fmap f (Leaf x) = Leaf x
  fmap f (Node n es) = Node n (map (fmap f) es)
  fmap f (NodeI n e es) = NodeI n e (map (fmap f) es)
  fmap f (NodeO m es) = NodeO m es

instance Num (E a) where
  a + b = Node "+" [a,b]
  a * b = Node "*" [a,b]
  a - b = Node "-" [a,b]
  negate a = Node "negate" [a]
  abs a = Node "abs" [a]
  signum a = Node "signum" [a]
  fromInteger a = Node "pval" [Leaf $ show (fromInteger a)]

instance Fractional (E a) where
  a / b = Node "/" [a,b]
  recip a = Node "recip" [a]
  fromRational a = Node "pval" [Leaf $ show (fromRational a)]

instance Floating (E a) where
  pi = Node "pi" []
  exp a = Node "exp" [a]
  sqrt a = Node "sqrt" [a]
  log a = Node "log" [a]
  a ** b = Node "**" [a,b]
  logBase a b = Node "logBase" [a,b]
  sin a = Node "sin" [a]
  tan a = Node "tan" [a]
  cos a = Node "cos" [a]
  asin a = Node "asin" [a]
  atan a = Node "atan" [a]
  acos a = Node "acos" [a]
  sinh a = Node "sinh" [a]
  tanh a = Node "tanh" [a]
  cosh a = Node "cosh" [a]
  asinh a = Node "asinh" [a]
  atanh a = Node "atanh" [a]
  acosh a = Node "acosh" [a]

instance Ord (E a) where
  compare _ _ = EQ

instance UnaryOp (E a) where
  ampDb a = Node "ampDb" [a]
  asFloat a = Node "asFloat" [a]
  asInt a = Node "asInt" [a]
  bitNot a = Node "bitNot" [a]
  cpsMIDI a = Node "cpsMIDI" [a]
  cpsOct a = Node "cpsOct" [a]
  cubed a = Node "cubed" [a]
  dbAmp a = Node "dbAmp" [a]
  distort a = Node "distort" [a]
  frac a = Node "frac" [a]
  isNil a = Node "isNil" [a]
  log10 a = Node "log10" [a]
  log2 a = Node "log2" [a]
  midiCPS a = Node "midiCPS" [a]
  midiRatio a = Node "midiRatio" [a]
  notE a = Node "notE" [a]
  notNil a = Node "notNil" [a]
  octCPS a = Node "octCPS" [a]
  ramp_ a = Node "ramp_" [a]
  ratioMIDI a = Node "ratioMIDI" [a]
  softClip a = Node "softClip" [a]
  squared a = Node "squared" [a]

------------------------------------------------------------------------------
-- dummy

instance Random a => Random (ToOSC a) where
  random  = error "random not defined for (ToOSC a)"
  randomR = error "randomR not defined for (ToOSC a)"
