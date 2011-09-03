{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Syntax of patterns for serialization and deserialization.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.Syntax where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import System.Random

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.Interpreter.S
import Sound.SC3.Lepton.Scratch.RespTest (pspe,loop01,loop02,loop03)

{-

XXX: Might rewrite to use ByteString instead of String.

-}
-- | Pattern syntax tree.
data Expr s
  = Leaf s
  | Node String [Expr s]
  | NodeI String (Expr Int) [Expr s]
  | NodeO MsgType [(String,Expr Double)]
  deriving (Eq,Read,Show)

unLeaf :: Expr s -> Either String s
unLeaf n = case n of
  Leaf n -> Right n
  _      -> Left "not a leaf"

toExpr :: Expr s -> Expr s
toExpr = id

eor :: a -> Either x a -> a
l `eor` r = either (const l) id r

{-

When reading from String, define a function for parsing, e.g.:

> safeRead :: (Read a) => String -> Either String a
> safeRead r = case reads r of
>   [(x,"")] -> Right x
>   _        -> Left "Failure in safeRead"

And change pval, plist, and prepeat as below:

>  Node "pval" [Leaf n] ->
>    pval <$> safeRead n
>  Node "plist" ps ->
>    pure $ plist $ rights $ map (\x -> unLeaf x >>= safeRead) ps
>  Node "prepeat" [Leaf n] ->
>    prepeat <$> safeRead n

-}
fromExprE f e = case e of
  Node "pempty" []        -> pure pempty
  Node "pval" [Leaf n]    -> pure $ pval n
  Node "plist" ps         -> pure $ plist $ rights $ map unLeaf ps
  Node "prepeat" [Leaf n] -> pure $ prepeat n
  Node "pcycle" ps        -> pure $ pcycle (rights $ map f ps)
  Node "prange" [p1,p2]   -> prange <$> f p1 <*> f p2
  Node "prandom" []       -> pure prandom
  Node "pforever" [p]     -> pforever <$> (f p)
  Node "pshuffle" ps      -> pure $ pshuffle (rights $ map f ps)
  Node "pmerge" [p1,p2]   -> pmerge <$> f p1 <*> f p2
  Node "ppar" ps          -> pure $ ppar $ rights $ map f ps
  _                       -> fromExprNum f e

fromExprNum f e = case e of
  Node "+" [a,b]    -> pure $ (0 `eor` f a) + (0 `eor` f b)
  Node "*" [a,b]    -> pure $ (1 `eor` f a) * (1 `eor` f b)
  Node "-" [a,b]    -> pure $ (0 `eor` f a) - (0 `eor` f b)
  Node "negate" [a] -> pure $ negate (0 `eor` f a)
  Node "abs" [a]    -> pure $ abs (0 `eor` f a)
  Node "signum" [a] -> pure $ signum (0 `eor` f a)
  _                 -> fromExprFractional f e

fromExprFractional f e = case e of
  Node "/" [a,b]   -> pure $ (0 `eor` f a) / (1 `eor` f b)
  Node "recip" [a] -> pure $ recip (1 `eor` f a)
  _                -> fromExprFloating f e

fromExprFloating f e = case e of
  Node "pi" []         -> pure (pi)
  Node "**" [a,b]      -> pure $ (1 `eor` f a) ** (0 `eor` f b)
  Node "logBase" [a,b] -> pure $ logBase (1 `eor` f a) (1 `eor` f b)
  Node func [a]        -> case lookup func funcs of
    Just func' -> pure $ func' (1 `eor` f a)
    Nothing    -> fromExprUnary f e
  _                    -> fromExprUnary f e
  where
    funcs =
      [("exp",exp),("sqrt",sqrt),("log",log),("sin",sin),("tan",tan)
      ,("cos",cos),("asin",asin),("acos",acos),("sinh",sinh),("tanh",tanh)
      ,("cosh",cosh),("asinh",asinh),("atanh",atanh),("acosh",acosh)]

fromExprUnary self e = case e of
  Node func [a] -> case lookup func funcs of
    Just func' -> pure $ func' (0 `eor` self a)
  _             -> Left ("Unknown expression" :: String)
  where
    funcs =
      [("ampDb",ampDb),("asFloat",asFloat),("asInt",asInt)
      ,("bitNot",bitNot),("cpsMIDI",cpsMIDI),("cpsOct",cpsOct)
      ,("cubed",cubed),("dbAmp",dbAmp),("distort",distort)
      ,("frac",frac),("isNil",isNil),("log10",log10)
      ,("log2",log2),("midiCPS",midiCPS),("midiRatio",midiRatio)
      ,("notE",notE),("notNil",notNil),("octCPS",octCPS)
      ,("ramp_",ramp_),("ratioMIDI",ratioMIDI),("softClip",softClip)
      ,("squared",squared)]

{-

This function takes 2 functions for recursing, one for Int patterns and
the other for arbitrary types.

When reading from string, apply show to Int patterns as shown below.
Note that Functor instance of Node will be required.

> fromExprI self self' e = case e of
>   NodeI "pseq" n ps ->
>     pseq <$> (self' $ fmap show n) <*> pure (rights $ map self ps)
>   ...

-}
fromExprI f f' e = case e of
  NodeI "pseq" n ps        -> pseq <$> (f' n) <*> pure (rights $ map f ps)
  NodeI "prand" n ps       -> prand <$> (f' n) <*> pure (rights $ map f ps)
  NodeI "preplicate" n [p] -> preplicate <$> (f' n) <*> f p
  NodeI "pchoose" n ps     -> prand <$> (f' n) <*> pure (rights $ map f ps)
  _                        -> fromExprE f e

{-

This function also takes 2 functions, one for ToOSC Double in parameter
pairs, and another for passing to unmatched case.

-}
fromExprO f f' e = case e of
  NodeO (Nset n) ps       -> pnset n <$> pure (concatMap g ps)
  NodeO (Snew d n a t) ps -> psnew d n a t <$> pure (concatMap g ps)
  _                       -> fromExprE f e
  where
    g (k,s) = either (const []) (\a -> [(k,a)]) (f' s)

-- | Same as Data.Function.fix.
fix :: (t -> t) -> t
fix f = f (fix f)

-- | Fix combinator with two arguments.
fix2 :: (a -> b -> a) -> b -> a
fix2 f g = f (fix2 f g) g

-- Using fix2 here, to isolate the recursion of Int from the other.
fromExpr' = fix2 fromExprI (fix $ fix2 fromExprI)

-- Using fix2, to isolate recursion of ('ToOSC' 'Double').
fromExpr = fix2 fromExprO fromExpr'

-- Reads expression from file.
fromFile path = fromExpr . read <$> readFile path

{-

let p = ppar [loop01,loop02,loop03]
in  writeFile "Sound/SC3/Lepton/Scratch/sw.txt" (show $ toExpr p)

do { Right r <- fromFile "Sound/SC3/Lepton/Scratch/sw.txt"
   ; audition (r :: R (ToOSC Double)) }

-}

-- ---------------------------------------------------------------------------
-- Sample patterns

pspe' =
  psnew "speSynth" Nothing AddToTail 1
    [("dur",prepeat 0.13)
    ,("freq", midiCPS pspe)]

p1 =
  pcycle
  [plist [1,2,3]
  ,pval 4
  ,prange (pval 5) (pval 10)]

p2 =
  psnew "foo" Nothing AddToTail 1
    [("dur", prepeat 1)
    ,("amp", pforever (prepeat 0.1 * plist [1,2,3]))
    ,("freq",
      pcycle
      [pseq (prange 1 8)
       [plist [1,2,3]
       ,prand 3 [4,5,6]]
      ,prand (prange 1 8)
       [prange 1 10, prange 101 110, prange 1001 1010]])]

p3 =
  pnset 1000
    [("dur", pforever 1)
    ,("amp", plist [1,2,3])]

p4 = ppar [p2,p3,pspe']

p5 =
  psnew "bar" Nothing AddToTail 1
    [("dur", prepeat 1 + pcycle [1.0,2.0,3.5,2.0])
    ,("freq", midiCPS $ prepeat 60 + pseq (prange 1 8) [0,7,14])]

-- ---------------------------------------------------------------------------
-- XXX: Dummy instances.
--
-- Functions are unused but type signatures are used.
-- When recursion for value patterns, int patterns, and ToOSC patterns could be
-- isolated, these dummy instance definitions could be removed.
--

instance Random String where
  random  = undefined
  randomR = undefined

instance Random a => Random (ToOSC a) where
  random = undefined
  randomR = undefined

instance Num a => Num (ToOSC a) where
  a + b = undefined
  a * b = undefined
  a - b = undefined
  negate a = undefined
  abs a = undefined
  signum a = undefined
  fromInteger a = undefined

instance Fractional a => Fractional (ToOSC a) where
  a / b = undefined
  recip = undefined
  fromRational = undefined

instance Ord a => Ord (ToOSC a) where
  compare = undefined

instance Floating a => Floating (ToOSC a) where
  pi = undefined
  exp a = undefined
  sqrt a = undefined
  log a = undefined
  a ** b = undefined
  logBase a b = undefined
  sin a = undefined
  tan a = undefined
  cos a = undefined
  asin a = undefined
  atan a = undefined
  acos a = undefined
  sinh a = undefined
  tanh a = undefined
  cosh a = undefined
  asinh a = undefined
  atanh a = undefined
  acosh a = undefined

instance UnaryOp a => UnaryOp (ToOSC a) where
  ampDb a = undefined
  asFloat a = undefined
  asInt a = undefined
  bitNot a = undefined
  cpsMIDI a = undefined
  cpsOct a = undefined
  cubed a = undefined
  dbAmp a = undefined
  distort a = undefined
  frac a = undefined
  isNil a = undefined
  log10 a = undefined
  log2 a = undefined
  midiCPS a = undefined
  midiRatio a = undefined
  notE a = undefined
  notNil a = undefined
  octCPS a = undefined
  ramp_ a = undefined
  ratioMIDI a = undefined
  softClip a = undefined
  squared a = undefined

instance Fractional Int where
  a / b = undefined
  recip = undefined
  fromRational = undefined

instance Floating Int where
  pi = undefined
  exp a = undefined
  sqrt a = undefined
  log a = undefined
  a ** b = undefined
  logBase a b = undefined
  sin a = undefined
  tan a = undefined
  cos a = undefined
  asin a = undefined
  atan a = undefined
  acos a = undefined
  sinh a = undefined
  tanh a = undefined
  cosh a = undefined
  asinh a = undefined
  atanh a = undefined
  acosh a = undefined

instance UnaryOp Int where
  ampDb a = undefined
  asFloat a = undefined
  asInt a = undefined
  bitNot a = undefined
  cpsMIDI a = undefined
  cpsOct a = undefined
  cubed a = undefined
  dbAmp a = undefined
  distort a = undefined
  frac a = undefined
  isNil a = undefined
  log10 a = undefined
  log2 a = undefined
  midiCPS a = undefined
  midiRatio a = undefined
  notE a = undefined
  notNil a = undefined
  octCPS a = undefined
  ramp_ a = undefined
  ratioMIDI a = undefined
  softClip a = undefined
  squared a = undefined

-- ---------------------------------------------------------------------------
-- Numeric classes

instance Num a => Num (Expr a) where
  a + b = Node "+" [a,b]
  a * b = Node "*" [a,b]
  a - b = Node "-" [a,b]
  negate a = Node "negate" [a]
  abs a = Node "abs" [a]
  signum a = Node "signum" [a]
  fromInteger a = pval (fromInteger a)

instance Fractional a => Fractional (Expr a) where
  a / b = Node "/" [a,b]
  recip a = Node "recip" [a]
  fromRational a = pval (fromRational a)

instance Floating a => Floating (Expr a) where
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

instance Ord a => Ord (Expr a) where
  compare a b = EQ

instance UnaryOp a => UnaryOp (Expr a) where
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

-- ---------------------------------------------------------------------------
-- Pattern classes

instance Mergable (Expr a) where
  merge a b = Node "merge" [a,b]

instance Pempty Expr where
  pempty = Node "pempty" []

instance Pval Expr where
  pval a = Node "pval" [Leaf a]

instance Prepeat Expr where
  prepeat a = Node "prepeat" [Leaf a]

instance Plist Expr where
  plist as = Node "plist" (map Leaf as)

instance Pconcat Expr where
  pconcat ps = Node "pconcat" ps

instance Pappend Expr where
  pappend p1 p2 = Node "pappend" [p1,p2]

instance Pseq Expr where
  pseq p1 p2 = NodeI "pseq" p1 p2

instance Preplicate Expr where
  preplicate p1 p2 = NodeI "preplicate" p1 [p2]

instance Pcycle Expr where
  pcycle ps = Node "pcycle" ps

instance Pforever Expr where
  pforever p = Node "pforever" [p]

instance Prandom Expr where
  prandom = Node "prandom" []

instance Prange Expr where
  prange p1 p2 = Node "prange" [p1,p2]

instance Pchoose Expr where
  pchoose n ps = NodeI "pchoose" n ps

instance Prand Expr where
  prand pn ps = NodeI "prand" pn ps

instance Pshuffle Expr where
  pshuffle ps = Node "pshuffle" ps

instance Pmerge Expr where
  pmerge p1 p2 = Node "pmerge" [p1,p2]

instance Ppar Expr where
  ppar ps = Node "ppar" ps

instance Psnew Expr where
  psnew def nid aa tid ms = NodeO (Snew def nid aa tid) ms

instance Pnset Expr where
  pnset tid ms = NodeO (Nset tid) ms