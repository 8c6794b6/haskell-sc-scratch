{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Data
import Data.Either
import Data.Word
import System.Random
import Text.PrettyPrint

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Data.ByteString as B
import qualified Data.Binary as Bin
import qualified Data.Serialize as Srl

{-

XXX: Write tests.

-}
-- | Pattern syntax tree.
data Expr s
  = Leaf s
  | Node String [Expr s]
  | NodeI String (Expr Int) [Expr s]
  | NodeO MsgType [(String,Expr Double)]
  deriving (Eq,Read,Show,Data,Typeable)

instance Functor Expr where
  fmap f (Leaf s) = Leaf (f s)
  fmap f (Node x ns) = Node x (map (fmap f) ns)
  fmap f (NodeI x n ns) = NodeI x n (map (fmap f) ns)
  fmap f (NodeO _ _) = error "NodeO does not support Functor"

instance Srl.Serialize s => Srl.Serialize (Expr s) where
  {-# INLINE put #-}
  put e = case e of
    Leaf n       -> Srl.put (0::Word8) *> Srl.put n
    Node s es    -> Srl.put (1::Word8) *> Srl.put s *> Srl.put es
    NodeI s i es -> Srl.put (2::Word8) *> Srl.put s *> Srl.put i *> Srl.put es
    NodeO s ps   -> Srl.put (3::Word8) *> Srl.put s *> Srl.put ps
  {-# INLINE get #-}
  get = Srl.getWord8 >>= \i -> case i of
    0 -> Leaf <$> Srl.get
    1 -> Node <$> Srl.get <*> Srl.get
    2 -> NodeI <$> Srl.get <*> Srl.get <*> Srl.get
    3 -> NodeO <$> Srl.get <*> Srl.get
    n -> error $ "Unexpected index in get: " ++ show n

instance Bin.Binary s => Bin.Binary (Expr s) where
  {-# INLINE put #-}
  put e = case e of
    Leaf n       -> Bin.put (0::Word8) *> Bin.put n
    Node s es    -> Bin.put (1::Word8) *> Bin.put s *> Bin.put es
    NodeI s i es -> Bin.put (2::Word8) *> Bin.put s *> Bin.put i *> Bin.put es
    NodeO s ps   -> Bin.put (3::Word8) *> Bin.put s *> Bin.put ps
  {-# INLINE get #-}
  get = Bin.getWord8 >>= \i -> case i of
    0 -> Leaf <$> Bin.get
    1 -> Node <$> Bin.get <*> Bin.get
    2 -> NodeI <$> Bin.get <*> Bin.get <*> Bin.get
    3 -> NodeO <$> Bin.get <*> Bin.get
    n -> error $ "Unexpected index in get: " ++ show n

unLeaf :: Expr s -> Either String s
unLeaf n = case n of
  Leaf x -> Right x
  _      -> Left "not a leaf"
{-# INLINE unLeaf #-}

toExpr :: Expr s -> Expr s
toExpr = id
{-# INLINE toExpr #-}

-- | Pretty printer for expression.
prettyP :: Show s => Expr s -> Doc
prettyP e = case e of
  Leaf x          -> text $ show x
  Node "pval" [l] -> text "pval" <+> prettyP l
  Node "pempty" _ -> text "pempty"
  Node x es ->
    text x $$ nest depth
    (parens $ sep (punctuate comma (map prettyP es)))
  NodeI x ei es ->
    text x $$ nest depth
    (parens (prettyP ei)) $$
    (parens $ sep (punctuate comma (map prettyP es)))
  NodeO m ps ->
    (text $ show m) $$ nest depth
    (parens $ sep $ punctuate comma $ map f ps)
  where
    f (k,p) = parens (doubleQuotes (text k) <> comma $$ prettyP p)
    depth = 2
{-# INLINE prettyP #-}

eor :: a -> Either x a -> a
l `eor` r = either (const l) id r
{-# INLINE eor #-}

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
  Node "prepeat" [Leaf n] -> pure $ prepeat n
  Node "plist" ps         -> pure $ plist $ rights $ map unLeaf ps
  Node "pconcat" ps       -> pure $ pconcat (rights $ map f ps)
  Node "pappend" [p1,p2]  -> pappend <$> f p1 <*> f p2
  Node "pcycle" ps        -> pure $ pcycle (rights $ map f ps)
  Node "pforever" [p]     -> pforever <$> (f p)
  Node "prange" [p1,p2]   -> prange <$> f p1 <*> f p2
  Node "prandom" []       -> pure prandom
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

-- Reads expression from file containing bytestring encoded data of Expr.
fromFile path = (\x -> Srl.decode x >>= fromExpr) <$> B.readFile path

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
  (+) = undefined
  (*) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Fractional a => Fractional (ToOSC a) where
  (/) = undefined
  recip = undefined
  fromRational = undefined

instance Ord a => Ord (ToOSC a) where
  compare = undefined

instance Floating a => Floating (ToOSC a) where
  pi = undefined
  exp = undefined
  sqrt = undefined
  log = undefined
  (**)= undefined
  logBase = undefined
  sin = undefined
  tan = undefined
  cos = undefined
  asin = undefined
  atan = undefined
  acos = undefined
  sinh = undefined
  tanh = undefined
  cosh = undefined
  asinh = undefined
  atanh = undefined
  acosh = undefined

instance UnaryOp a => UnaryOp (ToOSC a) where
  ampDb = undefined
  asFloat = undefined
  asInt = undefined
  bitNot = undefined
  cpsMIDI = undefined
  cpsOct = undefined
  cubed = undefined
  dbAmp = undefined
  distort = undefined
  frac = undefined
  isNil = undefined
  log10 = undefined
  log2 = undefined
  midiCPS = undefined
  midiRatio = undefined
  notE = undefined
  notNil = undefined
  octCPS = undefined
  ramp_ = undefined
  ratioMIDI = undefined
  softClip = undefined
  squared = undefined

instance Fractional Int where
  (/) = undefined
  recip = undefined
  fromRational = undefined

instance Floating Int where
  pi = undefined
  exp = undefined
  sqrt = undefined
  log = undefined
  (**) = undefined
  logBase = undefined
  sin = undefined
  tan = undefined
  cos = undefined
  asin = undefined
  atan = undefined
  acos = undefined
  sinh = undefined
  tanh = undefined
  cosh = undefined
  asinh = undefined
  atanh = undefined
  acosh = undefined

instance UnaryOp Int where
  ampDb = undefined
  asFloat = undefined
  asInt = undefined
  bitNot = undefined
  cpsMIDI = undefined
  cpsOct = undefined
  cubed = undefined
  dbAmp = undefined
  distort = undefined
  frac = undefined
  isNil = undefined
  log10 = undefined
  log2 = undefined
  midiCPS = undefined
  midiRatio = undefined
  notE = undefined
  notNil = undefined
  octCPS = undefined
  ramp_ = undefined
  ratioMIDI = undefined
  softClip = undefined
  squared = undefined

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
  compare _ _ = EQ

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

instance Mergable (Expr a) where
  merge a b = Node "merge" [a,b]

instance Pmerge Expr where
  pmerge p1 p2 = Node "pmerge" [p1,p2]

instance Ppar Expr where
  ppar ps = Node "ppar" ps

instance Psnew Expr where
  psnew def nid aa tid ms = NodeO (Snew def nid aa tid) ms

instance Pnset Expr where
  pnset tid ms = NodeO (Nset tid) ms
