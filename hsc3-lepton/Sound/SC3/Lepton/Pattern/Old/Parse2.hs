{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns encoded in ByteString with Bz, take 2.

Using dedicated data type to hold intermediate data structure.

-}
module Sound.SC3.Lepton.Pattern.Parse2
  ( PL(..)
  , toPL
  , fromPL
  , fromPLExt
  , parsePL
  ) where

import Control.Applicative hiding (many)
import Data.Function (fix)
import System.Random
import Prelude hiding (takeWhile)

import Data.Attoparsec.Lazy hiding (takeWhile, takeWhile1)
import Data.Attoparsec.Char8 hiding (Result(..), eitherResult, parse)
import Sound.SC3 hiding ((<*))

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified Data.Attoparsec.Char8 as A

import Sound.SC3.Lepton.Pattern.Dummy ()
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.Expression hiding (Plc(..))

data PL a
  = Atom a
  | BString C8.ByteString
  | PInt Int
  | PLI (PL Int)
  | PDouble Double
  | Pair (PL a) (PL a)
  | List [PL a]
  | Func C8.ByteString [PL a]
  | Var C8.ByteString
  | Lam C8.ByteString (PL a)
  | App (PL a) (PL a)
  deriving (Eq, Show)

toPL :: PL a -> PL a
toPL = id

parsePL :: LC8.ByteString -> Result (PL Double)
parsePL = parse (pat double)

instance Functor PL where
  fmap f (Atom a)    = Atom (f a)
  fmap _ (BString s) = BString s
  fmap f (List ps)   = List (map (fmap f) ps)
  fmap _ (Var s)     = Var s
  fmap f (Func n ps) = Func n (map (fmap f) ps)
  fmap f (Lam n p)   = Lam n (fmap f p)
  fmap f (App a b)   = App (fmap f a) (fmap f b)

-- instance Pval PL where pval a = Func "pval" [Atom a]
-- instance Plist PL where plist as = Func "plist" [List (map Atom as)]
-- instance Pempty PL where pempty = Func "pempty" []
-- instance Prepeat PL where prepeat a = Func "prepeat" [Atom a]
-- instance Prandom PL where prandom = Func "prandom" []

-- instance Pappend PL where pappend p1 p2 = Func "pappend" [p1,p2]
-- instance Pconcat PL where pconcat ps = Func "pconcat" [List ps]
-- instance Prange PL where prange p1 p2 = Func "prange" [p1,p2]
-- instance Pshuffle PL where pshuffle ps = Func "pshuffle" [List ps]

-- instance Plam PL where
--   plam f = Func "plam" [Lam "x" (\x -> f (Var "x"))] 

-- XXX: Added adhoc constructor PLI.
-- instance Pseq PL where pseq pn ps = Func "pseq" [PLI pn,List ps]
-- instance Preplicate PL where preplicate pn p = Func "preplicate" [PLI pn,p]
-- instance Pcycle PL where pcycle ps = Func "pcycle" [List ps]
-- instance Pforever PL where pforever p = Func "pforever" [p]
-- instance Prand PL where prand pn ps = Func "prand" [PLI pn,List ps]

unAtom :: PL t -> t
unAtom x = case x of
  Atom a -> a
  _      -> error "Not an atom."

fix2 :: (a -> b -> a) -> b -> a
fix2 f g = f (fix2 f g) g

fromPL = fix fromPLExt

{-
XXX: Restricted to double.

Using 'truncate' to get Int values. If the recursion could isolated
as in Expr, fmap truncate could be removed from patterns taking Int args.

forall. (PL a -> PL a)
-}
fromPLExt f e = case e of
  Func name args -> case (name,args) of
    ("pval",[Atom a])        -> pval a
    ("pempty",[])            -> pempty
    ("plist",[List as])      -> plist (map unAtom as)
    ("pappend",[p1,p2])      -> pappend (f p1) (f p2)
    ("pconcat",[List ps])    -> pconcat (map f ps)
    ("pcycle",[List ps])     -> pcycle (map f ps)
    ("prange",[p1,p2])       -> prange (f p1) (f p2)
    ("pseq",(ph:List ps:_))  -> pseq (fmap truncate $ f ph) (map f ps)
    ("prand",(ph:List ps:_)) -> prand (fmap truncate $ f ph) (map f ps)
  Lam v bdy   -> undefined
  -- Lam v bdy   -> plam (\x -> f $ resolve v x bdy)
  App fun val -> undefined
  _ -> error "Not yet ready"

{-
\(Lam v bdy) -> plam (\x -> fromPL $ resolve v x bdy)
  :: (Pempty p,
      Pval p,
      Plist p,
      Pconcat p,
      Pappend p,
      Pseq p,
      Pcycle p,
      Prange p,
      Prand p,
      Plam p,
      Random (p a),
      Show a,
      RealFrac (p a),
      Functor p) =>
     PL (p a) -> p (a -> p (p a))

.. So, we get stucked. Need a somewhat trick to return (p a) and (p (a -> p a)) 
in same function. 

TypeCheck.hs source code in ttf course is using DynTerm data type, and its 
signature is:

> data DynTerm repr h = forall a. DynTerm (TQ a) (repr h a)

Learn from this.

-}

resolve v x pl = case pl of
  Var v' | v' == v -> Atom x | otherwise -> Var v'
  Atom _ -> pl
  BString _ -> pl
  Pair p1 p2 -> Pair (resolve v x p1) (resolve v x p2)
  List ps -> List (map (resolve v x) ps)
  Func n args -> Func n (map (resolve v x) args)
  Lam n p -> Lam n (resolve v x p)
  App p1 p2 -> App (resolve v x p1) (resolve v x p2)

fromPLExtI f fi e = case e of
  Func name args -> case (name,args) of
    ("pseq", (pn:ps))     -> pseq (fi pn) (map f ps)
    ("prand", (pn:ps))    -> prand (fi pn) (map f ps)
  _ -> fromPLExt f e

pat p =
  lam p <|>
  app p <|>
  pat' p <|>
  (swapI <$> ifunc integral)

pat' p = choice
  [ atom p
  , list p
  , func p
  , var
  ]

atom p = Atom <$> p
bstring = BString <$> takeWhile1 (/= ' ')

list p = do
  char '['
  vs <- pat' p `sepBy` char ','
  char ']'
  return $ List vs

-- funcNames  = ["pval","plist","pappend","pconcat","prange","pcycle","pempty"]
-- iFuncNames = ["pseq","prand"]

funcNames  =
  ["pval","plist","pappend","pconcat","prange","pcycle","pempty","pseq","prand"]
iFuncNames = []

func p = choice (map g funcNames) where
  g name = do
    name' <- string name
    skipSpace
    args <- (braced (func p) <|> braced var <|> atom p <|> list p) `sepBy` char ' '
    return $ Func name' args

ifunc p = choice (map g iFuncNames) where
  g name = do
    name' <- string name
    skipSpace
    iarg <- braced (ifunc integral <|> func integral) <|>
            atom integral <|> list integral
    skipSpace
    args <- (braced (func p) <|> atom p <|> list p) `sepBy` char ' '
    return $ Func name' (iarg:args)

var = Var <$> varName

varName = do
  x <- char 'x'
  idx <- takeWhile (isDigit)
  return $ C8.cons x idx

lam p = do
  string "plam (\\"
  v <- varName
  skipSpace >> string "->" >> skipSpace
  bdy <- (Var <$> string v) <|> pat p
  char ')'
  return $ Lam v bdy

app p = do
  string "papp"
  skipSpace
  body <- braced (pat p)
  skipSpace
  val <- braced (pat p)
  return $ App body val

swapI a = case a of
  Atom x    -> Atom (fromInteger x)
  List ps   -> List (map swapI ps)
  Var n     -> Var n
  Func n ps -> Func n (map swapI ps)
  Lam n p   -> Lam n (swapI p)
  App b v   -> App (swapI b) (swapI v)

braced :: Parser a -> Parser a
braced a = char '(' *> a <* char ')'

integral :: Integral a => Parser a
integral = decimal <|> signed decimal <|> braced (signed decimal)

addAction :: Parser AddAction
addAction = choice $
  map (\x -> string (C8.pack (show x)) >> return x) [AddToHead .. AddReplace]

------------------------------------------------------------------------------
-- Sample inputs
--

pat_01 = LC8.pack "pval 1"
pat_02 = LC8.pack "plist [1.8,2,3.5,4.5,5.0]"
pat_03 = LC8.pack "pappend (pval 1) (plist [1.0,2.0,3.0])"
pat_04 =
  LC8.pack "pconcat [pval 1,plist [1.0,2.0,3.0],pappend (pval 2) (plist [3.0])]"
pat_05 = LC8.pack "prange (pval 1) (pval 8)"
pat_06 =
  LC8.pack "pseq (prange (pval 1) (pval 8)) [pval 1,prange (pval 2) (pval 9),pval 10]"
pat_07 = LC8.pack "plam (\\x0 -> x0)"
pat_08 = LC8.pack "plam (\\x0 -> pconcat [pval 1,x0,prange (x0) (pval 10)])"
pat_09 = LC8.pack "papp (plam (\\x0 -> x0)) (pval 1)"
pat_10 =
  LC8.pack "papp (papp (plam (\\x0 -> plam (\\x1 -> pconcat [x0,x1]))) (pval 10)) (pval 20)"
pat_pspe =
  LC8.pack "pcycle [prand (pval 1) [pempty,plist [24,31,36,43,48,55]],pseq (prange (pval 2) (pval 5)) [pval 60,prand (pval 1) [pval 63,pval 65],pval 67,prand (pval 1) [pval 70,pval 72,pval 74]],prand (prange (pval 3) (pval 9)) [pval 74,pval 75,pval 77,pval 79,pval 81]]"

-- test = do
--   let f = print . fmap (toBz . fromPL) . parse (pat double)
--   f pat_01
--   f pat_02
--   f pat_03
--   f pat_04
