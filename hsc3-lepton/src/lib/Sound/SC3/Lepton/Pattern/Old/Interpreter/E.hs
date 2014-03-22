{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Syntax of patterns for serialization and deserialization, take 2.

Data type 'E' does not takes no parameter. Value used for Leaf is
fixed to String type. Using wrapped 'read' function to get the value,
thus those primitive values are restricted to types which having Read
instance.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.E
  ( E(..)
  -- , fromE
  , toE
  ) where

import Control.Applicative
import Data.Data
import Data.Function (fix)
import System.Random

import qualified Data.Binary as Bin

import Sound.SC3
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.Bz

import qualified Sound.SC3.Lepton.Pattern.Expression as P

data E
  = Leaf String
  | Node String [E]
  deriving (Eq, Show, Data, Typeable)

instance Bin.Binary E where
  put e = case e of
    Leaf x       -> Bin.putWord8 0 >> Bin.put x
    Node n es    -> Bin.putWord8 1 >> Bin.put n >> Bin.put es
  get = do
    idx <- Bin.getWord8
    case idx of
      0 -> Leaf <$> Bin.get
      1 -> Node <$> Bin.get <*> Bin.get
      _ -> error $ "Unknown index in E get: " ++ show idx

toE :: E -> E
toE = id

unLeaf :: E -> Either String String
unLeaf (Leaf s) = Right s
unLeaf _        = Left "Not a leaf"

safeRead :: Read a => String -> Either String a
safeRead x = case reads x of
  [(y,[])] -> Right y
  _        -> Left $ "safeRead: failed reading '" ++ x ++ "'"

fix2 :: (a -> b -> a) -> b -> a
fix2 f g = f (fix2 f g) g

fix3 :: (a -> b -> c -> a) -> b -> c -> a
fix3 f g h = f (fix3 f g h) g h

fix3' :: (a -> b -> c -> a) -> (c -> b) -> c -> a
fix3' f g h = f (fix3' f g h) (g h) h

-- f' = fix3 fromE' (fix (fix2 (fix3 fromE'))) id
-- f = fix2 fromNodeO (fix2 fromNodeI (fix (fix2 fromNodeI)))
-- fromEa = fix (fix2 (fix3 fromE'))
-- fromEb = fix (fix3' fromE' (fix (fix3' fromE')))

-- fI = fix2 fromNodeI (fix (fix2 fromNodeI))

-- fII = fix2 fromNodeI (fix fromNodePattern)

-- fromE = fix3 fromNodeO fI fI

-- fromNodeO f1 f2 f3 e = case e of
--   Node "psnew" [Leaf def,Leaf nid,Leaf aa,Leaf tid,ps] ->
--     psnew def <$> safeRead nid <*> safeRead aa <*> safeRead tid <*> undefined
--   Node "pmerge" [p1,p2]          -> pmerge <$> f1 p1 <*> f1 p2
--   Node "ppar" ps                 -> ppar <$> (mapM f1 ps)
--   _ -> fromNodeI f1 f2 e
--   where
--     f3' = mapM (\(k,p) -> (k,) <$> f3 p)

-- fromNodeI fg fi e = case e of
--   -- NodeI "preplicate" n [p] -> preplicate <$> fi n <*> fg p
--   -- NodeI "pseq" n ps        -> pseq <$> fi n <*> (mapM fg ps)
--   -- NodeI "prand" n ps       -> prand <$> fi n <*> (mapM fg ps)
--   -- NodeI "pchoose" n ps     -> pchoose <$> fi n <*> (mapM fg ps)
--   _ -> fromNodePattern fg e

-- fi        :: Patterns p => E -> Either String (p Int)
-- f         :: Patterns p => E -> Either String (p a)
-- fromNodeI :: Patterns p =>
--              (E -> Either String (p Int)) -> (E -> Either String (p a)) ->
--               E -> Either String (p a)

fromNodeO g h fo e = case e of
  Node "psnew" (Leaf d:Leaf n:Leaf a:Leaf t:ps) ->
    let ps' = undefined
    in  P.psnew d <$> safeRead n <*> safeRead a <*> safeRead t <*> ps'
  Node "pnset" (Leaf t:ps) ->
    let ps' = undefined
    in  P.pnset <$> safeRead t <*> ps'
  _                        -> fromNodeI g h e

fromNodeI f fi e = case e of
  Node "preplicate" [pn,p] -> P.preplicate <$> fi pn <*> f p
  Node "pseq" (pn:ps)      -> P.pseq <$> fi pn <*> mapM f ps
  Node "prand" (pn:ps)     -> P.prand <$> fi pn <*> mapM f ps
  _                        -> fromNodePattern f e

fromNodePattern f e = case e of
  Node "pval" [Leaf x]    -> P.pval <$> (safeRead x)
  Node "plist" ps         -> P.plist <$> mapM ((safeRead =<<) . unLeaf) ps
  Node "prepeat" [Leaf x] -> P.prepeat <$> (safeRead x)
  Node "pempty" []        -> pure P.pempty
  Node "pappend" [p1,p2]  -> P.pappend <$> f p1 <*> f p2
  Node "pconcat" ps       -> P.pconcat <$> (mapM f ps)
  Node "pcycle" ps        -> P.pcycle <$> (mapM f ps)
  Node "pforever" [p]     -> fmap P.pforever (f p)
  Node "prange" [p1,p2]   -> P.prange <$> (f p1) <*> (f p2)
  Node "prandom" []       -> pure P.prandom
  Node "pshuffle" ps      -> P.pshuffle <$> (mapM f ps)
  _                       -> fromNum f e

fromNum f e = case e of
  Node "+" [a,b]    -> (+) <$> f a <*> f b
  Node "*" [a,b]    -> (*) <$> f a <*> f b
  Node "-" [a,b]    -> (-) <$> f a <*> f b
  Node "negate" [a] -> fmap negate (f a)
  Node "abs" [a]    -> fmap abs (f a)
  Node "signum" [a] -> fmap signum (f a)
  _                 -> fromFractional f e

fromFractional f e = case e of
  Node "/" [a,b]   -> (/) <$> f a <*> f b
  Node "recip" [a] -> fmap recip (f a)
  _                -> fromFloating f e

fromFloating f e = case e of
  Node "pi" []         -> pure pi
  Node "exp" [a]       -> fmap exp (f a)
  Node "sqrt" [a]      -> fmap sqrt (f a)
  Node "log" [a]       -> fmap log (f a)
  Node "**" [a,b]      -> (**) <$> f a <*> f b
  Node "logBase" [a,b] -> logBase <$> (f a) <*> (f b)
  Node "sin" [a]       -> fmap sin (f a)
  Node "tan" [a]       -> fmap tan (f a)
  Node "cos" [a]       -> fmap cos (f a)
  Node "asin" [a]      -> fmap asin (f a)
  Node "atan" [a]      -> fmap atan (f a)
  Node "acos" [a]      -> fmap acos (f a)
  Node "sinh" [a]      -> fmap sinh (f a)
  Node "tanh" [a]      -> fmap tanh (f a)
  Node "cosh" [a]      -> fmap cosh (f a)
  Node "asinh" [a]     -> fmap asinh (f a)
  Node "atanh" [a]     -> fmap atanh (f a)
  Node "acosh" [a]     -> fmap acosh (f a)
  _                    -> fromUnary f e

fromUnary f e = case e of
  Node "ampDb" [a]     -> fmap ampDb (f a)
  Node "asFloat" [a]   -> fmap asFloat (f a)
  Node "asInt" [a]     -> fmap asInt (f a)
  Node "bitNot" [a]    -> fmap bitNot (f a)
  Node "cpsMIDI" [a]   -> fmap cpsMIDI (f a)
  Node "cpsOct" [a]    -> fmap cpsOct (f a)
  Node "cubed" [a]     -> fmap cubed (f a)
  Node "dbAmp" [a]     -> fmap dbAmp (f a)
  Node "distort" [a]   -> fmap distort (f a)
  Node "frac" [a]      -> fmap frac (f a)
  Node "isNil" [a]     -> fmap isNil (f a)
  Node "log10" [a]     -> fmap log10 (f a)
  Node "log2" [a]      -> fmap log2 (f a)
  Node "midiCPS" [a]   -> fmap midiCPS (f a)
  Node "midiRatio" [a] -> fmap midiRatio (f a)
  Node "notE" [a]      -> fmap notE (f a)
  Node "notNil" [a]    -> fmap notNil (f a)
  Node "octCPS" [a]    -> fmap octCPS (f a)
  Node "ramp_" [a]     -> fmap ramp_ (f a)
  Node "ratioMIDI" [a] -> fmap ratioMIDI (f a)
  Node "softClip" [a]  -> fmap softClip (f a)
  Node "squared" [a]   -> fmap squared (f a)
  _                    -> Left $ "Unknown: " ++ show e

-- instance Pval E where pval a = Node "pval" [Leaf $ show a]
-- instance Plist E where plist as = Node "plist" (map (Leaf . show) as)
-- instance Prepeat E where prepeat a = Node "prepeat" [Leaf $ show a]

-- instance Pempty E where pempty = Node "pempty" []
-- instance Pappend E where pappend p1 p2 = Node "pappend" [p1,p2]
-- instance Pconcat E where pconcat ps = Node "pconcat" ps
-- instance Pcycle E where pcycle ps = Node "pcycle" ps
-- instance Prange E where prange p1 p2 = Node "prange" [p1,p2]
-- instance Pforever E where pforever p = Node "pforever" [p]
-- instance Prandom E where prandom = Node "prandom" []
-- instance Pshuffle E where pshuffle ps = Node "pshuffle" ps

-- instance Mergable (E a) where merge a b = Node "merge" [a,b]
-- instance Pmerge E where pmerge a b = Node "pmerge" [a,b]
-- instance Ppar E where ppar ps = Node "ppar" ps

-- instance Pseq E where pseq pn ps = Node "pseq" [pn,ps]
-- instance Prand E where prand pn ps = Node "prand" [pn,ps]
-- instance Pchoose E where pchoose pn ps = Node "pchoose" [pn,ps]
-- instance Preplicate E where
--   preplicate pn p = Node "preplicate" [pn,p]

-- instance Psnew E where
--   psnew d n a t ps =
--     Node "psnew" ([Leaf d, Leaf (show n), Leaf (show a), Leaf (show t)] ++ f ps)
--     where
--       f []         = []
--       f ((k,v):ps) = Leaf k : v : f ps

-- instance Pnset E where
--   pnset tid ps = Node (Nset tid) ps

-- instance Pseq E where pseq pn ps = NodeI "pseq" pn ps
-- instance Prand E where prand pn ps = NodeI "prand" pn ps
-- instance Pchoose E where pchoose pn ps = NodeI "pchoose" pn ps
-- instance Preplicate E where preplicate pn p = NodeI "preplicate" pn [p]

-- instance Psnew E where psnew def nid aa tid ps = NodeO (Snew def nid aa tid) ps
-- instance Pnset E where pnset tid ps = NodeO (Nset tid) ps

-- instance Functor E where
--   fmap _ (Leaf x) = Leaf x
--   fmap f (Node n es) = Node n (map (fmap f) es)

pval a = Node "pval" [Leaf (show a)]
plist as = Node "plist" (map (Leaf . show) as)
pempty = Node "pempty" []
prandom = Node "prandom" []
prepeat a = Node "prepeat" [Leaf (show a)]

pappend p1 p2 = Node "pappend" [p1,p2]
pconcat ps = Node "pconcat" ps
pcycle ps = Node "pcycle" ps
pforever p = Node "pforever" [p]
prange p1 p2 = Node "prange" [p1,p2]
pshuffle ps = Node "pshuffle" ps

pmerge p1 p2 = Node "pmerge" [p1,p2]
ppar ps = Node "ppar" ps

preplicate pn p = Node "preplicate" [pn,p]
pseq pn ps = Node "pseq" (pn:ps)
pchoose pn ps = Node "pchoose" (pn:ps)
prand pn ps = Node "prand" (pn:ps)

psnew :: String -> Maybe Int -> AddAction -> Int -> [(String,E)] -> E
psnew d n a t ps =
  Node "psnew" ([Leaf d,Leaf (show n),Leaf (show a),Leaf (show t)] ++ f ps)
  where
    f []         = []
    f ((k,v):qs) = Leaf k : v : f qs

pnset :: Int -> [(String,E)] -> E
pnset t ps =
  Node "pnset" (Leaf (show t) : f ps)
  where
    f []         = []
    f ((k,v):qs) = Leaf k : v : f qs

default (Integer, Double)

instance Num E where
  a + b = Node "+" [a,b]
  a * b = Node "*" [a,b]
  a - b = Node "-" [a,b]
  negate a = Node "negate" [a]
  abs a = Node "abs" [a]
  signum a = Node "signum" [a]
  fromInteger a = Node "pval" [Leaf $ show (fromInteger a :: Integer)]

instance Fractional E where
  a / b = Node "/" [a,b]
  recip a = Node "recip" [a]
  fromRational a = Node "pval" [Leaf $ show (fromRational a :: Double)]

instance Floating E where
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

instance Ord E where
  compare _ _ = EQ

instance UnaryOp E where
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

---------------------------------------------------------------------------------
-- -- dummy

instance Random a => Random (ToOSC a) where
  random  = error "random not defined for (ToOSC a)"
  randomR = error "randomR not defined for (ToOSC a)"

instance Random String where
  random  = error "random not defined for String"
  randomR = error "randomR not defined for String"

instance Fractional Int
instance Floating Int
instance UnaryOp Int

------------------------------------------------------------------------------
-- sample

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

pspeFreq =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]
