{-# LANGUAGE
   DeriveDataTypeable
 , GADTs
 , KindSignatures
 , NoMonomorphismRestriction
 , OverloadedStrings
 , RankNTypes
 , ScopedTypeVariables
 #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns, take 4. Deserialization was faster than ParseP.
This pattern matcher is working, though does not undersand lam and app.
-}
module Scratch.Parse4 where

import Data.ByteString.Lazy (ByteString)
import Data.Data

import Control.Applicative
import System.Random

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.Play

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Binary as Bin

data Etree
  = Leaf ByteString
  | Node ByteString [Etree]
    deriving (Eq,Show,Data,Typeable)

instance Bin.Binary Etree where
  {-# INLINE put #-}
  put e = case e of
    Leaf n    -> Bin.putWord8 0 *> Bin.put n
    Node s es -> Bin.putWord8 1 *> Bin.put s *> Bin.put es
  {-# INLINE get #-}
  get = Bin.getWord8 >>= \i -> case i of
    0 -> Leaf <$> Bin.get
    1 -> Node <$> Bin.get <*> Bin.get
    _ -> error $ "Unexpected index in get: " ++ show i

-- | Newtype wrapper for converting to expression tree.
newtype E a = E {unE :: Int -> Etree}

toE :: E a -> E a
toE = id

etree :: E a -> Etree
etree e = unE e 0

------------------------------------------------------------------------------
-- Pattern instances

instance Pval E where
  pval a = E $ \_ -> Node "pval" [Leaf $ Bin.encode a]

instance Plist E where
  plist xs = E $ \_ -> Node "plist" [Leaf $ Bin.encode xs]

instance Prepeat E where
  prepeat a = E $ \h -> Node "prepeat" [Leaf (Bin.encode a)]

instance Pempty E where pempty = constE "pempty"

instance Pappend E where pappend = binE "pappend"

instance Pconcat E where pconcat = listE "pconcat"

instance Preplicate E where
  preplicate p1 p2 = E $ \h -> Node "preplicate" [unE p1 h, unE p2 h]

instance Pseq E where
  pseq pn ps = E $ \h -> Node "pseq" (unE pn h:map (flip unE h) ps)

instance Pcycle E where pcycle = listE "pcycle"

instance Pforever E where pforever = unaryE "pforever"

instance Prandom E where prandom = constE "prandom"

instance Prange E where prange = binE "prange"

instance Prand E where
  prand pn ps = E $ \h -> Node "prand" (unE pn h:map (flip unE h) ps)

instance Pchoose E where
  pchoose pn ps = E $ \h -> Node "pchoose" (unE pn h:map (flip unE h) ps)

instance Pshuffle E where pshuffle = listE "pshuffle"

-- XXX: Add ptakeT, pdropT.
-- XXX: Add plam and papp, rewrite with de bruijn?

instance Psnew E where
  psnew d n a t ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "psnew" $
        Leaf (Bin.encode d):Leaf (Bin.encode n):
        Leaf (Bin.encode a):Leaf (Bin.encode t):ps'

instance Pnset E where
  pnset t ps = E $ \h ->
    let ps' = mkParams h ps
    in  Node "pnset" $ Leaf (Bin.encode t):ps'

instance Mergable (E a) where merge = binE "merge"

instance Pmerge E where pmerge = binE "pmerge"

instance Ppar E where ppar = listE "ppar"

instance Pfsm E where
  pfsm is ps = E $ \h ->
    let ps' = f ps
        f qs = case qs of
          []     -> []
          ((r,js):rest) -> unE r h : Leaf (Bin.encode js) : f rest
    in  Node "pfsm" (Leaf (Bin.encode is):ps')

mkParams :: Bin.Binary a => Int -> [(a, E e)] -> [Etree]
mkParams h ps = case ps of
  []         -> []
  ((k,v):qs) -> Leaf (Bin.encode k):unE v h:mkParams h qs

------------------------------------------------------------------------------
-- Util

constE :: ByteString -> E a
constE str = E $ \_ -> Node str []

binE :: ByteString -> E a -> E a -> E a
binE op e1 e2 = E $ \h -> Node op [unE e1 h, unE e2 h]

unaryE :: ByteString -> E a -> E a
unaryE op e = E $ \h -> Node op [unE e h]

listE :: ByteString -> [E a] -> E a
listE str es = E $ \h -> Node str (map (flip unE h) es)

------------------------------------------------------------------------------
-- Base classes

instance Show (E a) where
  show e = show $ etree e

instance Eq (E a) where
  e1 == e2 = show e1 == show e2

instance Ord (E a) where
  compare _ _ = EQ

------------------------------------------------------------------------------
-- Numeric classes

instance Num (E a) where
  (+) = binE "+"
  (*) = binE "*"
  (-) = binE "-"
  negate = unaryE "negate"
  abs = unaryE "abs"
  signum = unaryE "signum"
  fromInteger a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromInteger a :: Double))]

instance Fractional (E a) where
  (/) = binE "/"
  recip = unaryE "recip"
  fromRational a = E $ \h ->
    Node "pval" [Leaf (Bin.encode (fromRational a :: Double))]

instance Floating (E a) where
  pi = constE "pi"
  exp = unaryE "exp"
  sqrt = unaryE "sqrt"
  log = unaryE "log"
  (**) = binE "**"
  logBase = binE "logBase"
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

instance UnaryOp (E a) where
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

------------------------------------------------------------------------------
-- Type representations

data Ty t where
  TyInt    :: Ty Int
  TyDouble :: Ty Double
  TyToOSC  :: Ty a -> Ty (ToOSC a)
  TyArr    :: Ty a -> Ty b -> Ty (a->b)

data Equal a b where
  Equal :: Equal c c

instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy t1 t2 = case (t1,t2) of
  (TyInt,TyInt)       -> Just Equal
  (TyDouble,TyDouble) -> Just Equal
  (TyToOSC a, TyToOSC b) -> do
    Equal <- cmpTy a b
    return Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal
  _ -> Nothing

data Term r where
  Term :: (Random t, Show t) => Ty t -> r t -> Term r

------------------------------------------------------------------------------
-- Deserialize

-- | Exposing type r, so that it could be referred in scoped type variable.
--
-- Also explicitly specifying class constraints for Ty t, otherwise
-- expressions using Int will fix the result type to `Ty Int`.
--
type FromTree r = forall t a.
  ( Pempty r, Pval r, Plist r, Prandom r
  , Pappend r, Pconcat r, Preplicate r, Pseq r
  , Pcycle r, Prepeat r, Pforever r
  , Prange r, Prand r, Pchoose r, Pshuffle r
  , Pfsm r
  , Mergable (r (ToOSC Double)), Pmerge r, Ppar r
  , Psnew r, Pnset r
  , Num (r Int), Num (r t), Num (r (ToOSC Double))
  , UnaryOp (r Double)
  , Show t, Bin.Binary t, Random t)
  => (Etree,Ty t,a) -> Either String (Term r)

-- | fromTreeF tied with fixE.
fromTree :: forall r. FromTree r
fromTree = fixE fromTreeO

-- | Fix function for FromTree.
fixE :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixE f = f (fixE f)

-- | Node matcher for '(ToOSC Double)' patterns.
--
-- XXX: Add pdropT and ptakeT.
--
fromTreeO :: forall r. FromTree r -> FromTree r
fromTreeO self (e,t,d) = case e of
  Node "psnew" (Leaf def:Leaf nid:Leaf aa:Leaf tid:ps) -> do
    let def' = Bin.decode def
        nid' = Bin.decode nid
        aa'  = Bin.decode aa
        tid' = Bin.decode tid
    ps' <- unParams ps
    return $ Term toscd $ psnew def' nid' aa' tid' ps'
  Node "pnset" (Leaf tid:ps) -> do
    let tid' = Bin.decode tid
    ps' <- unParams ps
    return $ Term toscd $ pnset tid' ps'
  Node "pmerge" [e1,e2] -> do
    term1@(Term (t1@(TyToOSC TyDouble)) e1' :: Term r) <- self (e1,toscd,d)
    (Term t2 e2' :: Term r) <- self (e2,t,d) `asTypeOf` Right term1
    case cmpTy t1 t2 of
      Just Equal -> return $ Term toscd $ pmerge e1' e2'
      Nothing    -> Left $ "pmerge: type mismatch"
  Node "ppar" ps -> do
    ps' <- fromTreeOList ps t d
    return $ Term toscd (ppar ps')
  _ -> fromTreeF self (e,t,d)
  where
    toscd = TyToOSC TyDouble
    unParams ps = case ps of
      []            -> return []
      (Leaf k:v:qs) -> do
        (Term TyDouble r :: Term r) <- fixE fromTreeF (v,TyDouble,d)
        ((Bin.decode k,r):) <$> unParams qs
    fromTreeOList es t d
      | null es = return []
      | otherwise = do
        (Term (t1@(TyToOSC TyDouble)) r1' :: Term r) <- self ((head es),t,d)
        case cmpTy t1 t of
          Just Equal -> do
            (rest :: [r t]) <- fromTreeOList (tail es) t d
            return $ r1' : rest

-- | Node matcher for basic pattern classes.
--
-- Writing ('Term' r) to result of do bind for intermediate parse.
--
-- XXX: Add pfsm.
--
fromTreeF :: forall r. FromTree r -> FromTree r
fromTreeF self (e,t,d) = case e of
  Node "pval" [Leaf x] -> return $ Term t (pval (Bin.decode x))
  Node "plist" [Leaf xs] -> return $ Term t (plist (Bin.decode xs))
  Node "prepeat" [Leaf x] -> return $ Term t (prepeat (Bin.decode x))
  Node "pempty" [] -> return $ Term t pempty
  Node "pappend" [e1,e2] -> do
    (Term t1 e1' :: Term r) <- self (e1,t,d)
    (Term t2 e2' :: Term r) <- self (e2,t,d)
    case cmpTy t1 t2 of
      Just Equal -> return (Term t1 (pappend e1' e2'))
      _          -> Left $ "pappend: type mismatch "
  Node "pconcat" es -> do
    terms <- fromTreeList es t d
    return $ Term t (pconcat terms)
  Node "preplicate" [e1,e2] -> do
    (Term TyInt e1' :: Term r) <- self (e1,TyInt,d)
    (Term t2 e2'    :: Term r) <- self (e2,t,d)
    return $ Term t2 (preplicate e1' e2')
  Node "pseq" (en:es) -> do
    (Term TyInt en' :: Term r) <- self (en,TyInt,d)
    es' <- fromTreeList es t d
    return $ Term t (pseq en' es')
  Node "pforever" [e1] -> do
    (Term t' e' :: Term r) <- self (e1,t,d)
    case cmpTy t t' of
      Just Equal -> return $ Term t $ pforever e'
      Nothing    -> Left "pforever: type mismatch"
  Node "pcycle" es -> do
    es' <- fromTreeList es t d
    return $ Term t $ pcycle es'
  Node "prange" [e1,e2] -> do
    (Term t1 e1' :: Term r) <- self (e1,t,d)
    (Term t2 e2' :: Term r) <- self (e2,t,d)
    case cmpTy t1 t2 of
      Just Equal -> return $ Term t1 (prange e1' e2')
      Nothing    -> Left $ "prange: type mismatch"
  Node "prand" (en:es) -> do
    (Term TyInt en' :: Term r) <- self (en,TyInt,d)
    es' <- fromTreeList es t d
    return $ Term t (prand en' es')
  Node "pchoose" (en:es) -> do
    (Term TyInt en' :: Term r) <- self (en,TyInt,d)
    es' <- fromTreeList es t d
    return $ Term t (pchoose en' es')
  _ -> fromTreeU self (e,t,d)
  where
    fromTreeList es t d
      | null es   = return []
      | otherwise = do
        (Term t1 r1' :: Term r) <- self ((head es),t,d)
        case cmpTy t1 t of
          Just Equal -> do
            (rest :: [r t]) <- fromTreeList (tail es) t d
            return $ r1' : rest

-- | Node matcher for UnaryOp functions.
-- Result type is fixed to 'Ty Double'.
fromTreeU :: forall r. FromTree r -> FromTree r
fromTreeU self (e,t,d) = case e of
  Node name [e1] -> case lookup name ops of
    Just op -> do
      (Term TyDouble e' :: Term r) <- self (e1,t,d)
      return $ Term TyDouble (op e')
    Nothing -> fromTreeN self (e,t,d)
  _ -> fromTreeN self (e,t,d)
  where
    ops :: [(ByteString,r Double-> r Double)]
    ops =
      [("ampDb",ampDb),("asFloat",asFloat),("asInt",asInt),("bitNot",bitNot)
      ,("cpsMIDI",cpsMIDI),("cpsOct",cpsOct),("cubed",cubed),("dbAmp",dbAmp)
      ,("distort",distort),("frac",frac),("isNil",isNil),("log10",log10)
      ,("log2",log2),("midiCPS",midiCPS),("midiRatio",midiRatio),("notE",notE)
      ,("notNil",notNil),("octCPS",octCPS),("ramp_",ramp_)
      ,("ratioMIDI",ratioMIDI),("softClip",softClip),("squared",squared)]

-- | Node matcher for Num functions.
fromTreeN :: forall r. FromTree r -> FromTree r
fromTreeN self (e,t,d) = case e of
  Node "+" [e1,e2] -> ap2 (+) e1 e2 t d
  Node "*" [e1,e2] -> ap2 (*) e1 e2 t d
  Node "-" [e1,e2] -> ap2 (-) e1 e2 t d
  Node "negate" [e1] -> ap1 negate e1 t d
  Node "abs" [e1] -> ap1 abs e1 t d
  Node "signum" [e1] -> ap1 abs e1 t d
  _ -> fromTreeD self (e,t,d)
  where
    ap1 :: (Bin.Binary t, Random t, Show t, Num (r t))
        => (r t -> r t) -> Etree -> Ty t -> g -> Either String (Term r)
    ap1 f e1 t d = do
      (Term t1 e1' :: Term r) <- self (e1,t,d)
      case cmpTy t t1 of
        Just Equal -> return $ Term t (f e1')
        _          -> Left $ "Type mismatch in Num function"
    ap2 :: (Bin.Binary t, Random t, Show t, Num (r t))
        => (r t -> r t -> r t) -> Etree -> Etree -> Ty t -> g
        -> Either String (Term r)
    ap2 op e1 e2 t d = do
      Term t1 e1' <- self (e1,t,d)
      Term t2 e2' <- self (e2,t,d)
      case (cmpTy t t1, cmpTy t t2) of
        (Just Equal,Just Equal) -> return $ Term t (op e1' e2')
        _                       -> Left $ "Type mismatch in Num op"

-- | Node matcher for Double, functions in Floating and Fractional classes.
fromTreeD :: forall r. FromTree r -> FromTree r
fromTreeD self (e,t,d) = case e of
  Node "/" [e1,e2] -> do
    (Term TyDouble e1' :: Term r) <- self (e1,t,d)
    (Term TyDouble e2' :: Term r) <- self (e2,t,d)
    return $ Term TyDouble (e1'/e2')
  Node "recip" [e1] -> do
    (Term TyDouble e1' :: Term r) <- self (e1,t,d)
    return $ Term TyDouble (recip e1')
  Node "pi" [] -> return $ Term TyDouble pi
  Node "**" [e1,e2] -> do
    (Term TyDouble e1' :: Term r) <- self (e1,t,d)
    (Term TyDouble e2' :: Term r) <- self (e2,t,d)
    return $ Term TyDouble (e1' ** e2')
  Node "logBase" [e1,e2] -> do
    (Term TyDouble e1' :: Term r) <- self (e1,t,d)
    (Term TyDouble e2' :: Term r) <- self (e2,t,d)
    return $ Term TyDouble (logBase e1' e2')
  Node name [e1] -> case lookup name ops of
    Just op -> do
      (Term TyDouble e1' :: Term r) <- self (e1,t,d)
      return $ Term TyDouble (op e1')
    Nothing -> unknown name
  Node name _ -> unknown name
  where
    unknown x = Left $ "Unknown term: " ++ (LC8.unpack x)
    ops :: [(ByteString,r Double -> r Double)]
    ops =
      [("exp",exp),("sqrt",sqrt),("log",log)
      ,("sin",sin),("tan",tan),("cos",cos)
      ,("asin",asin),("atan",atan),("acos",acos)
      ,("sinh",sinh),("tanh",tanh),("cosh",cosh)
      ,("asinh",asinh),("atanh",atanh),("acosh",acosh)]

------------------------------------------------------------------------------
-- Variants of fromTree with fixed type.

e2rD :: Etree -> Either String (R Double)
e2rD e = case fromTree (e,TyDouble,undefined) of
  Right (Term TyDouble r :: Term R) -> return r
  Left err -> Left err

e2bzD :: Etree -> Either String (Bz Double)
e2bzD e = case fromTree (e,TyDouble,undefined) of
  Right (Term TyDouble r :: Term Bz) -> return r
  Left err -> Left err

e2r :: E (ToOSC Double) -> Either String (R (ToOSC Double))
e2r e = case fromTree (etree e,TyToOSC TyDouble,undefined) of
  Right (Term (TyToOSC TyDouble) r :: Term R) -> return r
  Left err -> Left err

tree2r :: Etree -> Either String (R (ToOSC Double))
tree2r e = case fromTree (e,TyToOSC TyDouble,undefined) of
  Right (Term (TyToOSC TyDouble) r :: Term R) -> return r
  Left err -> Left err

e2rio :: E (ToOSC Double) -> IO ()
e2rio e = case e2r e of Right r -> audition $ toR r

tree2bz :: Etree -> Either String (Bz (ToOSC Double))
tree2bz e = case fromTree (e,TyToOSC TyDouble,undefined) of
  Right (Term (TyToOSC TyDouble) r :: Term Bz) -> return r
  Left err -> Left err

e2bz :: E (ToOSC Double) -> Either String (Bz (ToOSC Double))
e2bz e = case fromTree (etree e,TyToOSC TyDouble,undefined) of
  Right (Term (TyToOSC TyDouble) r :: Term Bz) -> return r
  Left err -> Left err

------------------------------------------------------------------------------
-- Sample data

-- ghci> e2bz pspe
-- Right Snew "speSynth" Nothing AddToTail 1 [("dur",prepeat
-- 0.13),("amp",prepeat 0.1),("freq",pcycle [prand (pval 1) [pempty,plist
-- [24.0,31.0,36.0,43.0,48.0,55.0]],pseq (prange (pval 2) (pval 2)) [pval
-- 60.0,prand (pval 1) [pval 63.0,pval 65.0],pval 67.0,prand (pval 1)
-- [pval 70.0,pval 72.0,pval 74.0]],prand (prange (pval 3) (pval 3))
-- [pval 74.0,pval 75.0,pval 77.0,pval 79.0,pval 81.0]])]

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [pval 60, prand (pval 1) [pval 63, pval 65]
       ,pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("amp", prepeat 0.1)
  ,("freq", midiCPS pspeFreq)]

pspe2 = pmerge pspe pspe

pspe3 = ppar [pspe, pspe2]

pspe4 = preplicate (pval 3) pspe2

p05 = psnew "foo" Nothing AddToTail 1
  [("bar", plist [1,2,3] + preplicate (pval 3) (prange (0.1) 0.2))]