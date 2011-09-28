{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns, take 4.
-}
module Parser4 where

import Data.ByteString (ByteString)
import Data.Data

import System.Random

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.R

data Etree
  = Leaf String
  | Node String [Etree]
    deriving (Eq,Show,Data,Typeable)

newtype E a = E {unE :: Int -> Etree}

etree :: E a -> Etree
etree e = unE e 0

instance Pval E where
  pval a = E $ \_ -> Node "pval" [Leaf $ show a]

instance Plist E where
  plist xs = E $ \_ -> Node "plist" [Leaf $ show xs]

instance Pcycle E where
  pcycle ps = E $ \h -> Node "pcycle" (map (flip unE h) ps)

instance Pempty E where
  pempty = E $ \_ -> Node "pempty" []

instance Pappend E where
  pappend p1 p2 = E $ \h -> Node "pappend" [unE p1 h, unE p2 h]

instance Pconcat E where
  pconcat ps = E $ \h -> Node "pconcat" (map (flip unE h) ps)

instance Preplicate E where
  preplicate p1 p2 = E $ \h -> Node "preplicate" [unE p1 h, unE p2 h]

instance Pseq E where
  pseq pn ps = E $ \h -> Node "pseq" (unE pn h:map (flip unE h) ps)

instance Prange E where
  prange p1 p2 = E $ \h -> Node "prange" [unE p1 h, unE p1 h]

instance Prand E where
  prand pn ps = E $ \h -> Node "prand" (unE pn h:map (flip unE h) ps)

instance Prepeat E where
  prepeat a = E $ \h -> Node "prepeat" [Leaf (show a)]

instance Psnew E where
  psnew d n a t ps = E $ \h ->
    let f [] = []
        f ((k,v):qs) = Leaf k:unE v h:f qs
        ps' = f ps
    in  Node "psnew" (Leaf d: Leaf (show n): Leaf (show a): Leaf (show t): ps')

------------------------------------------------------------------------------
-- Types

data Ty t where
  TyInt       :: Ty Int
  TyDouble    :: Ty Double
  TyAddAction :: Ty AddAction
  TyMsgType   :: Ty MsgType
  TyToOSC     :: Ty a -> Ty (ToOSC a)
  TyArr       :: Ty a -> Ty b -> Ty (a->b)

data Equal a b where
  Equal :: Equal c c

instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy t1 t2 = case (t1,t2) of
  (TyInt,TyInt)       -> Just Equal
  (TyDouble,TyDouble) -> Just Equal
  (TyAddAction,TyAddAction) -> Just Equal
  (TyMsgType,TyMsgType) -> Just Equal
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

safeRead :: Read a => String -> Either String a
safeRead str = case reads str of
  [(a,"")] -> return a
  _        -> Left $ "Failed reading: " ++ str

------------------------------------------------------------------------------
-- Deserialize

-- | We want to expose type r, so that it could be referred
-- as scoped type variable.
--
type FromTree r = forall t a.
  ( Pempty r, Pval r, Plist r, Pappend r, Pconcat r
  , Preplicate r, Pseq r, Prange r, Prand r, Pcycle r
  , Prepeat r
  , Show t, Read t, Random t)
  => Etree -> Ty t -> a -> Either String (Term r)

-- fromTree ::
--   forall t a (r :: * -> *).
--   ( Pempty r, Pval r, Plist r, Pappend r, Pconcat r
--   , Preplicate r, Pseq r, Prange r
--   , Show t, Read t, Random t )
--   => Etree -> Ty t -> a -> Either String (Term r)

-- | Explicitly specifying `(Show t, Read t) => Ty t`, otherwise
-- expressions using Int will fix the result type to `Ty Int`.
--
-- Writing `Term r` to result of do bind for intermediate parse.
--
fromTree :: forall r. FromTree r
fromTree e t d = case e of
  Node "pval" [Leaf x] -> case safeRead x of
    Right y -> return $ Term t (pval y)
    Left err -> Left $ "pval: " ++ err
  Node "plist" [Leaf xs] -> case safeRead xs of
    Right xs' -> return $ Term t (plist xs')
    Left err  -> Left $ "plist: " ++ err
  Node "pempty" [] -> return $ Term t pempty
  Node "prepeat" [Leaf x] -> case safeRead x of
    Right y -> return $ Term t (prepeat y)
  Node "pappend" [e1,e2] -> do
    (Term t1 e1' :: Term r) <- fromTree e1 t d
    (Term t2 e2' :: Term r) <- fromTree e2 t d
    case cmpTy t1 t2 of
      Just Equal -> return (Term t1 (pappend e1' e2'))
      _          -> Left $ "pappend: type mismatch "
  Node "preplicate" [e1,e2] -> do
    (Term TyInt e1' :: Term r) <- fromTree e1 TyInt d
    (Term t2 e2'    :: Term r) <- fromTree e2 t d
    return $ Term t2 (preplicate e1' e2')
  Node "pconcat" es -> do
    terms <- fromTreeList es t d
    return $ Term t (pconcat terms)
  Node "pseq" (en:es) -> do
    (Term TyInt en' :: Term r) <- fromTree en TyInt d
    es' <- fromTreeList es t d
    return $ Term t (pseq en' es')
  Node "prand" (en:es) -> do
    (Term TyInt en' :: Term r) <- fromTree en TyInt d
    es' <- fromTreeList es t d
    return $ Term t (prand en' es')
  Node "prange" [e1,e2] -> do
    (Term t1 e1' :: Term r) <- fromTree e1 t d
    (Term t2 e2' :: Term r) <- fromTree e2 t d
    case cmpTy t1 t2 of
      Just Equal -> return $ Term t1 (prange e1' e2')
      Nothing    -> Left $ "prange: type mismatch"
  Node "pcycle" es -> do
    es' <- fromTreeList es t d
    return $ Term t $ pcycle es'
  where
    fromTreeList es t d
      | null es   = return []
      | otherwise = do
        (Term t1 r1' :: Term r) <- fromTree (head es) t d
        case cmpTy t1 t of
          Just Equal -> do
            (rest :: [r t]) <- fromTreeList (tail es) t d
            return $ r1' : rest

-- fromTreeList ::
--   forall t a (r :: * -> *).
--   ( Pempty r, Pval r, Plist r, Pappend r, Pconcat r
--   , Preplicate r, Pseq r
--   , Show t, Read t )
--   => [Etree] -> Ty t -> a -> Either String [r t]
-- fromTreeList es t d
--   | null es   = return []
--   | otherwise = do
--     (Term t1 r1' :: Term r) <- fromTree (head es) t d
--     case cmpTy t1 t of
--       Just Equal -> do
--         (rest :: [r t]) <- fromTreeList (tail es) t d
--         return $ r1' : rest

-- fromTreeList [] t d = return []
-- fromTreeList (r1:rs) t d = do
--   (Term t1 r1' :: Term r) <- fromTree r1 t d
--   case cmpTy t1 t of
--     Just Equal -> do
--       (rest :: [r t]) <- fromTreeList rs t d
--       return $ r1' : rest

-- fromTreeList [] t d = return []
-- fromTreeList (r1:rs) t d = do
--   (Term t1 r1' :: Term r) <- fromTree r1 t d
--   case cmpTy t1 t of
--     Just Equal -> do
--       (rest :: [r t]) <- fromTreeList rs t d
--       return $ r1' : rest

e2rDouble :: E a -> IO ()
e2rDouble e = case fromTree (etree e) TyDouble [] of
  Right (Term _ r)  -> mapPIO_ print r
  Left err          -> putStrLn err

e2bsDouble :: E a -> Either String ByteString
e2bsDouble e = case fromTree (etree e) TyDouble [] of
  Right (Term _ r)  -> Right $ byteStringP r
  Left err          -> Left err

------------------------------------------------------------------------------
-- Sample

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [ pval 60, prand (pval 1) [pval 63, pval 65]
       , pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("amp", prepeat 0.1)
  ,("freq", pspeFreq)]
