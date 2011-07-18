{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Example of gdiff written in its haddoc.
--
-- * http://en.wikibooks.org/wiki/Haskell/GADT
-- * http://www.haskell.org/haskellwiki/GADT
-- * Do 142857 trick in hexadecimal
-- 
module GDiffScratch where

import Data.Maybe

import Data.Generic.Diff
import Sound.SC3.Lepton

import Sample

-- import Sound.SC3.Lepton

-- Example shown in haddock of Data.Generics.Diff
-- 
-- Expr and Term data type refers each other. Add Val consturctor to Expr cause 
-- it does not terminate otherwise.

data Expr = Min Expr Term | Val Term
          deriving (Eq, Show)
                   
data Term = Parens Expr | Number Int
          deriving (Eq, Show)

data ExprTermFamily :: * -> * -> * where
  Min'    :: ExprTermFamily Expr (Cons Expr (Cons Term Nil))
  Val'    :: ExprTermFamily Expr (Cons Term Nil)
  Parens' :: ExprTermFamily Term (Cons Expr Nil)
  Number' :: ExprTermFamily Term (Cons Int Nil)
  Int'    :: Int -> ExprTermFamily Int Nil
                               
instance Family ExprTermFamily where
  decEq Min' Min' = Just (Refl, Refl)
  decEq Val' Val' = Just (Refl, Refl)
  decEq Parens' Parens' = Just (Refl, Refl)
  decEq Number' Number' = Just (Refl, Refl)
  decEq (Int' x) (Int' y) | x == y = Just (Refl, Refl)
                          | otherwise = Nothing
  decEq _ _ = Nothing

  fields Min' (Min e t) = Just (CCons e (CCons t CNil))
  fields Val' (Val t)   = Just (CCons t CNil)
  fields Parens' (Parens e) = Just (CCons e CNil)
  fields Number' (Number i) = Just (CCons i CNil)
  fields (Int' _) _         = Just CNil
  fields _ _                = Nothing

  apply Min' (CCons e (CCons t CNil)) = Min e t
  apply Val' (CCons t CNil)           = Val t
  apply Parens' (CCons e CNil) = Parens e
  apply Number' (CCons i CNil) = Number i
  apply (Int' i) CNil = i

  string Min' = "Min"
  string Val' = "Val"
  string Parens' = "Parens"
  string Number' = "Number"
  string (Int' i) = show i

instance Type ExprTermFamily Term where
  constructors = [Concr Number', Concr Parens']
  
instance Type ExprTermFamily Expr where  
  constructors = [Concr Min', Concr Val']
  
instance Type ExprTermFamily Int where  
  constructors = [Abstr Int']
  
term1, term2, term3, term4 :: Term
term1 = Parens (Min (Val (Number 2)) (Number 1))
term2 = Parens (Min (Val (Number 2)) (Number 2))
term3 = Parens (Min (Val (Number 1)) (Number 1))
term4 = Parens (Min (Val (Number 1)) (Number 1))

type ETDiff a = EditScript ExprTermFamily a a

d1_2, d2_3, d3_4, d4_1 :: ETDiff Term
d1_2 = diff term1 term2
d2_3 = diff term2 term3
d3_4 = diff term3 term4
d4_1 = diff term4 term1

-- Example shown in last part of short versioned gdiff paper. 

data Expr2 = One
           | Add Expr2 Term2
            deriving (Eq, Show)
                     
data Term2 = Neg Expr2
             deriving (Eq, Show)

data ExprTermFamily2 :: * -> * -> * where
  One' :: ExprTermFamily2 Expr2 Nil
  Add' :: ExprTermFamily2 Expr2 (Cons Expr2 (Cons Term2 Nil))
  Neg' :: ExprTermFamily2 Term2 (Cons Expr2 Nil)

instance Family ExprTermFamily2 where 
  decEq One' One' = Just (Refl, Refl)
  decEq Add' Add' = Just (Refl, Refl)
  decEq Neg' Neg' = Just (Refl, Refl)
  decEq _    _    = Nothing
  
  fields One' One = Just CNil
  fields Add' (Add e t) = Just (CCons e (CCons t CNil))
  fields Neg' (Neg e) = Just (CCons e CNil)
  fields _ _ = Nothing
  
  apply One' CNil = One
  apply Add' (CCons e (CCons t CNil)) = Add e t
  apply Neg' (CCons e CNil) = Neg e
  
  string One' = "One"
  string Add' = "Add"
  string Neg' = "Neg"
  
instance Type ExprTermFamily2 Expr2 where  
  constructors = [Concr One', Concr Add']
  
instance Type ExprTermFamily2 Term2 where
  constructors = [Concr Neg']
  
t21 = Add One (Neg One)  
t22 = Add One (Neg (Add One (Neg One)))

--
-- How can we compare lists structure?
-- 

data IL = ILN 
        | ILC Int IL
        deriving (Eq, Show)

data ILFamily :: * -> * -> * where 
  ILNil  :: ILFamily IL Nil
  ILCons :: ILFamily IL (Cons Int (Cons IL Nil))
  ILInt  :: Int -> ILFamily Int Nil 
  
instance Family ILFamily where
  decEq ILNil ILNil = Just (Refl,Refl)
  decEq ILCons ILCons = Just (Refl,Refl)
  decEq (ILInt i) (ILInt j)   | i == j = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq _ _ = Nothing

  fields ILNil ILN             = Just CNil
  fields ILCons (ILC i is) = Just (CCons i (CCons is CNil))
  fields (ILInt _) _           = Just CNil
  fields _ _                   = Nothing

  apply ILNil CNil = ILN
  apply ILCons (CCons i (CCons is CNil)) = ILC i is
  apply (ILInt i) CNil = i

  string ILNil = "ILN"
  string ILCons = "ILC"
  string (ILInt i) = show i
  
instance Type ILFamily IL where
  constructors = [Concr ILNil, Concr ILCons]
  
instance Type ILFamily Int where  
  constructors = [Abstr ILInt]
  
il1 = ILC 1 (ILC 2 (ILC 3 (ILC 4 (ILC 5 ILN))))  
il2 = ILC 1 (ILC 2 (ILC 33 (ILC 4 (ILC 5 ILN))))  

dil_1_2 :: EditScript ILFamily IL IL
dil_1_2 = diff il1 il2

-- 
-- How about comparing with [] and (:)?
--
-- Omit above IL, and use [], (:) instead of ILN, ILC.
-- Using Char as strict content type of list elements.
-- 

data CLFamily :: * -> * -> * where
  CLNil  :: CLFamily [Char] Nil
  CLCons :: CLFamily [Char] (Cons Char (Cons [Char] Nil))
  CLChar :: Char -> CLFamily Char Nil
  
instance Show (CLFamily a b) where  
  show CLNil = "[]"
  show CLCons = ":"
  show (CLChar c) = show c
  
instance Family CLFamily where
  decEq CLNil CLNil           = Just (Refl,Refl)
  decEq CLCons CLCons         = Just (Refl,Refl)
  decEq (CLChar a) (CLChar b) | a == b    = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq _ _                   = Nothing

  fields CLNil []      = Just CNil
  fields CLCons (x:xs) = Just (CCons x (CCons xs CNil))
  fields (CLChar _) _  = Just CNil
  fields _ _           = Nothing

  apply CLNil CNil                       = []
  apply CLCons (CCons x (CCons xs CNil)) = x:xs
  apply (CLChar c) CNil                  = c

  string = show

instance Type CLFamily [Char] where
  constructors = [Concr CLNil, Concr CLCons]
  
instance Type CLFamily Char where  
  constructors = [Abstr CLChar]
  
type CLDiff = EditScript CLFamily String String

cl1, cl2 :: [Char]
cl1 = "foo"  
cl2 = "fox"

dcl_1_2 :: CLDiff
dcl_1_2 = diff cl1 cl2

patchString :: CLDiff -> String -> String
patchString = undefined

--
-- How about SynthParam?
--
data SPFamily :: * -> * -> * where
  SPV      :: SPFamily SynthParam (Cons String (Cons Double Nil))
  SPC      :: SPFamily SynthParam (Cons String (Cons Int Nil))
  SPA      :: SPFamily SynthParam (Cons String (Cons Int Nil))
  SPName   :: String -> SPFamily String Nil
  SPDouble :: Double -> SPFamily Double Nil
  SPInt    :: Int -> SPFamily Int Nil

instance Show (SPFamily a b) where  
  show SPV = ":="
  show SPC = ":<-"
  show SPA = ":<="
  show (SPName n) = n
  show (SPDouble d) = show d
  show (SPInt i) = show i
  
instance Family SPFamily where  
  decEq SPV SPV = Just (Refl,Refl)
  decEq SPC SPC = Just (Refl,Refl)
  decEq SPA SPA = Just (Refl,Refl)
  decEq (SPName a) (SPName b) | a == b    = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq (SPInt a) (SPInt b)   | a == b    = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq _ _             = Nothing

  fields SPV (n:=v)     = Just (CCons n (CCons v CNil))
  fields SPC (n:<-v)    = Just (CCons n (CCons v CNil))
  fields SPA (n:<=v)    = Just (CCons n (CCons v CNil))
  fields (SPName _) _ = Just CNil
  fields (SPDouble _) _ = Just CNil
  fields (SPInt _) _    = Just CNil
  fields _ _            = Nothing

  apply SPV (CCons n (CCons v CNil)) = n := v
  apply SPC (CCons n (CCons v CNil)) = n :<- v
  apply SPA (CCons n (CCons v CNil)) = n :<= v
  apply (SPName s) CNil = s
  apply (SPDouble d) CNil = d
  apply (SPInt i) CNil = i

  string = show

instance Type SPFamily SynthParam where
  constructors = [Concr SPV, Concr SPC, Concr SPA]
  
instance Type SPFamily String where  
  constructors = [Abstr SPName]
  
instance Type SPFamily Int where  
  constructors = [Abstr SPInt]
  
instance Type SPFamily Double where  
  constructors = [Abstr SPDouble]
  
--
-- How about list of synthparams?
--

data SPSFamily :: * -> * -> * where
  SPNil  :: SPSFamily [SynthParam] Nil
  SPCons :: SPSFamily [SynthParam] (Cons SynthParam (Cons [SynthParam] Nil))
  SPSP   :: SynthParam -> SPSFamily SynthParam Nil
  
instance Show (SPSFamily a b) where
  show SPNil = "[]"
  show SPCons = ":"
  show (SPSP v) = show v
  
instance Family SPSFamily where
  decEq SPNil SPNil = Just (Refl,Refl)
  decEq SPCons SPCons = Just (Refl,Refl)
  decEq (SPSP a) (SPSP b) | a == b    = Just (Refl,Refl)
                          | otherwise = Nothing
  decEq _ _ = Nothing
  
  fields SPNil [] = Just CNil
  fields SPCons (x:xs) = Just (CCons x (CCons xs CNil))
  fields (SPSP _) _    = Just CNil
  fields _ _ = Nothing
  
  apply SPNil CNil = []
  apply SPCons (CCons x (CCons xs CNil)) = x:xs
  apply (SPSP v) CNil = v
  
  string = show

instance Type SPSFamily SynthParam where
  constructors = [Abstr SPSP]

instance Type SPSFamily [SynthParam] where
  constructors = [Concr SPNil, Concr SPCons]
  
{-

How about SCNode?  
 
Without comparing each element in SynthParam, time for diff t1 t2 was:

> real    0m0.432s
> user    0m0.417s
> sys     0m0.013s

While comparing each elements, time for diff t1 t2 was:

> real    0m2.885s
> user    0m2.826s
> sys     0m0.057s

It takes less time cause not seeking the detail of each SynthParams
names and values.

-} 
data SCNodeFamily :: * -> * -> * where  
  SCNSynth       :: 
    SCNodeFamily SCNode (Cons Int (Cons String (Cons [SynthParam] Nil)))
  SCNGroup       :: SCNodeFamily SCNode (Cons Int (Cons [SCNode] Nil))
  SCNNil         :: SCNodeFamily [SCNode] Nil
  SCNCons        :: SCNodeFamily [SCNode] (Cons SCNode (Cons [SCNode] Nil))
  SCNInt         :: Int -> SCNodeFamily Int Nil
  SCNString      :: String -> SCNodeFamily String Nil
  -- SCNPNil        :: SCNodeFamily [SynthParam] Nil
  -- SCNPCons       :: SCNodeFamily [SynthParam] (Cons SynthParam (Cons [SynthParam] Nil))
  -- SCNParam       :: SynthParam -> SCNodeFamily SynthParam Nil
  SCNParams      :: [SynthParam] -> SCNodeFamily [SynthParam] Nil
  
instance Show (SCNodeFamily a b) where  
  show SCNSynth = "Synth"
  show SCNGroup = "Group"
  show SCNNil = "[]"
  show SCNCons = ":"
  show (SCNInt i) = show i
  show (SCNString s) = s
  show (SCNParams ps) = show ps
  -- show SCNPNil = "[]"
  -- show SCNPCons = ":"
  -- show (SCNParam p) = show p
                                 
instance Family SCNodeFamily where 
  decEq SCNSynth SCNSynth = Just (Refl,Refl)
  decEq SCNGroup SCNGroup = Just (Refl,Refl)
  decEq SCNNil SCNNil = Just (Refl,Refl)
  decEq SCNCons SCNCons = Just (Refl,Refl)
  decEq (SCNInt !a) (SCNInt !b) | a == b = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq (SCNString !a) (SCNString !b) | a == b = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq (SCNParams !a) (SCNParams !b) | a == b = Just (Refl,Refl)
                                  | otherwise = Nothing
  -- decEq SCNPNil SCNPNil = Just (Refl,Refl)
  -- decEq SCNPCons SCNPCons = Just (Refl,Refl)
  -- decEq (SCNParam a) (SCNParam b) | a == b = Just (Refl,Refl)
                                  | otherwise = Nothing
  decEq _ _ = Nothing
  
  fields SCNSynth (Synth !i !n !ps) = Just (CCons i (CCons n (CCons ps CNil)))
  fields SCNGroup (Group !i !ns)   = Just (CCons i (CCons ns CNil))
  fields SCNNil []               = Just CNil
  fields SCNCons !(n:ns)          = Just (CCons n (CCons ns CNil))
  fields (SCNInt _) _            = Just CNil
  fields (SCNString _) _         = Just CNil
  fields (SCNParams _) _         = Just CNil
  -- fields SCNPNil []              = Just CNil
  -- fields SCNPCons (n:ns)         = Just (CCons n (CCons ns CNil))
  -- fields (SCNParam _) _         = Just CNil
  fields _ _                     = Nothing

  apply SCNSynth (CCons !i (CCons !n (CCons !ps CNil))) = Synth i n ps
  apply SCNGroup (CCons !i (CCons !ns CNil)) = Group i ns
  apply SCNNil CNil = []
  apply SCNCons (CCons !n (CCons !ns CNil)) = n:ns
  apply (SCNInt !i) CNil = i
  apply (SCNString !n) CNil = n
  apply (SCNParams !p) CNil = p
  -- apply SCNPNil CNil = []
  -- apply SCNPCons (CCons n (CCons ns CNil)) = n:ns
  -- apply (SCNParam p) CNil = p

  string = show

instance Type SCNodeFamily SCNode where
  constructors = [Concr SCNSynth, Concr SCNGroup]

instance Type SCNodeFamily [SCNode] where
  constructors = [Concr SCNNil, Concr SCNCons]
  
-- instance Type SCNodeFamily SynthParam where
--   constructors = [Abstr SCNParam] 
  
-- instance Type SCNodeFamily [SynthParam] where
--   constructors = [Concr SCNPNil, Concr SCNPCons] 
  
instance Type SCNodeFamily [SynthParam] where
  constructors = [Abstr SCNParams]
  
instance Type SCNodeFamily String where  
  constructors = [Abstr SCNString] 
  
instance Type SCNodeFamily Int where  
  constructors = [Abstr SCNInt] 

-- | Accumurate string representation of EditScript to list.
edits :: forall f txs tys . EditScriptL f txs tys -> [String] -> [String]
edits (Ins c d) acc   = ("Ins " ++ string c) : edits d acc
edits (Del c d) acc   = ("Del " ++ string c) : edits d acc
edits (Cpy c d) acc   = ("Cpy " ++ string c) : edits d acc
edits (CpyTree d) acc = "CpyTree" : edits d acc
edits _ acc           = acc

class Sized a where
  sizeOf :: a -> Int
  
instance Sized SCNode where
  sizeOf (Synth _ _ ps) = 1 + sizeOf ps
  sizeOf (Group _ ns) = 1 + sizeOf ns
  
instance Sized SynthParam where  
  sizeOf _ = 1
  
instance Sized a => Sized [a] where  
  sizeOf = sum . map sizeOf

-- data SCNodeF :: * -> * -> * where
--   Synth' :: Int -> String -> [SynthParam] -> SCNodeF SCNode Nil
--   Group' :: Int -> [SCNode] -> SCNodeF SCNode Nil 
--   SCNil  :: SCNodeF [SCNode] Nil
--   SCCons :: SCNodeF [SCNode] (Cons SCNode (Cons [SCNode] Nil))
  
-- instance Show (SCNodeF a b) where
--   show (Synth' i n ps) = "Synth" ++ concat [show i, " ", n, " ", show ps]
--   show (Group' i ns) = "Group" ++ concat [show i, " ", show ns]
--   show SCNil = "[]"
--   show SCCons = ":"
  
-- instance Family SCNodeF where
--   decEq (Synth' i1 n1 p1) (Synth' i2 n2 p2) 
--     | i1 == i2 && n1 == n2 && p1 == p2 = Just (Refl,Refl)
--     | otherwise = Nothing
--   decEq (Group' i1 n1) (Group' i2 n2)
--     | i1 == i2 && n1 == n2 = Just (Refl,Refl)
--     | otherwise = Nothing
--   decEq SCNil SCNil = Just (Refl,Refl)
--   decEq SCCons SCCons = Just (Refl,Refl)
--   decEq _ _ = Nothing
  
--   fields (Synth' _ _ _) _ = Just CNil
--   fields (Group' _ _) _   = Just CNil
--   fields SCNil [] = Just CNil
--   fields SCCons (n:ns) = Just (CCons n (CCons ns CNil))
--   fields _ _ = Nothing
  
--   apply (Synth' i n ps) CNil = Synth i n ps
--   apply (Group' i ns) CNil   = Group i ns
--   apply SCNil CNil = []
--   apply SCCons (CCons n (CCons ns CNil)) = n:ns
  
--   string = show
  
-- instance Type SCNodeF SCNode where  
--   constructors = [Abstr Synth', Abstr Group']
  
-- instance Type SCNodeF [SCNode] where  
--   constructors = [Concr SCNil, Concr SCCons]
  

-- instance Type SPFamily SynthParam where
--   constructors = [Abstr SPV, Abstr SPC, Abstr SPA]

--
-- Attempt to declare type families and data types for diffing Maybe.
-- Failed. Not understanding whether currently its possible or not to instantiate
-- data types which takes a constructor. Like in this Maybe case, is it possible
-- to use a constructor that takes an argument which its type is opened, like
-- 'Just'?
-- 

-- data MaybeFamily :: * -> * -> * -> * where
--   Just' :: Show a => Maybe a -> MaybeFamily a (Maybe a) (Cons a Nil)
--   Nothing' :: MaybeFamily a (Maybe a) Nil

-- instance (Show a, Eq a) => Family (MaybeFamily a) where
--   decEq (Just' a) (Just' b) | a == b = Just (Refl, Refl)
--                             | otherwise = Nothing
--   decEq Nothing' Nothing' = Just (Refl,Refl)
--   decEq _ _ = Nothing
  
--   fields (Just' _) (Just t) = Just (CCons t CNil)
--   fields Nothing' Nothing = Just CNil
--   fields _ _ = Nothing
  
--   apply (Just' _) (CCons a CNil) = Just a
--   apply Nothing' CNil = Nothing
  
--   string (Just' x) = "Just " ++ show x
--   string Nothing' = "Nothing"

-- instance (Show a, Eq a) => Type (MaybeFamily a) a where
--   constructors = undefined -- [Abstr Just, Concr Nothing]

-- instance (Show a, Eq a) => Type (MaybeFamily a) (Maybe a) where
--   constructors = [Abstr Just', Concr Nothing']
  
type SCNodeDiff = EditScript SCNodeFamily SCNode SCNode

diffSCNode :: SCNode -> SCNode -> SCNodeDiff
diffSCNode = {-# SCC "diffSCNode" #-} diff

main = do
  print $ diffSCNode t1 t2