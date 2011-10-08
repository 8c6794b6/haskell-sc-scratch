{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Type representations used in deserializing tagless syntax, take 1.

-}
module Type01 where

import Text.PrettyPrint

------------------------------------------------------------------------------
-- Syntax tree

data Tree
  = Leaf String
  | Node String [Tree]
  deriving (Eq,Show)

ppTree :: Tree -> Doc
ppTree t = case t of
  Leaf s    -> text s
  Node s ts -> text s <+> (vcat $ map (parens . ppTree) ts)

safeRead :: Read b => String -> Either String b
safeRead s = case reads s of
  [(x,"")] -> Right x
  _        -> Left $ "Failed reading: " ++ s

------------------------------------------------------------------------------
-- Type representations

data Ty t where
  Ty :: a -> Ty a
  TyAny :: Ty a
  TyInt :: Ty Int
  TyArr :: Ty a -> Ty b -> Ty (a->b)

instance Show (Ty a) where
  show t = case t of
    Ty _      -> "type of something"
    TyAny     -> "any"
    TyInt     -> "Int"
    TyArr a b -> concat ["(",show a," -> ",show b,")"]

toTy :: Ty a -> Ty a
toTy = id

data Equal a b where
  Equal :: Equal c c

instance Show (Equal a b) where
  show _ = "Equal"

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy ta tb = case (ta,tb) of
  (TyInt,TyInt)             -> Just Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal
  _                         -> Nothing

data ExtTy where
  ExtTy :: Ty ty -> ExtTy

instance Show ExtTy where
  show (ExtTy ty) = "ExtTy"
