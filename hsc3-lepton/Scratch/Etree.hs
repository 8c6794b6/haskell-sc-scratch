{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Expression syntax tree used for serialization/deserialization.

-}
module Scratch.Etree where

import Data.Data
import Data.ByteString.Lazy (ByteString)
import Control.Applicative hiding (empty)
import Text.PrettyPrint

import Sound.SC3 (AddAction)
import Sound.SC3.Lepton.Pattern.ToOSC ()

import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as LC8

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

ppTree :: Etree -> Doc
ppTree e = case e of
  Node "pdouble" [Leaf n] ->
    text "Node" <+>
    (doubleQuotes $ text "pdouble") <+>
    brackets
    (text "Leaf" <+> double (Bin.decode n))
  Node "pint" [Leaf n] ->
    text "Node" <+>
    (doubleQuotes $ text "pint") <+>
    brackets
    (text "Leaf" <+> int (Bin.decode n))
  Node "psnew" (Leaf def:Leaf nid:Leaf aa:Leaf tid:ps) ->
    text "psnew" <+>
    text (Bin.decode def :: String) <+>
    text (show (Bin.decode nid :: Maybe Int)) <+>
    text (show (Bin.decode aa :: AddAction)) <+>
    int (Bin.decode tid :: Int) $$
    (nest 2 $ sep $ unParam ps)
  Node n es ->
    text "Node" <+>
    (doubleQuotes $ text (LC8.unpack n)) $+$
    (nest 2 $ brackets (sep (punctuate comma $ map ppTree es)))
  Leaf x ->
    text "Leaf" <+> int (Bin.decode x :: Int)
  where
    unParam es = case es of
      []            -> []
      (Leaf k:e:ps) ->
        parens (text (Bin.decode k) <> comma $$ ppTree e) : unParam ps
