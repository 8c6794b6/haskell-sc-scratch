{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Parsing patterns, take 7.
Rewriting pattern classes to take extra argument.
-}
module Scratch.S where

import Sound.SC3
import Scratch.PC01

newtype S h a = S {unS :: Int -> String}

toS :: S h a -> S h a
toS = id

view :: S h a -> String
view e = unS e 0

viewSs :: [S h a] -> Int -> String
viewSs ss n = case ss of
  [] -> "[]"; (t:ts) -> '[': unS t n ++ go ts
  where go us = case us of [] -> "]"; (v:vs) -> ',' : unS v n ++ go vs

instance Show (S h a) where show = view

instance Eq (S h a) where
  a == b = unS a 0 == unS b 0
