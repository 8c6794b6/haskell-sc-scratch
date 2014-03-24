{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

Module containing boolean related pattern scratches.

-}
module Scratch.Pattern.If where

import Sound.SC3.Lepton

import System.Random.MWC

class Pbool p where
  pbool :: Bool -> p h Bool
  pif   :: p h Bool -> p h a -> p h a -> p h a
  pileq :: p h Int -> p h Int -> p h Bool
  pdleq :: p h Double -> p h Double -> p h Bool
  
instance Pbool S where
  pbool b = S $ \_ -> show b
  pif pp pt pf = S $ \h -> 
    concat ["if (",unS pp h,") then (",unS pt h,") else (",unS pf h,")"]
  pileq a b = S $ \h -> "pileq " ++ unS a h ++ " " ++ unS b h
  pdleq a b = S $ \h -> "pdleq " ++ unS a h ++ " " ++ unS b h
  
instance Pbool L where  
  pbool b = L $ \_ _ -> return [b]
  pif pp pt pf = 