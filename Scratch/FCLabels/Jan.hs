{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Playing with fclabels.
--
module Jan where

import Control.Arrow
import Data.Record.Label

data Person = Person 
  { _name :: String
  , _age :: Int  
  , _isMale :: Bool  
  , _place :: Place }
  deriving (Eq, Show)
  
data Place = Place { _city, _country, _continent :: String } 
           deriving (Eq, Show)

$(mkLabels [''Person, ''Place])

jan :: Person
jan = Person "jan" 71 True (Place "Utrecht" "The Netherlands" "Europe")

