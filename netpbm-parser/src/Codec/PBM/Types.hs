------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- Description : Module defining types used for PNM parse.
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Codec.PBM.Types where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

data Greymap = Greymap
  { greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                           " " ++ show m

data ParseState = ParseState
  { string :: L.ByteString
  , offset :: Int64
  } deriving (Show)

