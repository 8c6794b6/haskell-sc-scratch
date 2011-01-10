------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Sound.SC3.Lepton.UGen.Factory where

import Data.List (isPrefixOf)

import Sound.SC3

-- | Wrapper for control and tr_control.
--
-- Pass to tr_control when name starts with \"tr_\", otherwise, control with kr.
ctrl :: String -> Double -> UGen
ctrl name val
  | "t_" `isPrefixOf` name = tr_control name val
  | otherwise              = control kr name val
