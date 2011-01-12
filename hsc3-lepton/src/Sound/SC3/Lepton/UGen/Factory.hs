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
-- Pass to tr_control when name starts with \"t_\", otherwise, control with kr.
ctrl :: String -- ^ Control name
     -> Double -- ^ Default value
     -> UGen
ctrl name val
  | "t_" `isPrefixOf` name = tr_control name val
  | otherwise              = control kr name val

-- | Wrapper for making continuous KR control ugens.
--
-- Result is an MCE ugen with last element with specified name.
-- The range of controls could be set by n_setn message.
--
ctrls :: String   -- ^ Control name
      -> [Double] -- ^ Default values
      -> UGen
ctrls name vals = mce . snd $ sigs
  where
    sigs = foldr f (1,[ctrl name $ head vals]) (tail vals)
    f :: Double -> (Int, [UGen]) -> (Int, [UGen])
    f a (i,us) = (i+1, ctrl (name ++ "_" ++ show i) a : us)
