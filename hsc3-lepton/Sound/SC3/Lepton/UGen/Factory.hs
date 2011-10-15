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

import Data.Generics.Uniplate.Data
import Sound.SC3

import Sound.SC3.Lepton.Instance ()

-- | Makes different control ugen depending on its name prefix.
--
-- Rules are from SynthDef class in sclang:
--
-- * \"a_\" would be AR rate control ugen
--
-- * \"i_\" would be IR rate control ugen
--
-- * \"t_\" would be KR rate control ugen with trigger flag on.
--
-- * Other wise, KR rate control ugen without trigger.
--
ctrl :: String -- ^ Control name
     -> Double -- ^ Default value
     -> UGen
ctrl name val
  | "a_" `isPrefixOf` name = control AR name val
  | "i_" `isPrefixOf` name = control IR name val
  | "t_" `isPrefixOf` name = tr_control name val
  | otherwise              = control KR name val

-- | Infix variant of "ctrl".
(@@) :: String -> Double -> UGen
(@@) = ctrl

-- | Infix variant of "lag".
(@~) :: UGen -> UGen -> UGen
(@~) = lag

-- | For backword compatibility, synonym for \@\@.
(=:) :: String -> Double -> UGen
(=:) = ctrl
{-# DEPRECATED (=:) "Use @@ instead" #-}

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

-- | Set control value of given UGen.
setc ::
  String    -- ^ Name of control parameter
  -> Double -- ^ New value
  -> UGen   -- ^ Target ugen
  -> UGen
setc name value = transform f where
  f (Control r name' _ t) | name == name' = Control r name value t
  f x = x

-- | Set control values of given UGen
setcs ::
  [(String,Double)] -- ^ List of params to updat
  -> UGen           -- ^ Target ugen
  -> UGen
setcs kvs ug = foldr (\(k,v) acc -> setc k v acc) ug kvs
