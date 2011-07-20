{-# LANGUAGE Arrows #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Playing with arrows.
--
module A1 where

import Control.Arrow
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

addA f g = proc x -> do
  y <- f -< x
  z <- g -< x
  returnA -< y + z

ug1 = proc f -> do
  f' <- flip (sinOsc kr) 0 -< f
  freq <- (+800) <<< (*800) -< f'
  out 0 <<< (*0.3) <<< flip (sinOsc ar) 0 -< freq

ug2 :: UGen -> UGen
ug2 f = out 0 (pan2 (limiter flt 1 0.25) pos 1) where
  flt  = ringz sig cf q
  -- flt  = rlpf sig 2200 q
  cf   = lfdNoise3 'g' kr 8e-1 * 1 + 880
  q    = lfdNoise3 'q' kr 3e-2 * 0.025 + 0.0250201
  sig  = mix $ sinOsc ar 0 (mce [frq, frq*1.5, frq*2]) * 0.3 * amp
  frq  = ffnd + fidx
  ffnd = lfdNoise3 'f' kr f * 3200 + 3200
  fidx = sinOsc kr 1423.32 0 * (lfdNoise3 'm' kr 0.125 * 8 + 7.132)
  amp  = envGen kr hit 1 0 1 DoNothing shp
  shp  = envPerc atk dcy
  atk  = lfdNoise3 'a' kr (1/37) * 3e-1 + 3e-1
  dcy  = lfdNoise3 'l' kr (1/35) * 3e-1 + 3e-1
  hit  = impulse kr 3 0 + dust 'd' kr 1.85
  pos  = lfdNoise3 'p' kr 0.251

-- foo = do
--   let s = sinOsc ar
--   0 => phase s
--   lfdNoise3 kr 8 => freq s
--   return $ out 0 s

-- a1 = \f -> sinOsc ar f 0 >>^ out 0
