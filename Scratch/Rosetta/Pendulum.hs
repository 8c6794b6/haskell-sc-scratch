------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- * <http://rosettacode.org/wiki/Animate_a_pendulum>
--
module Main where

import Control.Arrow ((&&&))
import Control.Exception (bracket)
import Control.Monad (forM_)

import Graphics.HGL.Draw.Monad (Graphic)
import Graphics.HGL.Draw.Picture
import Graphics.HGL.Utils
import Graphics.HGL.Window
import Graphics.HGL.Run

main = pendulum

pendulum = runGraphics $ bracket
  (openWindowEx "Pendulum animation task" Nothing
   (600,400) DoubleBuffered (Just 30))
  closeWindow
  (\w -> forM_ pts $
    (\g -> setGraphic w g >> getWindowTick w) .
    (\(x,y) ->
      overGraphic (line (300,0) (x,y))
      (ellipse (x-12, y+12) (x+12, y-12))))
  where
    dt = 1/30
    t = - pi/4
    l = 1
    g = 9.812
    nextAVT (a,v,t) = (a',v',t+v'*dt)
      where
        a' = - (g/l) * sin t
        v' = v + a' * dt
    pts = map (\(_,t,_) ->
                (toInt . (300+) . (300*) . cos &&&
                 toInt . (300*) . sin) (pi/2+0.6*t)) $
          iterate nextAVT (- (g/l) * sin t, t, 0)

toInt :: Double -> Int
toInt = fromIntegral . round