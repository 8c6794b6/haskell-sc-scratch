------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Shows rolling rectangle. No user interaction is implemented.
--
module GLTut.RollingRectangle where

import Data.IORef
import Graphics.UI.GLUT

import GLTut.RollingCommon

main :: IO ()
main = do
  rot <- newIORef 0
  withGLUT "RollingRectangle" $ do
    displayCallback $= display rot
    reshapeCallback $= Just reshape
    addTimerCallback timeInterval $ timerProc (display rot)

display :: IORef GLdouble -> IO ()
display rot = do
  modifyIORef rot (+14.4)
  r <- readIORef rot
  displayRollingRect r
  swapBuffers
