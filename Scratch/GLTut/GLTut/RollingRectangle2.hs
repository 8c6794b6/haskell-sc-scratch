------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Shows rolling rectangle. Pressing any key except 'q' will alternate rolling
-- direction.
--
module GLTut.RollingRectangle2 where

import System.Exit
import Data.IORef

import Graphics.UI.GLUT

import GLTut.RollingCommon

main :: IO ()
main = do
  rot <- newIORef 0
  arg <- newIORef 14.4
  withGLUT "RollingRectangle2" $ do
    displayCallback $= display rot arg
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardProc arg)
    addTimerCallback timeInterval $ timerProc (display rot arg)

display :: IORef GLdouble -> IORef GLdouble -> IO ()
display rot arg = do
  w <- readIORef arg
  modifyIORef rot (+w)
  r <- readIORef rot
  displayRollingRect r
  swapBuffers

keyboardProc :: (Num a) => IORef a -> Key -> KeyState -> b -> c -> IO ()
keyboardProc arg ch state _ _
  | ch == Char 'q' = exitWith ExitSuccess
  | state == Down  = modifyIORef arg (*(-1))
  | otherwise      = return ()