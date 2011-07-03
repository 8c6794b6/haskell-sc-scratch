------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Drawing letters.
--
module GLTut.RenderString where

import System.Exit

import Graphics.UI.GLUT

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  createWindow "RenderString"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardProc
  mainLoop

display :: IO ()
display = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  lineWidth $= 4.0
  preservingMatrix $ do
    scale (1e-3::Double) 1e-3 1e-3
    w <- stringWidth Roman "Stroke font"
    translate (Vector3 (-0.5*(fromIntegral w)) 0 0 :: Vector3 Float)
    renderString Roman "Stroke font"
  swapBuffers

reshape :: Size -> IO ()
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 1e-3 50
  lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0

keyboardProc :: Key -> a -> b -> c -> IO ()
keyboardProc ch _ _ _
  | ch == Char 'q' = exitWith ExitSuccess
  | otherwise      = return ()
