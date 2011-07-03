------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Common codes used in RollingRectangle and RollingRectangle2.
--
module GLTut.RollingCommon where

import Graphics.UI.GLUT

timeInterval :: Timeout
timeInterval = 40

timerProc :: IO a -> IO ()
timerProc act = do
  act
  addTimerCallback timeInterval $ timerProc act

withGLUT :: String -> IO a -> IO ()
withGLUT windowName callbacks = do
  (_,_) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  createWindow windowName
  callbacks
  mainLoop

displayRollingRect :: GLdouble -> IO ()
displayRollingRect r = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix $ do
    rotate r (Vector3 0 0 1 :: Vector3 GLdouble)
    renderPrimitive Quads $ mapM_ vertex [
      Vertex3 0.1 0.1 0,
      Vertex3 (-0.1) 0.1 0,
      Vertex3 (-0.1) (-0.1) 0,
      Vertex3 0.1 (-0.1) 0 :: Vertex3 GLfloat]

reshape :: Size -> IO ()
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 0.001 50
  lookAt (Vertex3 0 0 (-1.0)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
