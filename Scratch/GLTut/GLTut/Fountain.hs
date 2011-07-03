------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Shows random particles.
--
module GLTut.Fountain where

import Control.Monad
import Data.IORef
import System.Exit
import System.Random

import Graphics.UI.GLUT hiding (position)

import GLTut.Particle hiding (newParticle, newParticles)

main :: IO ()
main = do
  p <- newParticle
  pState <- newIORef FState {objects=[p]}
  rend <- newIORef $ Renderer (display pState) pState

  (_,_) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  createWindow "Fountain"

  displayCallback $= display pState
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardProc
  addTimerCallback timerInterval $ timerProc rend

  mainLoop

data FState = FState {objects :: [Particle]}

updateParticle :: Particle -> Particle
updateParticle (Particle pos vel) = Particle pos' vel' where
  pos' = addVer3 pos vel
  vel' = addVer3 vel (Vertex3 0 (-1e-3) 0)

updateParticles :: [Particle] -> [Particle]
updateParticles ps =
  [q | p <- ps , let q = updateParticle p, getY (position q) > (-0.5)]

newParticle :: IO Particle
newParticle = Particle (Vertex3 0 0 0) `fmap` getRandomVel

getRandomVel :: IO Point
getRandomVel = do
  g0 <- newStdGen
  let (x,g1) = randomR (0.01, 0.02) g0
      (y,g2) = randomR (0.01, 0.03) g1
      -- (z,_) = (0,g2)
      -- (z,_) = randomR (-0.02, 0.02) g2
      (z,_) = randomR (-0.01, 0.01) g2
  return $ Vertex3 x y z

newParticles :: Int -> IO [Particle]
newParticles = flip replicateM newParticle

display :: IORef FState -> IO ()
display pState = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  ps <- readIORef pState
  pointSize $= 2
  preservingMatrix $ do
    renderPrimitive Points $ mapM_ (vertex . position) (objects ps)
  swapBuffers

timerProc :: IORef (Renderer FState) -> IO ()
timerProc ref = do
  gr <- readIORef ref
  gs <- readIORef $ state gr
  rendererFunc gr
  ps <- newParticles 32
  writeIORef (state gr) (FState {objects=ps++(updateParticles$objects gs)})
  addTimerCallback timerInterval $ timerProc ref

reshape :: Size -> IO ()
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 1e-3 50
  lookAt (Vertex3 0.5 0.2 1) (Vertex3 0.5 0.2 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0

keyboardProc :: Key -> a -> b -> c -> IO ()
keyboardProc ch _ _ _
  | ch == Char 'q' = exitWith ExitSuccess
  | otherwise      = return ()