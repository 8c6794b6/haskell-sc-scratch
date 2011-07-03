------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Shows moving points.
--
module GLTut.Commet where

import Data.IORef
import System.Exit

import Graphics.UI.GLUT hiding (position)

import GLTut.Particle

import qualified Data.Set as S

main :: IO ()
main = do
  cst <- newIORef CState
    {player = Particle {position = Vertex3 0 0 0, velocity = Vertex3 0 0 0}
    ,commets = []
    ,keys = S.empty}
  rend <- newIORef Renderer {rendererFunc = display cst, state = cst}

  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  createWindow "Commet"

  displayCallback $= display cst
  reshapeCallback $= Just (reshape cst)
  keyboardMouseCallback $= Just (keyboardProc cst)
  addTimerCallback timerInterval $ timerProc rend

  mainLoop

data CState = CState
  { player :: Particle
  , commets :: [Particle]
  , keys :: S.Set Key }

updateObject :: Particle -> Particle
updateObject (Particle pos vel) = Particle pos' vel' where
  pos' = addVer3 pos vel
  -- vel' = vel
  vel' = addVer3 vel (Vertex3 0 (-1e-4) 0)

updateCommets :: [Particle] -> [Particle]
updateCommets = map updateCommet

updateCommet :: Particle -> Particle
updateCommet (Particle pos vel) = Particle pos' vel' where
  pos' = addVer3 pos vel
  vel' = addVer3 vel (Vertex3 0 (-1e-4) 0)

display :: IORef CState -> IO ()
display ref = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  st <- readIORef ref
  setCenter st
  pointSize $= 4
  showCommets $ commets st
  showCenter $ player st
  swapBuffers
  where
    showCommets cs = preservingMatrix $
      renderPrimitive Points $ mapM_ vertex $ map position cs
    showCenter (Particle (Vertex3 x y z) _) = preservingMatrix $ do
      translate (Vector3 x y z)
      renderObject Wireframe (Sphere' 0.05 5 5)

modifyCommet :: IORef CState -> IO ()
modifyCommet ref = do
  st <- readIORef ref
  let ppos = position $ player st
      pvel = velocity $ player st
      addVecList = map (`searchMap` vectorMap) $ S.elems (keys st)
      addVec = foldl addVer3 (Vertex3 0 0 0) addVecList
      Vertex3 x y z = addVec
      newCenter = Particle (addVer3 ppos pvel) (addVer3 pvel addVec)
  ps <- if or [null addVecList, addVec == Vertex3 0 0 0] then
          return []
        else
          newParticles 32 (addVer3 ppos pvel)
          (addVer3 pvel (Vertex3 (-x) (-y) (-z)))
  writeIORef ref $ CState newCenter
    (take 10240 $ ps++(updateCommets $ commets st))
    (keys st)
  where
    vectorMap =
      [(Char 'h', Vertex3 (-3e-3) 0 0)
      ,(Char 'j', Vertex3 0 (-3e-3) 0)
      ,(Char 'k', Vertex3 0 3e-3 0)
      ,(Char 'l', Vertex3 3e-3 0 0)
      ,(Char 'n', Vertex3 0 0 3e-3)
      ,(Char 'p', Vertex3 0 0 (-3e-3))]

timerProc :: IORef (Renderer CState) -> IO ()
timerProc ref = do
  gr <- readIORef ref
  modifyCommet $ state gr
  rendererFunc gr
  addTimerCallback timerInterval $ timerProc ref

setCenter :: CState -> IO ()
setCenter gs = do
  let Vertex3 x y _ = position $ player gs
      k = 1.1
  lookAt (Vertex3 (x*k) (y*k) 1) (Vertex3 (x*k) (y*k) 0) (Vector3 0 1 0)

reshape :: IORef CState -> Size -> IO ()
reshape ref size@(Size w h) = do
  viewport $= (Position 0 0, size)
  st <- readIORef ref
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 1e-3 50
  setCenter st
  matrixMode $= Modelview 0

keyboardProc :: IORef CState -> Key -> KeyState -> a -> b -> IO ()
keyboardProc ref ch st _ _
  | ch == Char 'q' = exitWith ExitSuccess
  | st == Down     = modifyIORef ref $ \s -> s {keys = S.insert ch $ keys s}
  | st == Up       = modifyIORef ref $ \s -> s {keys = S.delete ch $ keys s}
  | otherwise      = return ()
