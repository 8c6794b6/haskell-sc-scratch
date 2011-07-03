------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Showing alpha blended particles.
--
module GLTut.AlphaBlend where

import Graphics.UI.GLUT hiding (position)

import System.Exit
import System.Random
import Data.IORef
import Control.Monad
import qualified Data.Set as S

import GLTut.Particle hiding (Particle(..), newParticle, newParticles)

data AParticle = AParticle
  { position :: Point
  , velocity :: Point
  , temperature :: Float }

data AState = AState
  { center :: AParticle
  , objects :: [AParticle]
  , keys :: S.Set Key }

updateObject :: AParticle -> AParticle
updateObject (AParticle pos vel tem) = AParticle (addVer3 pos vel) vel (tem-dt)
  where dt = 0.1875

updateObjects :: [AParticle] -> [AParticle]
updateObjects = foldr f [] where
  f o os = if temperature o > 0 then updateObject o:os else os

tempAve :: Float
tempAve = 50

newParticle :: Point -> Point -> IO AParticle
newParticle p v = do
  vel <- randomVel
  tem <- randomTem
  return $ AParticle p (addVer3 v vel) tem
  where
    randomVel :: IO Point
    randomVel = do
      g0 <- newStdGen
      let (x,g1) = randomR (-6e-3,6e-3) g0
          (y,g2) = randomR (-6e-3,6e-3) g1
          (z,_)  = randomR (-6e-3,6e-3) g2
      return $ Vertex3 x y z

    randomTem :: IO Float
    randomTem = getStdRandom (randomR (tempAve-10, tempAve+10))

newParticles :: Int -> Point -> Point -> IO [AParticle]
newParticles n p v = replicateM n (newParticle p v)

main :: IO ()
main = do
  as <- newIORef $ AState (AParticle p0 p0 0) [] S.empty
  rend <- newIORef Renderer
    { rendererFunc = display as
    , state = as }

  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  createWindow "AlphaBlend"

  displayCallback $= display as
  reshapeCallback $= Just (reshape as)
  keyboardMouseCallback $= Just (keyboardProc as)
  addTimerCallback timerInterval $ timerProc rend

  mainLoop

display :: IORef AState -> IO ()
display ref = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcColor)
  loadIdentity
  st <- readIORef ref
  setCenter st
  pointSize $= 4
  showParticles $ objects st
  showCenter $ center st
  swapBuffers
  where
    tempToColor4 t a = Color4
      (1-0.04*(tempAve-t)) (1-0.08*(tempAve-t)) (1-0.02*(tempAve-t)) a
    getSprite w =
      [Vertex3 (-w) (-w) 0
      ,Vertex3 (-w)   w  0
      ,Vertex3   w  (-w) 0
      ,Vertex3   w  (-w) 0
      ,Vertex3 (-w)   w  0
      ,Vertex3   w    w  0]

    showParticles = mapM_ $ \o -> do
      color (tempToColor4 (temperature o) 0.8)
      preservingMatrix $ renderPrimitive Triangles $ do
        mapM_ vertex $ map (addVer3 $ position o) $ getSprite 0.04

    showCenter (AParticle (Vertex3 x y z) _ _) = do
      color $ Color3 (1::Double) 1 1
      preservingMatrix $ do
        translate (Vector3 x y z)
        renderObject Wireframe (Cube 0.06)

modifyAState :: IORef AState -> IO ()
modifyAState ref = do
  st <- readIORef ref
  let ks = keys st
      pos = position $ center st
      vel = velocity $ center st
      addVecList = map (`searchMap` vectorMap) $ S.elems ks
      addVec = foldl addVer3 p0 addVecList
      Vertex3 x y z = addVec
  ps <- if or [null addVecList, addVec == p0] then
          return []
        else
          newParticles 8 (addVer3 pos vel) (addVer3 vel (Vertex3 (-x) (-y) (-z)))
  let newCenter = AParticle
        { position = addVer3 pos vel
        , velocity = addVer3 vel addVec
        , temperature = temperature $ center st }
  writeIORef ref $ AState
    { center = newCenter
    , objects = ps ++ (updateObjects $ objects st)
    , keys = ks }
  where
    vectorMap =
      [(Char 'h', Vertex3 (-3e-3) 0 0)
      ,(Char 'j', Vertex3 0 (-3e-3) 0)
      ,(Char 'k', Vertex3 0 3e-3 0)
      ,(Char 'l', Vertex3 3e-3 0 0)
      ,(Char 'n', Vertex3 0 0 3e-3)
      ,(Char 'p', Vertex3 0 0 (-3e-3))]

timerProc :: IORef (Renderer AState) -> IO ()
timerProc ref = do
  rend <- readIORef ref
  modifyAState $ state rend
  rendererFunc rend
  addTimerCallback timerInterval $ timerProc ref

setCenter :: AState -> IO ()
setCenter st = do
  let Vertex3 x y _ = position $ center st
  lookAt (Vertex3 x y 1) (Vertex3 x y 0) (Vector3 0 1 0)

reshape :: IORef AState -> Size -> IO ()
reshape ref size@(Size w h) = do
  viewport $= (Position 0 0, size)
  st <- readIORef ref
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 0.001 50
  setCenter st
  matrixMode $= Modelview 0

keyboardProc :: IORef AState -> Key -> KeyState -> a -> b -> IO ()
keyboardProc ref ch ks _ _
  | ch == Char 'q' = exitWith ExitSuccess
  | ks == Down     = modifyIORef ref $ \s -> s {keys = S.insert ch $ keys s}
  | ks == Up       = modifyIORef ref $ \s -> s {keys = S.delete ch $ keys s}
  | otherwise      = return ()
