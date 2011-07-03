------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Common codes for fountain and commet.
--
module GLTut.Particle where

import Control.Monad
import Data.IORef
import System.Random

import Graphics.UI.GLUT hiding (position)

type Point = Vertex3 GLdouble

data Particle = Particle {position :: Point, velocity :: Point}

data Renderer s = Renderer
  { rendererFunc :: DisplayCallback
  , state :: IORef s }

timerInterval :: Timeout
timerInterval = 16

p0 :: Point
p0 = Vertex3 0 0 0

addVer3 :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
addVer3 (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3 (x1+x2) (y1+y2) (z1+z2)

subVer3 :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
subVer3 (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3 (x1-x2) (y1-y2) (z1-z2)

getY :: Vertex3 a -> a
getY (Vertex3 _ y _) = y

newParticle :: Point -> Point -> IO Particle
newParticle p v = (Particle p . addVer3 v) `fmap` getRandomPoint

newParticles :: Int -> Point -> Point -> IO [Particle]
newParticles n p v = replicateM n (newParticle p v)

getRandomPoint :: IO Point
getRandomPoint = do
  g0 <- newStdGen
  let (x,g1) = randomR (-3e-3, 3e-3) g0
      (y,g2) = randomR (-3e-3, 3e-3) g1
      (z,_)  = randomR (-3e-3, 3e-3) g2
  return $ Vertex3 x y z

searchMap :: (Eq a, Num b) => a -> [(a, Vertex3 b)] -> Vertex3 b
searchMap f ((ch,v):xs) = if f == ch then v else searchMap f xs
searchMap _ [] = Vertex3 0 0 0
