{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Implementation of Pattern concepts DSL inspired by oleg's ttf.
--
module P6 where

import System.Random

newtype P a = P {unP :: StdGen -> [a]}

newtype S a = S {unS :: [String] -> String}

evalP :: P a -> StdGen -> [a]
evalP p g = unP p g

evalPIO :: P a -> IO [a]
evalPIO p = unP p `fmap` newStdGen

class Pval p where
  pval :: a -> p a

instance Pval P where
  pval a = P $ \_ -> [a]

class Pempty p where
  pempty :: p a

instance Pempty P where
  pempty = P $ \_ -> []

class Plist p where
  plist :: [a] -> p a

instance Plist P where
  plist a = P $ \_ -> a

class Pseq p where
  pseq :: p Int -> [p a] -> p a

instance Pseq P where
  pseq n ps = P $ \g ->
    concatMap (\p -> unP p g) $
    concatMap (\n' -> concat $ replicate n' ps) (unP n g)

class Prand p where
  prand :: p Int -> [p a] -> p a

instance Prand P where
  prand n ps = P $ \g ->
    let is = take (sum $ unP n g) (randomRs (0, length ps - 1) g)
    in  concatMap (\j -> unP (ps!!j) g) is

p1 = pseq (pval 3) [pval "foo"]
p2 = prand (plist [1,2,3]) [plist [5,4..1], pval 8]

pspe = pseq (pval maxBound)
       [prand (pval 1) [pempty, plist [24,31,36,43,48,55]]
        
       ,prand (prand (pval 1) [pval 2, pval 3, pval 4, pval 5])
        [pseq (pval 1)
         [pval 60
         ,prand (pval 1) [pval 63, pval 65]
         ,pval 67
         ,prand (pval 1) [pval 70, pval 72, pval 74]]]
        
       ,prand 
        (prand (pval 1)
         [pval 3, pval 4, pval 5, pval 6, pval 7, pval 8, pval 9])
        [pval 74, pval 75, pval 77, pval 79, pval 81]]
