{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Using lam, app, z and s with pattern, take 2.
-}
module Pat02 where

import System.Random
import System.Random.Shuffle

class Pprim p where
  pval    :: a -> p a
  plist   :: [a] -> p a
  pempty  :: p a

class Pinfinite p where
  prepeat  :: a -> p a
  pforever :: p a -> p a
  pcycle   :: [p a] -> p a

class Plist p where
  pappend :: p a -> p a -> p a
  pconcat :: [p a] -> p a
  preplicate :: p Int -> p a -> p a
  pseq :: p Int -> [p a] -> p a

class Prandom p where
  prange   :: Random a => p a -> p a -> p a
  prand    :: p Int -> [p a] -> p a
  pshuffle :: [p a] -> p a

class Pzip p where
  pzip :: p a -> p b -> p (a,b)
  pfst :: p (a,b) -> p a
  psnd :: p (a,b) -> p b

class Plc p where
  pz   :: p (a,h) a
  ps   :: p h a -> p (any,h) a
  plam :: p (a,h) b -> p h (a->[b])
  papp :: p h (a->[b]) -> p h a -> p h b

newtype L h a = L {unL :: h -> StdGen -> [a]}

runLIO :: L () a -> IO [a]
runLIO l = return . unL l () =<< newStdGen

const2 :: a -> b -> c -> a
const2 a _ _ = a

gens :: StdGen -> [StdGen]
gens = iterate (snd . next)

instance Pprim (L h) where
  pval x = L $ const2 [x]
  plist xs = L $ const2 xs
  pempty = L $ const2 []

instance Plist (L h) where
  pappend a b = L $ \h g -> let g' = snd (next g) in unL a h g ++ unL b h g'
  pconcat = foldr1 pappend
  preplicate a b = L $ \h g ->
    let p' = concatMap (`replicate` b) (unL a h g)
    in  concat $ zipWith (\q g' -> unL q h g') p' (gens g)
  pseq p = preplicate p . pconcat

instance Pinfinite (L h) where
  prepeat x = L $ const2 (repeat x)
  pforever p = L $ \h g -> concatMap (\g' -> unL p h g') (gens g)
  pcycle ps | null ps   = pempty
            | otherwise = pforever (pconcat ps)
