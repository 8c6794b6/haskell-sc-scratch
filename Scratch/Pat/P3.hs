{-# LANGUAGE ExistentialQuantification #-}
------------------------------------------------------------------------------
-- | Another attempt to implement patterns, take 3.

module P3 where

import Control.Applicative
import Data.List (transpose)
import System.Random

class Pattern p where
    runP :: (RandomGen g) => p a -> g -> ([a], g)
    runP' :: (RandomGen g) => p a -> g -> [a]
    runP' p g = fst $ runP p g

data Pat a = Pempty
           | Pid a
           | Pseq (Pat Int) [Pat a]
           | Prand (Pat Int) [Pat a]
           | Pinf a
             deriving (Eq, Read, Show)

instance Pattern Pat where
    runP (Pid a) g = ([a], g)
    runP (Pseq n ps) g = (a, g')
        where a = concatMap (\n -> runPseq n g ps) (runP' n g)
              g' = snd $ next g
    runP (Prand n ps) g = (a, h) 
        where a = concatMap (\n -> fst $ runPrand g n ps) (runP' n g)
              h = snd $ next g
    runP (Pinf a) g = (repeat a, g)

newtype Pbind k v = Pbind [(k, v)]
               deriving (Eq, Read, Show)

type P k a = Pat (Pbind k (Pat a))

runPseq :: (RandomGen g, Pattern p)=> Int -> g -> [p a] -> [a]
runPseq n g ps = f (concat $ replicate n ps)
    where f = fst . foldr f' z 
          f' a (b,g) = (a'++b, g') 
              where (a',g') = runP a g
          z = ([],g)

runPrand :: (RandomGen g, Pattern p) => g -> Int -> [p a] -> ([a], g)
runPrand g num ps = foldr f' ([], g) (replicate num ps) 
    where f' a (b,g') = (runP' a' g' ++ b, g'') 
              where (a', g'') = chooseOne a g'

chooseOne :: RandomGen g => [a] -> g -> (a, g)
chooseOne xs g = (xs !! idx ,g')
    where (idx, g') = randomR (0, length xs - 1) g

-- newtype Pbind a = Pbind [(String, a)]
--     deriving (Eq, Show, Read)

runPb :: (a -> [b]) -> Pbind k a -> [[(k, b)]]
runPb f (Pbind ps) = transpose $ map f' ps 
    where f' (n, p) = zipWith (,) (repeat n) (f p)

runPbind :: (Pattern p, RandomGen g) => Pbind k (p a) -> g -> [[(k, a)]]
runPbind p g = runPb (flip runP' g) p

p :: P k a -> [[(k, a)]]
p = undefined

p1 :: Pat Double
p1 = Pid 1

p2 :: Pat Double
p2 = Pseq (Pid 1) [Pid 1, Pid 2, Pid 3]

p3 :: Pat Double
p3 = Prand (Pid 5) $ map Pid [1..100]