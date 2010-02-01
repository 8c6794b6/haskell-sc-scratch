{-# LANGUAGE ExistentialQuantification #-}
------------------------------------------------------------------------------
-- | Another attempt to implement patterns, take 2.
--
-- This time,

module P2 where

import Data.Monoid
import Control.Applicative
import System.Random

import P
    (Pbind(..),
     runPb)

class Pat p where
    runP :: (RandomGen g) => p a -> g -> [a]

-- data Pattern a = forall p. Pat p => Pattern (p a)
data Pattern a = forall p. (Functor p, Pat p) => Pattern (p a)

instance Pat Pattern where
    runP (Pattern p) g = runP p g
instance Functor Pattern where
    fmap f (Pattern p) = Pattern (fmap f p)


newtype Pid a = Pid a deriving (Eq, Read, Show)

instance Pat Pid where
    runP (Pid a) _ = [a]
instance Functor Pid where
    fmap f (Pid a) = Pid (f a)


data Pempty a = Pempty

instance Pat Pempty where
    runP _ _ = []
instance Functor Pempty where
    fmap f Pempty = Pempty


newtype Pinf a = Pinf a

instance Pat Pinf where
    runP (Pinf a) _ = repeat a
instance Functor Pinf where
    fmap f (Pinf a) = Pinf (f a)


data Pseq a = Pseq (Pattern Int) [Pattern a]

instance Pat Pseq where
    runP (Pseq p ps) g =
        concatMap (\n' -> concat $ replicate n' (concatMap (flip runP g) ps))
                      (runP p g)
instance Functor Pseq where
    fmap f (Pseq p ps) = Pseq p (map (fmap f) ps)

-- instance Functor Pattern where
--     fmap f (Pattern p) = Pattern (fmap f p)
-- 
--
-- XXX: Show?
--
-- instance (Show p) => Show (Pseq p) where
--     show (Pseq p ps) = "Pseq " ++ show p ++ " " ++ show ps

data Prand a = Prand (Pattern Int) [Pattern a]

instance Pat Prand where
    runP (Prand n ps) g = a 
        where a = concatMap (\n -> fst $ runPrand g n ps) (runP n g)

runPrand :: (RandomGen g, Pat p) => g -> Int -> [p a] -> ([a], g)
runPrand g num ps = foldr f' ([],g) (replicate num ps) 
    where f' a (b,g') = (runP (fst $ chooseOne a g') g' ++ b, g'') 
              where (_, g'') = next g
                        

instance Functor Prand where
    fmap f (Prand n ps) = Prand n (fmap (fmap f) ps)

chooseOne :: RandomGen g => [a] -> g -> (a, g)
chooseOne xs g = (xs !! idx ,g')
    where (idx, g') = randomR (0, length xs - 1) g

pid :: a -> Pattern a
pid = Pattern . Pid

pseq :: (Functor p, Pat p) => Int -> [p a] -> Pattern a
pseq n xs = Pattern $ Pseq (pid n) $ map Pattern xs

p1 :: Pid Double
p1 = Pid 1

p2 :: Pseq Double
p2 = Pseq (Pattern (Pid 1)) (map Pattern [Pid 1, Pid 2, Pid 3])

p3 :: Pseq Double
p3 = Pseq (Pattern (Pid 2)) [Pattern p1, Pattern p2]

p4 :: Prand Double
p4 = Prand (Pattern (Pid 2)) [Pattern p1, Pattern p2, Pattern p3]

p5 :: Prand Double
p5 = Prand (Pattern (Pid 3)) 
     [Pattern (Pid 1), 
      Pattern (Pid 2), 
      Pattern (Pid 3)]
