{-# LANGUAGE ExistentialQuantification, GADTs #-}
------------------------------------------------------------------------------
-- | Another attempt to implement patterns, take 2.

module P2 where

import Data.Monoid
import Control.Applicative
import System.Random

import P1
    (Pbind(..),
     runPb)

class (Functor p) => Pat p where
    runP :: (RandomGen g) => p a -> g -> ([a], g)
    runP' :: (RandomGen g) => p a -> g -> [a]
    runP' p g = fst $  runP p g 
    runPIO :: p a -> IO [a]
    runPIO p = getStdRandom (runP p)

-- data Pattern a = forall p. Pat p => Pattern (p a)
data Pattern a = forall p. (Pat p) => Pattern (p a)

instance Pat Pattern where
    runP (Pattern p) g = runP p g
instance Functor Pattern where
    fmap f (Pattern p) = Pattern (fmap f p)

instance Pat [] where
    runP xs g = (xs, g)

newtype Pid a = Pid a deriving (Eq, Read, Show)

instance Pat Pid where
    runP (Pid a) g = ([a], g)
instance Functor Pid where
    fmap f (Pid a) = Pid (f a)


data Pempty a = Pempty

instance Pat Pempty where
    runP _ g = ([], g)
instance Functor Pempty where
    fmap f Pempty = Pempty


newtype Pinf a = Pinf a

instance Pat Pinf where
    runP (Pinf a) g = (repeat a, g)
instance Functor Pinf where
    fmap f (Pinf a) = Pinf (f a)


data Pseq a = Pseq (Pattern Int) [Pattern a]

instance Pat Pseq where
    runP (Pseq p ps) g = (a, g') 
        where a = concatMap (\n -> runPseq n g ps) (runP' p g)
              g' = snd $ next g
instance Functor Pseq where
    fmap f (Pseq p ps) = Pseq p (map (fmap f) ps)

-- There is a choise: whether to pass the seed for each repetation, or
-- repeat with same seed. Choosed to pass different seed for every repetation.
runPseq :: (RandomGen g, Pat p)=> Int -> g -> [p a] -> [a]
runPseq n g ps = f (concat $ replicate n ps)
    where f = fst . foldr f' z 
          f' a (b,g) = (a'++b, g') 
              where (a',g') = runP a g
          z = ([],g)


-- instance Functor Pattern where
--     fmap f (Pattern p) = Pattern (fmap f p)
 

-- XXX: Show?
--
-- instance (Show p) => Show (Pseq p) where
--     show (Pseq p ps) = "Pseq " ++ show p ++ " " ++ show ps

data Prand a = Prand (Pattern Int) [Pattern a]

instance Pat Prand where
    runP (Prand n ps) g = (a, h)
        where a = concatMap (\n -> fst $ runPrand g n ps) (runP' n g)
              h = snd $ next g
instance Functor Prand where
    fmap f (Prand n ps) = Prand n (fmap (fmap f) ps)

runPrand :: (RandomGen g, Pat p) => g -> Int -> [p a] -> ([a], g)
runPrand g num ps = foldr f' ([], g) (replicate num ps) 
    where f' a (b,g') = (runP' a' g' ++ b, g'') 
              where (a', g'') = chooseOne a g'
                  

chooseOne :: RandomGen g => [a] -> g -> (a, g)
chooseOne xs g = (xs !! idx ,g')
    where (idx, g') = randomR (0, length xs - 1) g

runPbind :: (Pat p, RandomGen g) => Pbind (p a) -> g -> [[(String, a)]]
runPbind p g = undefined

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

p6 :: Pseq Double
p6 = Pseq (Pattern (Pid 3)) $
     [ Pattern $ Prand (Pattern $ Pid 1) $ map Pattern [Pid 1, Pid 2, Pid 3],
       Pattern $ Prand (Pattern $ Pid 2) $ map Pattern [Pid 11, Pid 12],
       Pattern $ Prand (Pattern $ Pid 3) $ map (Pattern . Pid) [101..999]] 
       
