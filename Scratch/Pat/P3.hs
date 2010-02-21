{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
------------------------------------------------------------------------------
-- | Another attempt to implement patterns, take 3.

module P3 where

import Control.Applicative
import Data.List (transpose)
import Data.Monoid
import System.Random

class Pattern p where
    runP :: (RandomGen g) => p a -> g -> ([a], g)
    runP' :: (RandomGen g) => p a -> g -> [a]
    runP' p g = fst $ runP p g
    runPIO :: p a -> IO [a]
    runPIO p = getStdRandom (runP p)

data AnyPattern a = forall p. Pattern p => AnyPattern (p a)

instance Pattern AnyPattern where
    runP (AnyPattern p) g = runP p g

instance Pattern [] where
    runP xs g = (xs, g)

data Pat a = Pempty
           | Pid a
           | Pseq (Pat Int) [Pat a]
           | Prand (Pat Int) [Pat a]
           | Pinf a
             deriving (Eq, Read, Show)

instance Pattern Pat where
    runP Pempty g = ([], g)
    runP (Pid a) g = ([a], g')
        where g' = snd $ next g
    runP (Pseq n ps) g = (a, g')
        where a = concatMap (\n -> runPseq n g ps) (runP' n g)
              g' = snd $ next g
    runP (Prand n ps) g = (a, h)
        where a = concatMap (\n -> fst $ runPrand g n ps) (runP' n g)
              h = snd $ next g
    runP (Pinf a) g = (repeat a, g') 
        where g' = snd $ next g 

instance Functor Pat where
    fmap f (Pid a) = Pid (f a)
    fmap f (Pseq n ps) = Pseq n (map (fmap f) ps)
    fmap f (Prand n ps) = Prand n (map (fmap f) ps)
    fmap f (Pinf p) = Pinf (f p)

instance Monoid (Pat a) where
    mempty = Pempty
    p1 `mappend` p2 = Pseq (Pid 1) [p1, p2]

instance Applicative Pat where
    pure = Pid

    Pempty <*> p = Pempty
    Pid f <*> p = fmap f p
    Pseq n xs <*> Pseq m ys = Pseq n (zipWith (<*>) xs ys)
    Pseq n xs <*> Prand m ys = Pseq n (zipWith (<*>) xs ys)
    Pseq n xs <*> p = (head xs) <*> p
    Prand n xs <*> Pseq m ys = Prand n (zipWith (<*>) xs ys)
    Prand n xs <*> Prand m ys = Prand n (zipWith (<*>) xs ys)
    Prand n xs <*> p = (head xs) <*> p
    Pinf f <*> p = fmap f p 

instance (Num a) => Num (Pat a) where
    p1 + p2 = (+) <$> p1 <*> p2
    p1 * p2 = (*) <$> p1 <*> p2
    p1 - p2 = (-) <$> p1 <*> p2
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Pid . fromInteger

instance (Enum a) => Enum (Pat a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = Pid . toEnum
    fromEnum (Pid a) = fromEnum a
    fromEnum (Pinf a) = fromEnum a
    fromEnum _ = error "fromEnum not defined"

instance Fractional a => Fractional (Pat a) where
    p1 / p2 = (/) <$> p1 <*> p2
    recip = fmap recip
    fromRational = Pid . fromRational
    
data RandomRs a = Random a => RandomRs Int (a, a)

instance Pattern RandomRs where
    runP (RandomRs n r) g = (a, g')
        where g' = snd $ next g
              a = take n $ randomRs r g

-- | @Pchain@ in sclang could be emulated by applying @mappend@ to "Pbind". 
newtype Pbind k v = Pbind [(k, v)]
               deriving (Eq, Read, Show)

instance Monoid (Pbind k v) where
    mempty = Pbind []
    Pbind xs  `mappend` Pbind ys = Pbind (xs `mappend` ys)

instance Functor (Pbind k) where
    fmap f (Pbind xs) = Pbind (map (fmap f) xs)

-- type P k a = Pat (Pbind k (Pat a))
type P k a = (Pattern p1, Pattern p2) => p1 (Pbind k (p2 a))

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

runPb :: (a -> [b]) -> Pbind k a -> [[(k, b)]]
runPb f (Pbind ps) = transpose $ map f' ps
    where f' (n, p) = zipWith (,) (repeat n) (f p)

runPbind :: (Pattern p, RandomGen g) => Pbind k (p a) -> g -> [[(k, a)]]
runPbind p g = runPb (flip runP' g) p

p :: (Pattern p1, Pattern p2, RandomGen g) => 
     p1 (Pbind k (p2 a))  -> g -> ([[(k, a)]], g)
p pat gen = (a, g') 
    where a = concatMap (flip runPbind gen) . (flip runP' gen) $ pat
          g' = snd $ next gen

--
-- Sample patterns and pbinds.
-- 

p1 :: Pat Double
p1 = Pid 1

p2 :: Pat Double
p2 = Pseq (Pid 2) [Pid 1, Pid 2, Pid 3]

p3 :: Pat Double
p3 = Prand (Pid 6) $ map Pid [1..100]

p4 :: AnyPattern (AnyPattern Double)
p4 = AnyPattern [AnyPattern [1..10],
                 AnyPattern (RandomRs 10 (1, 100))]

pb1 :: Pat (Pbind String (AnyPattern Double))
pb1 = Prand (Pid 2)
      [Pid $ Pbind
           [("foo", AnyPattern p2),
            ("bar", AnyPattern rs1)],
       Pid $ Pbind
           [("foo", AnyPattern p3),
            ("bar", AnyPattern [1..6])]]

pb2 :: Pbind String (Pat Double)
pb2 = Pbind [("dur", 
              Prand (Pinf 99)
                [Pseq 1 [0.125,0.125,0.5],
                 Pseq 1 [0.3, 0.5, 0.2]]),
             ("legato", 
              Prand (Pinf 99) 
                [Pseq 1 [0.1, 0.6, 1.01],
                 Pseq 1 [0.1, 0.3, 0.6]])]

pb3 :: Pbind String (Pat Double)
pb3 = Pbind [("degree", 
              Prand (Pinf 99) 
                [-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11])]

rs1 :: RandomRs Double
rs1 = RandomRs 6 (1,20)