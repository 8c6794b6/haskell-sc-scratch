-- | Module to play with pattern.
-- Using both datatype and typeclasses.
-- 
-- XXX: 
--  * Implement Prand. 
--  * Try implement this with making Pat as a typeclass, and then
--    instaniate each pattern from datatype (or newtype).
-- 

module P where

import Control.Applicative
import Data.List (transpose)
import Data.Monoid

data Pat a = Pid a
           | Pseq (Pat Int) [Pat a] 
           | Prand (Pat Int) [Pat a]
           | Pinf a 
           | Pempty
             deriving (Eq, Show)

instance Functor Pat where
    fmap f (Pid a) = Pid (f a)
    fmap f (Pseq p ps) = Pseq p (map (fmap f) ps)
    fmap f (Prand p ps) = Prand p (map (fmap f) ps)
    fmap f (Pinf a) = Pinf (f a)
    fmap f Pempty = Pempty

data Pbind a = Pbind [(String, a)]
             deriving (Eq, Show)

instance Functor Pbind where
    fmap f (Pbind ps) = Pbind $ map (fmap f) ps

runPat :: Pat a -> [a]
runPat (Pid a) = [a]
runPat (Pseq n ps) = 
    concat $ concatMap (\n' -> replicate n' (concatMap runPat ps)) (runPat n)
runPat (Prand n ps) = undefined
runPat (Pinf a) = repeat a
runPat Pempty = []

rp :: (a -> [b]) -> Pbind a -> [[(String, b)]]
rp f (Pbind ps) = transpose $ map g ps 
    where g (n,p) = zipWith (,) (repeat n) (f p)

runPbind :: Pbind (Pat a) -> [[(String, a)]]
runPbind = rp runPat

pat1 :: Pat (String, Double)
pat1 = Pid ("foo", 3)

pat2 :: Pat (String, Double)
pat2 = Pseq (Pid 2) [Pid ("foo",0), Pid ("bar", 1)]

pat3 :: Pat (Pbind (Pat Double))
pat3 = Pseq (Pid 2) [Pid pbind1]

pbind1 :: Pbind (Pat Double)
pbind1 = Pbind [("foo", Pseq (Pid 2) [Pid 1, Pid 2, Pid 3]),
                ("bar", Pseq (Pid 2) [Pid 4, Pid 5, Pid 6]),
                ("buzz", Pseq (Pid 2) [Pid 7, Pid 8, Pid 9])]

type P a = Pat (Pbind (Pat a))

p :: Pat (Pbind (Pat a)) -> [[(String, a)]]
p = concatMap runPbind . runPat

p1 :: P Double
p1 = Pseq (Pid 2)
        [Pseq (Pid 1) 
          [Pid (Pbind [("foo", Pseq (Pid 1) [Pid 1, Pid 2, Pid 3]),("bar",Pid 10)])],
         Pseq (Pid 2)
          [Pid (Pbind [("foo", Pid 2),("bar", Pid 20)])]]

