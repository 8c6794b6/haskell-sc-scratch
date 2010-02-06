{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, DeriveDataTypeable,
  FlexibleInstances #-}
------------------------------------------------------------------------------
-- | Attempt to implement patterns, take 5.
--

module P5 where

import Control.Applicative
import Data.Generics
import Data.List (transpose)

data Pattern a = Pid a
	       | Pseq (Pattern PValue) [Pattern a]
	       | Prand (Pattern PValue) [Pattern a]
	       | Pbind [(String, (Pattern a))]
		 deriving (Eq, Show, Read, Data, Typeable)

instance Functor Pattern where
    fmap f (Pid a) = Pid (f a)
    fmap f (Pseq p ps) = Pseq p (map (fmap f) ps)
    fmap f (Prand p ps) = Prand p (map (fmap f) ps)
    fmap f (Pbind ps) = Pbind (fmap (fmap $ fmap f) ps)

data PValue = PNum Double
	    | PString String
	    | PPair PValue PValue
	    | PList [PValue]
	      deriving (Eq, Show, Read, Data, Typeable)

instance Num PValue where
    fromInteger = PNum . fromInteger
    PNum a + PNum b = PNum (a+b)
    _ + _ = error "not a number"
    PNum a * PNum b = PNum (a*b)
    _ * _ = error "not a number"
    PNum a - PNum b = PNum (a-b)
    _ - _ = error "not a number"
    abs (PNum a) = PNum (abs a)
    abs _ = error "not a number"
    signum (PNum a) = PNum (signum a)
    signum _ = error "not a number"

instance Enum PValue where
    succ (PNum a) = PNum (succ a)
    pred (PNum a) = PNum (pred a)
    toEnum = PNum . fromIntegral
    fromEnum (PNum a) = ceiling a

class Pv p where
    toPv :: p -> PValue
    fromPv :: PValue -> p

instance Pv PValue where
    toPv = id
    fromPv = id

instance Pv Int where
    toPv = PNum . fromIntegral
    fromPv (PNum a) = ceiling a
    fromPv _ = 0

instance Pv Double where
    toPv = PNum
    fromPv (PNum a) = a
    fromPv _ = 0

instance Pv String where
    toPv = PString
    fromPv (PString s) = s
    fromPv _ = ""

instance (Pv a, Pv b) => Pv (a, b) where
    toPv (a, b) = PPair (toPv a) (toPv b)
    fromPv (PPair a b) = (fromPv a, fromPv b)
    fromPv _ = error "not a pair"

instance Pv a => Pv [a] where
    toPv a = PList (map toPv a)
    fromPv (PList a) = map fromPv a
    fromPv _ = []

runP :: Pattern PValue -> [PValue]
runP (Pid a) = [a]
runP (Pseq p ps) =
    concatMap (\x -> concat $ replicate (fromPv x) $ concatMap runP ps) (runP p)
runP (Pbind ps) = map toPv $ transpose $ map (map toPv . f) ps
    where f (n,p) = zip (repeat n) (runP p)

--
-- Sample patterns
--

p1 :: Pattern PValue
p1 = Pseq (Pid 3)
     [Pid 1, Pid 2, Pid 3]

p2 :: Pattern PValue
p2 = fmap f p1 where
    f (PNum n) = PNum (n*333)
    f _ = PNum 0

p3 :: Pattern PValue
p3 = Pseq (Pid 3)
     [Pbind [("foo", p1),("bar",p2)],
      Pbind [("foo", p2),("bar",p1)]]

p3' :: [[(String,Double)]]
p3' = map fromPv $ runP p3

p4 :: Pattern PValue
p4 = Pbind [("foo", p1), ("bar", p2)]
