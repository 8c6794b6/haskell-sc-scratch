{-# LANGUAGE NoMonomorphismRestriction #-}
module Ex02 where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State

add :: Num a => a -> a -> (a -> t) -> t
add x y k = k (x + y)

mul :: Num a => a -> a -> (a -> t) -> t
mul x y k = k (x * y)

add2 :: Num a => a -> a -> (a -> IO t) -> IO t
add2 x y k = let z = x + y in print z >> k z

fac :: Integral a => a -> a
fac n = case n of
  0 -> 1
  _ -> n * fac (n-1)

facC :: Integral a => a -> (a -> b) -> b
facC n k = case n of
  0 -> k 1
  _ -> facC (n-1) (\x -> k (n * x))

--   facC 3 id
-- = facC 2 (\x -> id (3 * x))
-- = facC 1 (\y -> (\x -> id (3 * x)) (2 * y))
-- = facC 0 (\z -> (\y -> (\x -> id (3 * x)) (2 * y)) (1 * z))
-- = (\z -> (\y -> (\x -> id (3 * x)) (2 * y)) (1 * z)) 1
-- = (\y -> (\x -> id (3 * x)) (2 * y)) (1 * 1)
-- = (\x -> id (3 * x)) (2 * (1 * 1))
-- = id (3 * (2 * (1 * 1)))
-- = 3 * (2 * (1 * 1))
-- = 6

fib n = case n of
  0 -> 1
  1 -> 1
  _ -> fib (n-2) + fib (n-1)

fibC n k = case n of
  0 -> k 1
  1 -> k 1
  _ -> fibC (n-1) (\x -> fibC (n-2) (\y -> k (x + y)))

idC :: a -> (a -> b) -> b
idC x k = k x

succC :: Enum a => a -> (a->b) -> b
succC a k = k (succ a)

predC :: Enum a => a -> (a->b) -> b
predC a k = k (pred a)

newtype CT w a = CT {unCT :: (a -> w) -> w}

instance Monad (CT w) where
  return x = CT (\k -> k x)
  CT m >>= f = CT (\k -> m (\v -> unCT (f v) k))

ex1 = do
  a <- return 1
  b <- return 2
  return $ a + b

ex2 = do
  a <- return 1
  b <- CT (\bob -> bob 10)
  return $ a + b

ex3 = do
  a <- return 1
  b <- CT (\bob -> bob 10 ++ bob 20)
  return $ a + b

i :: Monad m => (m w) -> CT (m a) w
i x = CT (\fred -> x >>= fred)

run :: Monad m => CT (m a) a -> m a
run m = unCT m return

ex4 = run $ do
  a <- i [1,2]
  b <- i [10,20]
  return $ a + b

ex5 = run $ do
  i $ putStrLn "What is youre name?"
  name <- i getLine
  i $ putStrLn $ "Merry Xmax " ++ name

ex6 = do
  a <- return 1
  b <- CT (\bob -> "Hello, bob")
  return $ a + b

test6 = unCT ex6 show
