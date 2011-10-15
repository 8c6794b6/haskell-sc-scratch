{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://okmij.org/ftp/continuations/Haskell-tutorial.pdf>

-}
module Tutorial where

import Control.Applicative
import Control.Monad

import Control.Monad.Cont hiding (Cont(..))
import Data.Functor.Identity

-- import Control.Monad.Cont

data Cont r a = Cont {unCont :: ContT r Identity a}

runCon :: Cont r a -> (a -> r) -> r
runCon c k = runIdentity (runContT (unCont c) (Identity . k))

-- t1 :: Cont w Integer
-- t1 =
--   liftM2 (-)
--   (reset
--    (liftM2 (+) (return 3)
--     (shift (\k -> liftM2 (*) (return 5) (return 2)))))
--   (return 1)

-- t1' =
--   liftM2 (-)
--   (reset
--    (liftM2 (+) (return 3)
--     (shift (\k -> return (5*2)))))
--   (return 1)

infixl 6 -!, +!
infixl 7 *!

(-!),(+!),(*!) :: (Num a, Monad m) => m a -> m a -> m a
(-!) = liftM2 (-)
(+!) = liftM2 (+)
(*!) = liftM2 (*)

--
-- t2 = reset (return 3 +! shift (\k -> return (5*2))) -! return 1

-- Exercise:
-- Enable to Remove 'return' from above expression, so that we can write:
--
t2' = reset (3 +! shift (\k -> 5 * 2)) -! 1

-- instance (Show (m a), Eq (m a), Num a, Functor m, Monad m) => Num (m a) where

instance Num a => Num (C w a) where
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  (-) = liftM2 (-)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = return . fromInteger

instance Eq ((->) a a) where
  _ == _ = True

instance Show ((->) a a) where
  show _ = "<function>"

instance Show (C w a) where
  show _ = "<continuation>"

instance Eq (C w a) where
  _ == _ = False

-- OchaCaml expression is:
--
-- > fst (reset (fun () -> let x = ("hi","bye") in (x,x)))
--
-- t13 = liftM fst (reset $ do
--   x <- shift (\k -> return ("hi", "bye"))
--   return (x,x))

t3 = reset (return 2 *! shift (\k -> return $ k (k 10))) +! return 1

-- shift = undefined

-- reset = undefined

data C w a = C {unC :: (a->w) -> w}

instance Monad (C w) where
  return x = C (\k -> k x)
  m >>= f  = C (\k -> unC m (\v -> unC (f v) k))

instance Functor (C w) where
  fmap f (C m) = C (\k -> m (k . f))

instance Applicative (C w) where
  pure  = return
  m <*> a = m >>= \h -> fmap h a

runC :: C w w -> w
runC m = unC m id

reset :: C a a -> C w a
reset = return . runC

shift :: ((a->w) -> C w w) -> C w a
shift f = C (runC . f)

------------------------------------------------------------------------------
-- From here, written while reading ContExample.hs

runCT :: Monad m => ContT r m r -> m r
runCT m = runContT m return

resetT :: Monad m => ContT a m a -> ContT w m a
resetT = lift . runCT

shiftT :: Monad m => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (runCT . f)

data Tree = Empty | Node Tree Int Tree deriving Show

make_complete :: Int -> Tree
make_complete d = go d 1 where
  go 0 _ = Empty
  go d n = Node (go (d-1) (2*n)) n (go (d-1) (2*n+1))

tree1,tree2 :: Tree
tree1 = make_complete 3
tree2 = make_complete 4

print_tree :: Tree -> IO ()
print_tree t = case t of
  Empty -> return ()
  Node l n r -> do
    print_tree l
    putStrLn $ "walked to " ++ show n
    print_tree r

data Thread m a = Done | Resume a (m (Thread m a))

walk_tree :: MonadIO m => Tree -> m (Thread m Int)
walk_tree t = runCT (walk_tree' t >> return Done)

walk_tree' :: MonadIO m => Tree -> ContT (Thread m Int) m ()
walk_tree' Empty = return ()
walk_tree' (Node l n r) = do
  walk_tree' l
  liftIO $ putStrLn $ "walked to " ++ show n
  yield n
  walk_tree' r
  where
    yield n = shiftT (\k -> return $ Resume n (k ()))

walk1 :: IO ()
walk1 = walk_tree tree1 >>= check where
  check Done = return ()
  check (Resume n r) = do
    putStrLn $ "found " ++ show n
    r >>= check

same_fringe :: Tree -> Tree -> IO ()
same_fringe t1 t2 = ap2 check (walk_tree t1) (walk_tree t2) where
  check Done Done = putStrLn "*** The same ***"
  check (Resume n _) Done =
    putStrLn $ unwords ["First tree",show n,"second is empty"]
  check Done (Resume n _) =
    putStrLn $ unwords ["Second tree", show n,"first is empty"]
  check (Resume n1 _) (Resume n2 _) | n1 /= n2 =
    putStrLn $ unwords ["First tree", show n1,"second tree",show n2]
  check (Resume n1 r1) (Resume n2 r2) = ap2 check r1 r2
  ap2 f x y = x >>= \x' -> y >>= \y' -> f x' y'

sf1 = same_fringe tree1 tree1
sf2 = same_fringe tree2 tree2

sf12 = same_fringe tree1 tree2