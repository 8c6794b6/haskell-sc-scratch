{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
-}
module Scratch01 where

import Data.IORef
import Data.STRef
import Control.Monad.ST
import Control.Monad.Trans

import qualified Data.IntMap as IM
import qualified Data.Map as M

f :: [Bool] -> [Int] -> Int
f xs ys = length xs + length ys

------------------------------------------------------------------------------
-- Section 2.

{-

IORef functions:

newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()

STRef functions:

newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()

-}

class Mutation m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

instance Mutation IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance Mutation (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

ref01 = do
  a <- newRef 1
  readRef a

{-|

From dumped core, we can see IORef is used inside the do expression.

> Scratch02.readAndPrint :: GHC.Types.IO ()
> [GblId]
> Scratch02.readAndPrint =
>   GHC.Base.>>=
>     @ GHC.Types.IO
>     GHC.Base.$fMonadIO
>     @ (GHC.IORef.IORef GHC.Types.Char)
>     @ ()
>     (Scratch02.newRef
>        @ GHC.Types.IO
>        @ GHC.IORef.IORef
>        Scratch02.$fMutationIOIORef
>        @ GHC.Types.Char
>        (GHC.Types.C# 'x'))
>     (\ (r_acr :: GHC.IORef.IORef GHC.Types.Char) ->
>        GHC.Base.>>=
>          @ GHC.Types.IO
>          GHC.Base.$fMonadIO
>          @ GHC.Types.Char
>          @ ()
>          (Scratch02.readRef
>             @ GHC.Types.IO
>             @ GHC.IORef.IORef
>             Scratch02.$fMutationIOIORef
>             @ GHC.Types.Char
>             r_acr)
>          (\ (y_acs :: GHC.Types.Char) ->
>             System.IO.print @ GHC.Types.Char GHC.Show.$fShowChar y_acs))
>

-}
readAndPrint :: IO ()
readAndPrint = do
  r <- newRef 'x'
  v <- readRef r
  print v


------------------------------------------------------------------------------
-- 2.2 Arithmetic

class Add a b where
  type Sumty a b :: *
  add :: a -> b -> Sumty a b

instance Add Integer Double where
  type Sumty Integer Double = Double
  add a b = fromInteger a + b

instance Add Double Integer where
  type Sumty Double Integer = Double
  add a b = a + fromInteger b

instance Num a => Add a a where
  type Sumty a a = a
  add x y = x + y

{-
Alternative: Define Sumty outside of Add class.

class Add a b where
  add :: a -> b -> Sumty a b

type family Sumty a b :: *

type instance Sumty Integer Double = Double
type instance Sumty Double Integer = Double

instance Add Integer Double where
  add a b = fromIntegral a + b

instance Add Double Integer where
  add a b = a + fromIntegral b
-}

{-

This works:

> > add (3::Int) (3::Int)
> 6

But these will not:

> > add 3 3 :: Sumty Int Int
>     Couldn't match type `Sumty a0 b0' with `Int'
>     Expected type: Sumty Int Int
>       Actual type: Sumty a0 b0
>     In the return type of a call of `add'
>     In the expression: add 3 3 :: Sumty Int Int
>     In an equation for `it': it = add 3 3 :: Sumty Int Int

> > add (3::Int) 3 :: Sumty Int Int
> <interactive>:1:1:
>     No instance for (Add Int b0)
>       arising from a use of `add'
>     Possible fix: add an instance declaration for (Add Int b0)
>     In the expression: add (3 :: Int) 3 :: Sumty Int Int
>     In an equation for `it': it = add (3 :: Int) 3 :: Sumty Int Int
>
> <interactive>:1:1:
>     Couldn't match type `Sumty Int b0' with `Int'
>     Expected type: Sumty Int Int
>       Actual type: Sumty Int b0
>     In the return type of a call of `add'
>     In the expression: add (3 :: Int) 3 :: Sumty Int Int
>     In an equation for `it': it = add (3 :: Int) 3 :: Sumty Int Int
>

-}

class Cons a b where
  type ResTy a b
  cons :: a -> [b] -> [ResTy a b]

instance Cons Integer Double where
  type ResTy Integer Double = Double
  cons x ys = fromIntegral x : ys

------------------------------------------------------------------------------
-- 2.3 Graphs

class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]
instance Graph G1 where
  type Vertex G1 = Int
  data Edge G1 = MkEdge (Vertex G1) (Vertex G1)
  src = undefined
  tgt = undefined
  outEdges = undefined

newtype G2 = G2 (Map (Vertex G2) [Vertex G2])
instance Graph G2 where
  type Vertex G2 = String
  data Edge G2 = MkEdge2 Int (Vertex G2) (Vertex G2)
  src = undefined
  tgt = undefined
  outEdges = undefined

------------------------------------------------------------------------------
-- 2.4 Associated types

-- Code example skipped.

------------------------------------------------------------------------------
-- 2.5 Type functions are open

newtype Age = MkAge Int deriving (Eq, Show)

instance Add Age Int where
  type Sumty Age Int = Age
  add (MkAge a) b = MkAge (a + b)

------------------------------------------------------------------------------
-- 2.6 Type functions may be recursive

instance (Add Integer a) => Add Integer [a] where
  type Sumty Integer [a] = [Sumty Integer a]
  add x ys = map (add x) ys

{-
ghci> add (3::Integer) [1.0,2.0,3.0::Double]
[4.0,5.0,6.0]
-}

instance (Monad m, Mutation m, MonadTrans t) => Mutation (t m) where
  type Ref (t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

g :: Bool -> Integer
g = fromTable (toTable fb)

fb :: Bool -> Integer
fb p = if p then factorial 100 else fibonacci 30

fibonacci :: Integer -> Integer
fibonacci n = case n of
  0 -> 1
  1 -> 1
  _ -> fibonacci (n-1) + fibonacci (n-2)

factorial :: Integer -> Integer
factorial n = case n of
  0 -> 1
  _ -> n * factorial (n-1)

instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum t _) (Left a)  = fromTable t a
  fromTable (TSum _ t) (Right a) = fromTable t a

instance (Memo a, Memo b) => Memo (a,b) where
  data Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y

instance Memo a => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w))
  toTable f = TList (f []) (toTable (\x -> toTable (\xs -> f (x:xs))))
  fromTable (TList t _) [] = t
  fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

{-
NG:
Table for all Int values will be created when toTable .
Want to do this in lazy manner.

instance Memo Int where
  data Table Int w = TIMap (IM.IntMap w)
  toTable f = TIMap (IM.fromList $ map (\i -> (i,f i)) [minBound .. maxBound])
  fromTable (TIMap t) k = t IM.! k

-}
instance Memo Int where
  data Table Int w = TIMap (IM.IntMap w)
  -- toTable f = TIMap (toTable (\x -> IM.insert x (f x) IM.empty))
  toTable f = undefined
  fromTable (TIMap m) k = m IM.! k

fib :: Int -> Int
fib n = case n of
  0 -> 1
  1 -> 1
  _ -> fib (n-2) + fib (n-1)

------------------------------------------------------------------------------
-- 3.3 Generic finite maps

class Key k where
  data Map k :: * -> *
  empty :: Map k v
  lkup  :: k -> Map k v -> Maybe v
  insert :: k -> v -> Map k v -> Map k v

instance Key Bool where
  data Map Bool v = MB (Maybe v) (Maybe v)
  empty = MB Nothing Nothing
  lkup p (MB t f) = if p then t else f
  insert k v (MB t f) = if k then MB (Just v) f else MB t (Just v)

instance (Key a, Key b) => Key (Either a b) where
  data Map (Either a b) v = MS (Map a v) (Map b v)
  empty = MS empty empty
  lkup k (MS l r) = case k of
    Right a -> lkup a r
    Left  b -> lkup b l
  insert k v (MS l r) = case k of
    Right a -> MS l (insert a v r)
    Left b  -> MS (insert b v l) r

instance (Key a, Key b) => Key (a,b) where
  data Map (a,b) v = MP (Map a (Map b v))
  empty = MP empty
  lkup (a,b) (MP m) = case lkup a m of
    Just m' -> lkup b m'
    Nothing -> Nothing
  insert (a,b) v (MP m) = case lkup a m of
    Nothing -> MP (insert a (insert b v empty) m)
    Just m' -> MP (insert a (insert b v m') m)

instance Key Int where
  data Map Int v = MI (IM.IntMap v)
  empty = MI IM.empty
  lkup k (MI m) = IM.lookup k m
  insert k v (MI m) = MI $ IM.insert k v m

instance Key Char where
  data Map Char v = MC (IM.IntMap v)
  empty = MC IM.empty
  lkup k (MC m) = IM.lookup (fromEnum k) m
  insert k v (MC m) = MC $ IM.insert (fromEnum k) v m

instance Key () where
  data Map () v = MU (Maybe v)
  empty = MU Nothing
  lkup k (MU v) = v
  insert k v _ = MU (Just v)

instance (Key a) => Key [a] where
  data Map [a] v = ML (Map a (Map [a] v)) (Maybe v)
  empty = ML empty Nothing
  lkup []     (ML _ v)  = v
  lkup (x:xs) (ML vs _) = case lkup x vs of
    Just vs' -> lkup xs vs'
    Nothing  -> Nothing
  insert []     v (ML ms m) = ML ms (Just v)
  insert (x:xs) v (ML ms m) = case lkup x ms of
    Just ms' -> ML (insert x (insert xs v ms') ms) m
    Nothing  -> ML (insert x (insert xs v empty) ms) m

intPairTest :: (Int,Int) -> Maybe Int
intPairTest k =
  let ops = [insert (x,y) (x*y) | let i = [1..100::Int], x <- i, y <- i]
      m = foldr (.) id ops empty
  in  lkup k m

-- > intPairTest (2,8)
-- Just 16

mkWordMap :: IO (Map String Int)
mkWordMap = do
  ws <- lines `fmap` readFile "/usr/share/dict/words"
  return $ foldr (.) id (zipWith insert ws [1..]) empty

listKeyTest :: [String] -> IO ()
listKeyTest ks = do
  m <- mkWordMap
  mapM_ (\k -> print (k,lkup k m)) ks

lookupWord :: [String] -> IO ()
lookupWord ks = do
  ws <- lines `fmap` readFile "/usr/share/dict/words"
  let m = foldr (.) id (zipWith M.insert ws [1..]) M.empty
  mapM_ (\k -> print (k, M.lookup k m)) ks

-- ghci> listKeyTest ["apple", "apples", "banana", "bananas", "cherry"]
-- ("apple",Just 21130)
-- ("apples",Just 21134)
-- ("banana",Just 23099)
-- ("bananas",Just 23101)
-- ("cherry",Just 29638)

------------------------------------------------------------------------------
-- 3.4 Session types and their duality

data Stop = Done
newtype In a b = In (a -> IO b)
data Out a b = Out a (IO b)

add_server :: In Int (In Int (Out Int Stop))
add_server = In $ \x -> return $ In $ \y -> do
  putStrLn "Thinking"
  return $ Out (x+y) (return Done)

class Session a where
  type Dual a
  run :: a -> Dual a -> IO ()

instance Session b => Session (In a b) where
  type Dual (In a b) = Out a (Dual b)
  run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance Session b => Session (Out a b) where
  type Dual (Out a b) = In a (Dual b)
  run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

instance Session Stop where
  type Dual Stop = Stop
  run Done Done = return ()

add_client :: Out Int (Out Int (In Int Stop))
add_client = Out 3 $ return $ Out 4 $ do
  putStrLn "Waiting"
  return $ In $ \z -> print z >> return Done

neg_server :: In Int (Out Int Stop)
neg_server = In $ \x -> do
  putStrLn "Thinking"
  return $ Out (-x) (return Done)

neg_client :: Out Int (In Int Stop)
neg_client = Out 8 $ do
  putStrLn "Waiting"
  return $ In $ \z -> print z >> return Done

instance (Session a, Session b) => Session (Either a b) where
  type Dual (Either a b) = (Dual a, Dual b)
  run (Left y)  (x,_) = run y x
  run (Right y) (_,x) = run y x

instance (Session a, Session b) => Session (a,b) where
  type Dual (a,b) = Either (Dual a) (Dual b)
  run (x,_) (Left y)  = run x y
  run (_,x) (Right y) = run x y

server = (neg_server,add_server)

client :: Either (Out Int (In Int Stop)) (Out Int (Out Int (In Int Stop)))
client = Right add_client
