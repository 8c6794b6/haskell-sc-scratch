{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Typefun where

import qualified Data.IORef as IO
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.STRef as ST

--
-- 1. Introduction
--

f :: [Int] -> [Bool] -> Int
f is bs = length is + length bs

--
-- 2. Associated Types: indexing types by types
--

class Mutation m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

instance Mutation IO where
  type Ref IO = IO.IORef
  newRef = IO.newIORef
  readRef = IO.readIORef
  writeRef = IO.writeIORef

-- instance Mutation (ST.STRef s) where
--   type Ref (ST.STRef s) = ST.STRef s
--   newRef = ST.newSTRef
--   readRef = ST.readSTRef
--   writeRef = ST.writeSTRef

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance (Monad m, Mutation m, MonadTrans t) => Mutation (t m) where
  type Ref (t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef


data T m a = MkT [Ref m a]

class Add a b where
  type SumTy a b :: *
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

instance (Num a) => Add a a where
  type SumTy a a = a
  add x y = x + y

newtype Age = MkAge Int

instance Add Age Int where
  type SumTy Age Int = Age
  add (MkAge a) n = MkAge (a+n)

instance (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add x y = map (add x) y

class Cons a b where
  type ResTy a b :: *
  cons :: a -> [b] -> [ResTy a b]

instance Cons Integer Double where
  type ResTy Integer Double = Double
  cons x ys = fromInteger x : ys

instance Num a => Cons a a where
  type ResTy a a = a
  cons x ys = x : ys

class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]

instance Graph G1 where
  type Vertex G1 = Int
  data Edge G1 = MkEdge1 (Vertex G1) (Vertex G1)
  src = undefined
  tgt = undefined
  outEdges = undefined

newtype G2 = G2 (M.Map (Vertex G2) [Vertex G2])

instance Graph G2 where
  type Vertex G2 = String
  data Edge G2 = MkEdge2 Int (Vertex G2) (Vertex G2)
  src = undefined
  tgt = undefined
  outEdges = undefined

neighbours g v = map tgt (outEdges g v)

--
-- 3. Optimised container representations
--

class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

memo_bool :: Bool -> Int
memo_bool = fromTable (toTable f_bool)

f_bool :: Bool -> Int
f_bool True  = 100
f_bool False = 100

instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum a b) = either (fromTable a) (fromTable b)

instance (Memo a, Memo b) => Memo (a,b) where
  newtype Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y

instance (Memo a) => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w))
  toTable f = TList (f []) (toTable (\x -> toTable (\xs -> f (x:xs))))
  fromTable (TList t _) [] = t
  fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

class Key k where
  data Map k :: * -> *
  empty :: Map k v
  lkup :: k -> Map k v -> Maybe v

instance Key Bool where
  data Map Bool e = MB (Maybe e) (Maybe e)
  empty = MB Nothing Nothing
  lkup False (MB v _) = v
  lkup True  (MB _ v) = v

instance (Key a, Key b) => Key (Either a b) where
  data Map (Either a b) e = MS (Map a e) (Map b e)
  empty = MS empty empty
  lkup (Left k) (MS m _)  = lkup k m
  lkup (Right k) (MS _ m) = lkup k m

instance (Key a, Key b) => Key (a,b) where
  data Map (a,b) e = MP (Map a (Map b e))
  empty = MP empty
  lkup (a,b) (MP m) = maybe Nothing (lkup b) (lkup a m)

instance Key Int where
  newtype Map Int e = MI (IM.IntMap e)
  empty = MI IM.empty
  lkup k (MI m) = IM.lookup k m

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

instance (Session b) => Session (In a b) where
  type Dual (In a b) = Out a (Dual b)
  run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance (Session b) => Session (Out a b) where
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

instance (Session a, Session b) => Session (Either a b) where
  type Dual (Either a b) = (Dual a, Dual b)
  run (Left y)  (x,_) = run y x
  run (Right y) (_,x) = run y x

instance (Session a, Session b) => Session (a,b) where
  type Dual (a,b) = Either (Dual a) (Dual b)
  run (x,_) (Left y)  = run x y
  run (_,x) (Right y) = run x y

server :: (In Int (Out Int Stop), In Int (In Int (Out Int Stop)))
server = (neg_server, add_server)

client :: Either a (Out Int (Out Int (In Int Stop)))
client = Right add_client

--
-- 4. Typed sprintf and sscanf
--

data F f where
  Lit :: String -> F L
  Val :: Parser val -> Printer val -> F (V val)
  Cmp :: F f1 -> F f2 -> F (C f1 f2)

data L
data V val
data C f1 f2

type Parser a = String -> [(a,String)]
type Printer a = a -> String

f_ld :: F L
f_ld = Lit "day"

f_lds :: F (C L L)
f_lds = Cmp (Lit "day") (Lit "s")

f_dn :: F (C L (V Int))
f_dn = Cmp (Lit "day ") int

f_nds :: F (C (V Int) (C L L))
f_nds = Cmp int (Cmp (Lit " day") (Lit "s"))

int :: F (V Int)
int = Val reads show

type SPrintf f = TPrinter f String
type family TPrinter f x
type instance TPrinter L x = x
type instance TPrinter (V val) x = val -> x
type instance TPrinter (C f1 f2) x = TPrinter f1 (TPrinter f2 x)

-- | Try:
--
-- > > sprintf f_ld
-- > "day"
-- > > sprintf f_nds 8
-- > "8 days"
--
sprintf :: F f -> SPrintf f
sprintf p = printer p id

printer :: F f -> (String -> a) -> TPrinter f a
printer (Lit l) k     = k l
printer (Val _ v) k   = \x -> k (v x)
printer (Cmp f1 f2) k = printer f1 $ \s1 -> printer f2 $ \s2 -> k (s1++s2)

type SScanf f = String -> Maybe (TParser f (), String)

type family TParser f x
type instance TParser L x = x
type instance TParser (V val) x = (x,val)
type instance TParser (C f1 f2) x = TParser f2 (TParser f1 x)

sscanf :: F f -> SScanf f
sscanf fmt inp = parser fmt () inp

parser :: F f -> a -> String -> Maybe (TParser f a, String)
parser (Lit str) v s = parseLit str v s
parser (Val reads _) v s = parseVal reads v s
parser (Cmp f1 f2) v s = case parser f1 v s of
  Nothing -> Nothing
  Just (v1,s1) -> parser f2 v1 s1

parseLit :: String -> a -> String -> Maybe (a, String)
parseLit str v s = case L.stripPrefix str s of
  Nothing -> Nothing
  Just s' -> Just (v,s')

parseVal :: Parser b -> a -> String -> Maybe ((a,b), String)
parseVal reads v s = case reads s of
  [(v',s')] -> Just ((v,v'),s')
  _         -> Nothing

newtype Dollars = MkD Int

dollars :: F (V Dollars)
dollars = Val read_dol show_dol where
  read_dol ('$':s) = [(MkD d, s) | (d,s) <- reads s]
  read_dol _       = []
  show_dol (MkD d) = '$' : show d

--
-- 5. Fun with phantom types
--

data Zero
data Succ n

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven

class Nat n where
  toInt :: n -> Int
instance Nat Zero where
  toInt _ = 0
instance (Nat n) => Nat (Succ n) where
  toInt _  = 1 + toInt (undefined :: n)

newtype Pointer n = MkPointer Int
newtype Offset n = MkOffset Int

multiple :: forall n. (Nat n) => Int -> Offset n
multiple i = MkOffset (i * toInt (undefined :: n))

addP :: Pointer m -> Offset n -> Pointer (GCD Zero m n)
addP (MkPointer x) (MkOffset y) = MkPointer (x+y)

type family GCD d m n
type instance GCD d Zero Zero = d
type instance GCD d (Succ m) (Succ n) = GCD (Succ d) m n
type instance GCD Zero (Succ m) Zero = Succ m
type instance GCD (Succ d) (Succ m) Zero = GCD (Succ Zero) d m
type instance GCD Zero Zero (Succ n) = Succ n
type instance GCD (Succ d) Zero (Succ n) = GCD (Succ Zero) d n

class PMonad m where
  unit :: a -> m p p a
  bind :: m p q a -> (a -> m q r b) -> m p r b

data Lnil
data Lcons l a

data Locked
data Unlocked

newtype LockM p q a = LockM { unLockM :: IO a }

instance PMonad LockM where
  unit x = LockM (return x)
  bind m k = LockM (unLockM m >>= unLockM . k)

lput :: String -> LockM p p ()
lput = LockM . putStrLn

type family Get n p
type instance Get Zero (Lcons e p) = e
type instance Get (Succ n) (Lcons e p) = Get n p

type family Set n e' p
type instance Set Zero e' (Lcons e p) = Lcons e' p
type instance Set (Succ n) e' (Lcons e p) = Lcons e (Set n e' p)

newtype Lock n = Lock Int deriving Show

mkLock :: forall n. Nat n => Lock n
mkLock = Lock (toInt (undefined :: n))

lock0 :: Lock Zero
lock0 = mkLock

lock1 :: Lock One
lock1 = mkLock

lock2 :: Lock Two
lock2 = mkLock

acquire :: (Get n p ~ Unlocked) => Lock n -> LockM p (Set n Locked p) ()
acquire l = LockM (putStrLn ("acquire " ++ show l))

release :: (Get n p ~ Locked) => Lock n -> LockM p (Set n Unlocked p) ()
release l = LockM (putStrLn ("release " ++ show l))

type ThreeLocks = Lcons Unlocked (Lcons Unlocked (Lcons Unlocked Lnil))

-- | Try:
--
-- > > run3 $ withLock lock2 (lput "hello")
-- > acquire Lock 2
-- > hello
-- > release Lock 2
--
run3 :: LockM ThreeLocks ThreeLocks a -> IO a
run3 = unLockM

with1 a = withLock lock1

withLock :: (Get n p ~ Unlocked, Get n q ~ Locked)
            => Lock n
            -> LockM (Set n Locked p) q b
            -> LockM p (Set n Unlocked q) b
withLock lck a =
  acquire lck `bind` \_ ->
  a `bind` \x ->
  release lck `bind` \_ ->
  unit x

critical1 :: (Get One p ~ Locked) => LockM p p ()
critical1 = LockM (putStrLn "Critical section 1")

--
-- Appendix
--

data D m = MakeD (m Int)

type family T2 a :: *
-- f1 :: D T2          -- Illegal (unsaturated)
-- f1 = undefined

type family S a :: * -> *
f2 :: D (S a)
f2 = undefined

type family R a b :: *
-- f3 :: D (R a)      -- Illegal (unsaturated)
-- f3 = undefined

class C' a where
  type F' a :: *
  inj :: a -> F' a
  prj :: F' a -> a

-- bar :: C' a => F a -> F a
bar x = inj (prj x)   -- Should show error, but not.

instance C' Int where
  type F' Int = Int
  inj = id
  prj = id

instance C' Char where
  type F' Char = Int
  inj _ = 0
  prj _ = 'a'

data I
type family TApply f x
type instance TApply I x = x
type instance TApply (a->c) x = a -> TApply c x

class FCompose a where
  type Result a :: *
  (+^):: (String -> a) -> (String -> b) -> (String -> TApply (Result a) b)

instance FCompose String where
  type Result String = I
  f1 +^ f2 = \s -> f2 (f1 s)

instance FCompose c => FCompose (a->c) where
  type Result (a->c) = a -> Result c
  f1 +^ f2 = \s -> \x -> ((\s -> f1 s x) +^ f2) s

lit2 :: String -> (String -> String)
lit2 str = \s -> s ++ str

int2 :: String -> Int -> String
int2 = \s -> \x -> s ++ show x

sprintf2 :: (String -> t) -> t
sprintf2 fmt = fmt ""

f2_ld :: String -> String
f2_ld = lit2 "day"

f2_lds :: String -> String
f2_lds = lit2 "day" +^ lit2 "s"

f2_dn :: String -> Int -> String
f2_dn = lit2 "day" +^ int2

f2_nds :: String -> Int -> String
f2_nds = int2 +^ lit2 " day" +^ lit2 "s"