{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- The goal is to define a data type by cases, where one can add new cases to
-- the data type and new functions over the data type, without recompiling
-- existing code, and while retaining static type safety.
--
module Pearl.DataTypesALaCarte where

data Expr f = In (f (Expr f))

data Val e = Val Int
data Add e = Add e e

data (f :+: g) e = Inl (f e) | Inr (g e)

infixr 2 :+:

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

(+@) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x +@ y = inject (Add x y)
infixl 6 +@

e1 :: Expr (Add :+: Val)
e1 = val 30000 +@ val 1330 +@ val 7

data Mul x = Mul x x
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 *@

(*@) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x *@ y = inject (Mul x y)

e2 :: Expr (Val :+: Add :+: Mul)
e2 = val 80 *@ val 5 +@ val 4

e3 :: Expr (Val :+: Mul)
e3 = val 6 *@ val 7

class Render f where
  render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val x) = show x

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x

data Term f a
  = Pure a
  | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Monad (Term f) where
  return x = Pure x
  (Pure x) >>= f   = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)

data Incr t = Incr Int t
newtype Recall t = Recall (Int -> t)

instance Functor Incr where
  fmap f (Incr i t) = Incr i (f t)

instance Functor Recall where
  fmap f (Recall g) = Recall (f . g)

inject2 :: (g :<: f) => g (Term f a) -> Term f a
inject2 = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject2 (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = inject2 (Recall Pure)

tick :: Term (Recall :+: Incr) Int
tick = recall >>= \y -> incr 1 >> return y

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x)   = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

newtype Mem = Mem Int deriving (Eq, Show)

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a,Mem)) -> (Mem -> (a,Mem))

instance Run Incr where
  runAlgebra (Incr k r) = \(Mem i) -> r (Mem (i+k))

instance Run Recall where
  runAlgebra (Recall r) = \(Mem i) -> r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- | Try:
--
-- > > run tick (Mem 4)
--
run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra

data Teletype a
  = GetChar (Char -> a)
  | PutChar Char a

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a

instance Functor Teletype where
  fmap f (GetChar g)    = GetChar (f . g)
  fmap f (PutChar c a) = PutChar c (f a)

instance Functor FileSystem where
  fmap f (ReadFile fp g) = ReadFile fp (f . g)
  fmap f (WriteFile fp str a) = WriteFile fp str (f a)

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f)    = getChar >>= f
  execAlgebra (PutChar c io) = putChar c >> io

instance Exec FileSystem where
  execAlgebra (ReadFile fp g) = readFile fp >>= g
  execAlgebra (WriteFile fp str a) = writeFile fp str >> a

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl x) = execAlgebra x
  execAlgebra (Inr x) = execAlgebra x

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

getCharT :: (Teletype :<: f) => Term f Char
getCharT = inject2 (GetChar Pure)

putCharT :: (Teletype :<: f) => Char -> Term f ()
putCharT c = inject2 (PutChar c (Pure ()))

readFileT :: (FileSystem :<: f) => FilePath -> Term f String
readFileT fp = inject2 (ReadFile fp Pure)

writeFileT :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFileT fp str = inject2 (WriteFile fp str (Pure ()))

cat :: FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
  contents <- readFileT fp
  mapM putCharT contents
  return ()

class Render2 f where
  render2 :: (Show a, Render2 g) => f (Term g a) -> String

pretty2 :: (Render2 f, Show a) => Term f a -> String
pretty2 (Impure t) = render2 t
pretty2 (Pure t) = show t

instance Render2 Teletype where
  render2 (GetChar f) = "GetChar"
  render2 (PutChar c _) = "PutChar '" ++ [c] ++ "'"

instance Render2 FileSystem where
  render2 (ReadFile fp _) = "ReadFile " ++ fp
  render2 (WriteFile fp str _) = "WriteFile " ++ fp ++ " " ++ str

instance (Render2 f, Render2 g) => Render2 (f :+: g) where
  render2 (Inl x) = render2 x
  render2 (Inr x) = render2 x
