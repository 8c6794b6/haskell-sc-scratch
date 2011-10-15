{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/Incope.hs>
Renamed module to ICP to avoid confusion.

-}
module ICP where

import Text.Show.Functions

class Symantics repr where
  int  :: Int -> repr Int
  bool :: Bool -> repr Bool

  lam :: (repr a -> repr b) -> repr (a->b)
  app :: repr (a->b) -> repr a -> repr b
  fix :: (repr a -> repr a) -> repr a

  add :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  leq :: repr Int -> repr Int -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a

test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)
test3 () = lam (\x -> add (app x (int 1)) (int 2))

testgib () =
  lam (\x -> lam (\y ->
    fix (\self -> lam (\n ->
      if_ (leq n (int 0)) x
        (if_ (leq n (int 1)) y
           (add (app self (add n (int (-1))))
                (app self (add n (int (-2))))))))))

testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
testgib2 () =
  lam (\x -> (lam (\y -> app (app (app (testgib ()) x) y) (int 5))))

testpowfix () =
  lam (\x ->
    fix (\self ->
      lam (\n ->
        if_ (leq n (int 0)) (int 1)
            (mul x (app self (add n (int (-1))))))))

testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 7))
testpowfix72 () = app (testpowfix7 ()) (int 2)

------------------------------------------------------------------------------
-- Interpreter to evaluate

newtype R a = R a

unR :: R a -> a
unR (R a) = a

comp :: R a -> a
comp = unR

instance Symantics R where
  int x = R x
  bool x = R x

  lam f = R $ \x -> unR (f (R x))
  app e1 e2 = R $ (unR e1) (unR e2)
  fix e = R $ let g f = f (g f) in unR (g e)

  add e1 e2 = R $ unR e1 + unR e2
  mul e1 e2 = R $ unR e1 * unR e2
  leq e1 e2 = R $ unR e1 <= unR e2
  if_ e1 e2 e3 = R $ if unR e1 then unR e2 else unR e3

------------------------------------------------------------------------------
-- Interpreter to view

newtype S a = S (Int -> String)

unS :: S a -> (Int -> String)
unS (S f) = f

view :: S a -> String
view e = unS e 0

toS :: S a -> S a
toS = id

instance Show (S a) where
  show = view

instance Symantics S where
  int x = S $ \_ -> "int " ++ if x < 0 then "(" ++ show x ++ ")" else show x
  bool x = S $ \_ -> "bool " ++ show x

  lam f = S $ \h ->
    let x = "x" ++ show h
    in  concat ["lam (\\",x," -> ",unS (f (S (const x))) (succ h),")"]
  app e1 e2 = S $ \h -> concat ["app (",unS e1 h,") (",unS e2 h,")"]
  fix f = S $ \h ->
    let x = "f" ++ show h
    in  concat ["fix (\\",x," -> ", unS (f (S (const x))) (succ h),")"]

  add e1 e2 = S $ \h -> concat ["add (",unS e1 h,") (",unS e2 h,")"]
  mul e1 e2 = S $ \h -> concat ["mul (",unS e1 h,") (",unS e2 h,")"]
  leq e1 e2 = S $ \h -> concat ["leq (",unS e1 h,") (",unS e2 h,")"]
  if_ e1 e2 e3 = S $ \h ->
    concat ["if_ (",unS e1 h,") (",unS e2 h,") (",unS e3 h,")"]

------------------------------------------------------------------------------
-- Interpreter to get size of term

newtype L a = L Int

unL :: L a -> Int
unL (L i) = i

tsize :: L a -> Int
tsize = unL

instance Symantics L where
  int x = L 1
  bool x = L 1

  lam f = L $ unL (f (L 0)) + 1
  app e1 e2 = L $ unL e1 + unL e2 + 1
  fix f = L $ unL (f (L 0)) + 1

  add e1 e2 = L $ unL e1 + unL e2 + 1
  mul e1 e2 = L $ unL e1 + unL e2 + 1
  leq e1 e2 = L $ unL e1 + unL e2 + 1
  if_ e1 e2 e3 = L $ unL e1 + unL e2 + unL e3 + 1

------------------------------------------------------------------------------
-- Partial evaluater

-- data P a where
--   VI :: Int -> P Int
--   VB :: Bool -> P Bool
--   VF :: (P a -> P b) -> P (a->b)
--   E  :: W a -> P a

-- newtype W a = W { unW :: forall r. Symantics r => r a }

-- instance Symantics W where
--   int x = W $ int x
--   bool x = W $ bool x
--   -- XXX: how can we define lam?
--   -- lam f = W $ lam (\x -> unW $ (f (W x)))

-- abstr :: forall r t. Symantics r => P t -> r t
-- abstr e = case e of
--   VI i -> int i
--   VB b -> bool b
--   VF f -> lam (abstr . f . E)
--   E x  -> x
