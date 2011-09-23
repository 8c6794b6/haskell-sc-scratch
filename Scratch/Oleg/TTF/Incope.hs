{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/Incope.hs>

`IN`terpreter, `CO`mpiler, and `P`artial `E`valuater.

-}
module Incope where

------------------------------------------------------------------------------
-- Target language

class Symantics repr where
  int :: Int -> repr Int
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

testgib () = lam (\x -> lam (\y ->
               fix (\self -> lam (\n ->
                 if_ (leq n (int 1)) y
                   (add (app self (add n (int (-1))))
                        (app self (add n (int (-2)))))))))

testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
testgib2 () = lam (\x -> lam (\y -> app (app (app (testgib ()) x) y) (int 5)))

testpowfix () =
  lam (\x ->
    fix (\self -> lam (\n ->
       if_ (leq n (int 0)) (int 1)
         (mul x (app self (add n (int (-1))))))))

testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 7))

------------------------------------------------------------------------------
-- Interpreter to evaluate expression

newtype R a = R a deriving Show
unR (R a) = a

instance Symantics R where
  int x = R x
  bool b = R b
  lam f = R (unR . f . R)
  app e1 e2 = R ((unR e1) (unR e2))
  fix f = R (fix' (unR . f . R)) where fix' f = f (fix' f)
  add e1 e2 = R (unR e1 + unR e2)
  mul e1 e2 = R (unR e1 * unR e2)
  leq e1 e2 = R (unR e1 <= unR e2)
  if_ be et ee = R (if unR be then unR et else unR ee)

compR = unR
mkitest f = compR (f ())

itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest test3
itestgib = mkitest testgib
itestgib1 = mkitest testgib1
itestgib2 = mkitest testgib2
itestpw = mkitest testpowfix
itestpw7 = mkitest testpowfix7
itestpw72 = mkitest (\() -> app (testpowfix7 ()) (int 2))

------------------------------------------------------------------------------
-- Interpreter to give size of expression

newtype L a = L Int deriving Show
unL (L x) = x

instance Symantics L where
  int _ = L 1
  bool _ = L 1
  lam f = L (unL (f (L 0)) + 1)
  app e1 e2 = L (unL e1 + unL e2 + 1)
  fix f = L (unL (f (L 0)) + 1)
  add e1 e2 = L (unL e1 + unL e2 + 1)
  mul e1 e2 = L (unL e1 + unL e2 + 1)
  leq e1 e2 = L (unL e1 + unL e2 + 1)
  if_ be et ee = L (unL be + unL et + unL ee + 1)
  
compL = unL

ltest1 = compL . test1 $ ()
ltest2 = compL . test2 $ ()
ltest3 = compL . test3 $ ()

ltestgib = compL . testgib $ ()
ltestgib1 = compL . testgib1 $ ()
ltestgib2 = compL . testgib2 $ ()

ltestpw = compL . testpowfix $ ()
ltestpw7 = compL . testpowfix7 $ ()
ltestpw72 = compL (app (testpowfix7 ()) (int 2))

------------------------------------------------------------------------------
-- Compiler

data ByteCode t where
  Var :: Int -> ByteCode t 
  Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
  App :: ByteCode (t1->t2) -> ByteCode t1 -> ByteCode t2
  Fix :: Int -> ByteCode t -> ByteCode t
  INT :: Int -> ByteCode Int
  BOOL :: Bool -> ByteCode Bool
  Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
  Mul :: ByteCode Int -> ByteCode Int -> ByteCode Int
  Leq :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
  IF :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
  LIFT :: t -> ByteCode t
  
instance Show (ByteCode t) where  
  show (Var n)= "V" ++ show n
  show (Lam n b) = "(\\V" ++ show n ++ " -> " ++ show b ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Fix n b) = "(fix\\V" ++ show n ++ " " ++ show b ++ ")"
  show (INT n) = show n
  show (BOOL b) = show b
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Leq e1 e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
  show (IF be et ee) = 
    "(if " ++ show be ++ " then " ++ show et ++ " else " ++ show ee ++ ")"
    
newtype C t = C (Int -> (ByteCode t, Int))
unC (C t) vc0 = t vc0

instance Symantics C where
  int x = C (\vc -> (INT x, vc))
  bool b = C (\vc -> (BOOL b, vc))
  lam f = C $ \vc -> 
    let v = vc
        var = C (\vc -> (Var v, vc))
        (body,vc') = unC (f var) (succ vc)
    in  (Lam v body, vc')
  app e1 e2 = C $ \vc -> 
    let (e1b,vc1) = unC e1 vc
        (e2b,vc2) = unC e2 vc1
    in  (App e1b e2b, vc2)
  fix f = C $ \vc ->
    let v = vc
        var = C (\vc -> (Var v, vc))
        (body,vc') = unC (f var) (succ vc)
    in  (Fix v body, vc')
  add e1 e2 = C $ \vc ->
    let (e1b,vc1) = unC e1 vc
        (e2b,vc2) = unC e2 vc1
    in  (Add e1b e2b, vc2)
  mul e1 e2 = C $ \vc ->
    let (e1b,vc1) = unC e1 vc
        (e2b,vc2) = unC e2 vc1
    in  (Mul e1b e2b, vc2)
  leq e1 e2 = C $ \vc ->
    let (e1b,vc1) = unC e1 vc
        (e2b,vc2) = unC e2 vc1
    in  (Leq e1b e2b, vc2)
  if_ be et ee = C $ \vc ->
    let (beb,vc1) = unC be vc
        (etb,vc2) = unC et vc1
        (eeb,vc3) = unC ee vc2
    in  (IF beb etb eeb, vc3)

compC repr = fst $ unC repr 0

ctest1 = compC . test1 $ ()
ctest2 = compC . test2 $ ()
ctest3 = compC . test3 $ ()

ctestgib = compC . testgib $ ()
ctestgib1 = compC . testgib1 $ ()
ctestgib2 = compC . testgib2 $ ()

ctestpw = compC . testpowfix $ ()
ctestpw7 = compC . testpowfix7 $ ()
ctestpw72 = compC (app (testpowfix7 ()) (int 2))

------------------------------------------------------------------------------
-- Partial evaluator

data P1 t = P1 (Maybe (R t)) (C t)

abstr1 :: P1 t -> C t
abstr1 (P1 _ dyn) = dyn

instance Symantics P1 where
  int x = P1 (Just (int x)) (int x)
  bool b = P1 (Just (bool b)) (bool b)
  add (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (add n1 n2))
  add e1 e2 = P1 Nothing (add (abstr1 e1) (abstr1 e2))
  mul (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (mul n1 n2))
  mul e1 e2 = P1 Nothing (mul (abstr1 e1) (abstr1 e2))
  leq (P1 (Just n1) _) (P1 (Just n2) _) = bool (unR (leq n1 n2))
  leq e1 e2 = P1 Nothing (leq (abstr1 e1) (abstr1 e2))
  if_ (P1 (Just s) _) et ef = if unR s then et else ef
  if_ eb et ef = P1 Nothing (if_ (abstr1 eb) (abstr1 et) (abstr1 ef))
  
  -- XXX:
  -- The result of (lam f) must be P1 Nothing _ or P1 (Just _) _. 
  -- Alas, we won't know which until we apply the function 'f' to a 
  -- particular P1 value
  lam = undefined
  app = undefined
  fix = undefined
  
data P t where  
  VI :: Int -> P Int
  VB :: Bool -> P Bool
  VF :: (P a -> P b) -> P (a->b)
  E  :: C t -> P t

abstr :: P t -> C t
abstr (VI i) = int i
abstr (VB b) = bool b
abstr (VF f) = lam (abstr . f . E)
abstr (E t) = t

{-
instance Functor P where
  fmap f = E . fmap f . abstr
-}

instance Symantics P where
  int x = VI x
  bool b = VB b
  
  -- lam :: (repr a -> repr b) -> repr (a->b)
  lam = VF
  -- app :: repr (a->b) -> repr a -> repr b
  app (VF f) ea = f ea
  app (E f) ea = E (app f (abstr ea))
  
  -- fix :: (repr a -> repr a) -> repr a
  -- fix f = f (E (fix (abstr . f . E)))
  fix f = pfix f
  
  add (VI 0) e = e
  add e (VI 0) = e
  add (VI n1) (VI n2) = VI (n1+n2)
  add e1 e2 = E (add (abstr e1) (abstr e2))
  
  mul e@(VI 0) _ = e
  mul _ e@(VI 0) = e
  mul (VI 1) e = e
  mul e (VI 1) = e
  mul e1 e2 = E (mul (abstr e1) (abstr e2))
  
  leq (VI n1) (VI n2) = VB (n1 <= n2)
  leq e1 e2 = E (leq (abstr e1) (abstr e2))
  
  if_ (VB b1) et ee = if b1 then et else ee
  if_ be      et ee = E (if_ (abstr be) (abstr et) (abstr ee))
  
pfix :: forall a. (P a -> P a) -> P a  
pfix f = res where
  res :: P a
  res = case f res of
    E _  -> E (fix (abstr . f . E))
    VF g -> VF $ \x -> case x of
      E cde -> E (app (fix (abstr . f . E)) cde)
      x     -> g x
      
compP = compC . abstr      

ptest1 = compP . test1 $ ()
ptest2 = compP . test2 $ ()
ptest3 = compP . test3 $ ()

ptestgib = compP . testgib $ ()
ptestgib1 = compP . testgib1 $ ()
ptestgib2 = compP . testgib2 $ ()

ptestpw = compP . testpowfix $ ()
ptestpw7 = compP . testpowfix7 $ ()
ptestpw72 = compP (app (testpowfix7 ()) (int 2))

------------------------------------------------------------------------------
-- A partial evaluator that does not use GADTs

-- XXX: TBW

------------------------------------------------------------------------------
-- The HOAS bytecode compiler
--

data HByteCode t where
  HVar :: t -> HByteCode t
  HLam :: (HByteCode t1 -> HByteCode t2) -> HByteCode (t1->t2)
  HApp :: HByteCode (t1->t2) -> HByteCode t1 -> HByteCode t2
  