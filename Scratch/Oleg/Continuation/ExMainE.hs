{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf>

-}
module ExMainE where

import Control.Applicative
import Control.Monad
import Tutorial

-- Ex3. (1)
--
--   5 * reset (fun () -> [.] + 3 * 4)
--

ex03_01 :: C w Int
ex03_01 = 5 * reset (hole + 3 * 4) where
  hole = shift (\k -> 100)

-- Ex3. (2)
--
--   reset (fun () -> if [.] then "hello" else "hi" ) ^ " world"
--
-- Using helper (++!) and ifm.
--

(++!) = liftM2 (++)
infixr 5 ++!

ifM :: Monad m => m Bool -> m e -> m e -> m e
ifM p t f = if' `liftM` p `ap` t `ap` f where
  if' p t f = if p then t else f

ex03_02 :: C w String
ex03_02 = reset (ifM hole (return "hello") (return "hi")) ++! return " world"
  where
    hole = shift (\_ -> return "boo!")

-- Ex3. (3)
--
--    fst (reset (fun () -> let x = [.] in (x,x)))
--
tupM :: Monad m => m a -> m b -> m (a, b)
tupM = liftM2 (,)

ex03_03 :: C w String
ex03_03 = fst `liftM` reset (let x = hole in tupM x x) where
  hole = shift (\_ -> return ("hi","bye"))

ex03_03' = fst `liftM` reset (do { x <- hole; return (x, x) })
  where hole = shift (\k -> return ("hi","bye"))

-- Ex3. (4)
--
--   string_length (reset (fun () -> "x" ^ string_of_int [.]))
--

-- Need a fixed type signature.
showInt :: Int -> String
showInt = show

ex03_04 :: C w String
ex03_04 = reset (return "x" ++! showInt `liftM` hole) where
  hole = shift (\_ -> return "foooo")

-- Ex4
-- times == Prelude.product
--
times :: Num a => [a] -> a
times xs = case xs of
  []     -> 1
  (y:ys) -> y * times ys

times' :: Num a => [a] -> a
times' xs = go xs id where
  go ys k = case ys of
    []     -> k 1
    (z:zs) -> if z == 0 then k 0 else z * (go zs k)

-- *ExMainE> times [0,1..10^4]
-- 0
-- (0.42 secs, 126906544 bytes)
-- *ExMainE> times' [0,1..10^4]
-- 0
-- (0.06 secs, 18416976 bytes)
--

-- How to write above times' with continuation monad?
--
timesM xs = undefined

naive_print :: Tree -> IO ()
naive_print t = case t of
  Empty -> return ()
  Node l n r -> do
    naive_print l
    print n
    naive_print r