------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Some code inspired from lisp.
--

import Control.Monad (forM_, foldM_)
import Control.Monad.Fix (mfix)
import Text.Printf (printf)
import Data.List (unfoldr)

-- | Inspired from this common lisp function:
--
-- > (defun nested-dotimes ()
-- >   (dotimes (x 20)
-- >     (dotimes (y 20)
-- >       (format t "~3d " (* (1+ x) (1+ y))))
-- >     (format t "~% ")))
--
-- Almost direct translation of above, using forM_:
--
-- > forM_ [1..20] $ \x -> do
-- >   forM_ [1..20] $ \y -> do
-- >     putStr $ printf "%4d" (x*y :: Int)
-- >   putStr "\n"
--
nestedDo :: IO ()
nestedDo =
  forM_ [1..20] $ \x -> do
    forM_ [1..20] $ \y -> do
      putStr $ printf "%4d" (x*y :: Int)
    putStr "\n"

-- | Using mapM_ instead of forM_. Seems ugly to me.
nestedDo2 :: IO ()
nestedDo2 =
  mapM_ (\x -> do
    mapM_ (\y -> putStr $ printf "%4d" (x*y::Int)) [1..20]
    putStr "\n")
  [1..20]

-- | Construct the target list first, and then split with each 20 elements.
-- Might be less imperative style, though seems verbose.
nestedDo3 :: IO ()
nestedDo3 = g [x*y | x <- [1..20], y <- [1..20]]
  where
    g [] = return ()
    g xs = do
      let (xs', xs'') = splitAt 20 xs
      mapM_ (\x -> putStr $ printf "%4d" (x::Int)) xs'
      putStr "\n"
      g xs''

-- | Using do macro in common lisp:
--
-- > (defun do-example-1 ()
-- >   (do ((n 0 (1+ n))
-- >        (cur 0 next)
-- >        (next 1 (+ cur next)))
-- >       ((= 10 n) cur)))
--
-- Immitating this behaviour with using unfoldr and loop stopping condition.
--
eleventhFib :: Num a => [a]
eleventhFib =
  flip unfoldr (0,0,1) $ \(n,i,j) ->
    if n == 10
      then Nothing
      else Just (i+j, (succ n,j,i+j))

-- | Or using until.
--
eleventhFib2 :: Num a => a
eleventhFib2 = x where
  (_,x,_) = until (\(n,_,_) -> n == 10)
            (\(n,x,y) -> (succ n,y,x+y))
            (0,0,1)


-- | BTW, a famouse way to get fibonnaci sequence
--
fbs :: Num a => [a]
fbs = 0 : 1 : zipWith (+) fbs (tail fbs)

-- | Another example use of do macro:
--
-- > (defun do-example-2 ()
-- >   (do ((i 0 (1+ i)))
-- >        ((>= i 4))
-- >        (print i)))
--
-- This time, it contains a side-effect: print.
-- Building blocks extracted from above code are:
--
-- * Variable definition and optional statement (repeatable): (i 0 (1+ i))
--
-- * End test form: (>= i 4)
--
-- * Body statement: (print i)
--
-- Initial, using unfoldr and sequence_.  From my point of view, this seems to
-- have most resembling structure from the original lisp code.
-- 
doPrint :: IO ()
doPrint = sequence_ $ flip unfoldr 0 $ \x ->
  if x >= 4
    then Nothing
    else Just (print x, succ x)

-- | Variation 2, using foldM.
doPrint' :: IO ()
doPrint' = foldM_ (\_ b -> print b) () $ takeWhile (< 4) (iterate succ 0)

-- | Variation 3, using mfix.
doPrint'' :: IO ()
doPrint'' = mfix $ f 0 where
  f k _ = if k >= 4
            then return ()
            else print k >> f (succ k) undefined
