{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/Intro2.hs>
--
module Intro2 where

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
  
instance ExpSYM Int where  
  lit n = n
  neg e = - e
  add e1 e2 = e1 + e2
  
instance ExpSYM String where  
  lit n = show n
  neg e = "(-" ++ e ++ ")"
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"
  
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))  

eval :: Int -> Int
eval = id

view :: String -> String
view = id

tfl1 = [lit 1, add (lit 1) (lit 3), tf1]