------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module LearnYour.Zipper.WatchYourStep where

import LearnYour.Zipper.Tree
import LearnYour.Zipper.Scratch (Crumb(..), Zipper)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft _                = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight _                = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r: bs)  = Just (Node x t r, bs)
goUp (t, RightCrumb x l: bs) = Just (Node x l t, bs)
goUp (_,_)                   = Nothing

