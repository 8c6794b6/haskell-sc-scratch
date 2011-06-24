------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with HTML templating in haskell.
--
module Main where

import Text.StringTemplate

main = do
  body <- readFile "body.txt"
  qs <- lines `fmap` readFile "contents.txt"
  let t = newSTMP body :: StringTemplate String
      qs' = zipWith (\i q -> render $
                       setAttribute "question" q $
                       setAttribute "divid" (show i) $
                       t) [19..] qs
  mapM_ putStrLn qs'
