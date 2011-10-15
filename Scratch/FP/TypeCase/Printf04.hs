{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Variation of Printf, take 4.
-}
module Printf04 where

data Printf t where
  Lit :: String -> Printf t -> Printf t
  Eol :: Printf t -> Printf t
  Int :: Printf t -> Printf (Int->t)
  Str :: Printf t -> Printf (String->t)
  Eod :: Printf String

-- To add a new formatter, say 'Chr', we need to modify above data type.
-- This modification will not occur in Printf03, adding new format does not
-- affect exisiting data type, thus adding new format in different module,
-- different package would be also possible.

-- | Showing string representation similar to printf function syntax in c.
instance Show (Printf t) where
  show t = case t of
    Lit x a -> x ++ show a
    Eol a -> "\n" ++ show a
    Int a -> "%d" ++ show a
    Str a -> "%s" ++ show a
    Eod -> ""

printfApp :: Printf t -> String -> t
printfApp t s = case t of
  Lit x k -> printfApp k (s++x)
  Eol k -> printfApp k (s++"\n")
  Int k -> \x -> printfApp k (s++show x)
  Str k -> \x -> printfApp k (s++x)
  Eod -> s

printf :: Printf t -> t
printf p = printfApp p ""

p04 :: Printf (Int -> Int -> String -> String)
p04 = Int . Lit " bananas and " . Int . Lit " " . Str . Lit "." $ Eod

-- ghci> p04
-- %d bananas and %d %s.

p04_show = printf p04 8 12 "dogs"
