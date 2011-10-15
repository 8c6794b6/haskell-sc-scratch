{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Variation of Printf, take 1.
-}

lit :: String -> (String -> a) -> String -> a
lit x k s = k (s++x)

eol :: (String -> a) -> String -> a
eol k s = k (s ++ "\n")

int :: (String -> a) -> String -> Int -> a
int k s x = k (s ++ show x)

str :: (String -> a) -> String -> String -> a
str k s x = k (s++x)

eod :: String -> String
eod = id

p01 :: Int -> Int -> String -> String
p01 = (int . lit " bananas and " . int . lit " " . str . lit "." $ eod) ""

p01_show :: String
p01_show = p01 3 5 "oranges"