{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Variation of Printf, take 2.
-}
module Printf02 where

newtype Printf t = Printf {printfApp :: String -> t}

lit :: String -> Printf a -> Printf a
lit x k = Printf $ \s -> printfApp k (s++x)

eol :: Printf a -> Printf a
eol k = Printf $ \s -> printfApp k (s ++ "\n")

int :: Printf a -> Printf (Int -> a)
int k = Printf $ \s x -> printfApp k (s ++ show x)

str :: Printf a -> Printf (String -> a)
str k = Printf $ \s x -> printfApp k (s ++ x)

eod :: Printf String
eod = Printf id

printf p = printfApp p ""

p02 :: Printf (Int -> Int -> String -> String)
p02 = int . lit " bananas and " . int . lit " " . str . lit "." $ eod

p02_show = printf p02 10 5 "apple"