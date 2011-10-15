{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Variation of Printf, take 3. BTW, its Haskell98.
-}
module Printf03 where

class Format f where
  lit :: String -> f r -> f r
  eol :: f r -> f r
  int :: f r -> f (Int->r)
  str :: f r -> f (String->r)
  eod :: f String

newtype Printf t = Printf {printfApp :: String -> t}

instance Format Printf where
  lit x k = Printf $ \s -> printfApp k (s++x)
  eol k = Printf $ \s -> printfApp k (s++"\n")
  int k = Printf $ \s x -> printfApp k (s++show x)
  str k = Printf $ \s x -> printfApp k (s++x)
  eod = Printf id

printf :: Printf t -> t
printf p = printfApp p ""

p03 :: Format f => f (Int -> Int -> String -> String)
p03 = int . lit " bananas and " . int . lit " " . str . lit "." $ eod

p03_show :: String
p03_show = printf p03 3 5 "oranges"

-- ghci> p03_show
-- "3 bananas and 5 oranges."

------------------------------------------------------------------------------
-- Viewing the syntax

newtype ViewF t = ViewF {unViewF :: String}

instance Show (ViewF t) where
  show = unViewF

viewF :: ViewF t -> ViewF t
viewF = id

instance Format ViewF where
  lit x k = ViewF $ x ++ unViewF k
  eol k = ViewF $ "\n" ++ unViewF k
  int k = ViewF $ "%d" ++ unViewF k
  str k = ViewF $ "%s" ++ unViewF k
  eod = ViewF ""

p03_view :: ViewF (Int -> Int -> String -> String)
p03_view = viewF p03

-- ghci> p03_view
-- %d bananas and %d %s.

------------------------------------------------------------------------------
-- Adding a new formatter, char and double

class FormatExt01 f where
  char   :: f r -> f (Char->r)
  double :: f r -> f (Double->r)

instance FormatExt01 Printf where
  char k = Printf $ \s x -> printfApp k (s++[x])
  double k = Printf $ \s x -> printfApp k (s++show x)

instance FormatExt01 ViewF where
  char k = ViewF $ "%c" ++ unViewF k
  double k = ViewF $ "%f" ++ unViewF k

p03b :: (FormatExt01 f, Format f)
        => f (Int -> String -> Double -> String -> String)
p03b =
  lit "sum of " . int . lit " " . str . lit " was " .
  double . lit " " . str . lit "." $ eod

p03b_show :: String
p03b_show = printf p03b 3 "cats" 100 "kg"
