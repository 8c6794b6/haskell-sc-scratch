{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
From /4 Typed sprintf and sscanf/.

-}
module Scratch04 where

{-
The typing puzzle is that we want the following to be true:

sprintf "Name=%s"         :: String -> String
sprintf "Age=%d"          :: Int -> String
sprintf "Name=%s, Age=%d" :: String -> Int -> String

-}

data F f where
  Lit :: String -> F L
  Val :: Parser val -> Printer val -> F (V val)
  Cmp :: F f1 -> F f2 -> F (C f1 f2)

data L
data V val
data C f1 f2

type Parser a  = String -> [(a,String)]
type Printer a = a -> String

int :: F (V Int)
int = Val (\x -> reads x :: [(Int,String)]) show

lit :: String -> F L
lit = Lit

(<>) :: F a -> F b -> F (C a b)
(<>) = Cmp

infixr 4 <>

f_ld :: F L
f_ld = lit "day"

f_lds :: F (C L L)
f_lds = lit "day" <> lit "s"

f_dn :: F (C L (V Int))
f_dn = lit "day " <>  int

f_nds = Cmp int (Cmp (Lit " day") (Lit "s"))

f_nds' = int <> lit " day" <> lit "s"

type SPrintf f = TPrinter f String
type family TPrinter f x
type instance TPrinter L x = x
type instance TPrinter (V val) x = val -> x
type instance TPrinter (C f1 f2) x = TPrinter f1 (TPrinter f2 x)

{-|
ghci> sprintf f_ld
"day"
-}
sprintf :: F f -> SPrintf f
sprintf p = printer p id

printer :: F f -> (String -> a) -> TPrinter f a
printer f k = case f of
  Lit str   -> k str
  Val _ s   -> \x -> k (s x)
  Cmp f1 f2 -> printer f1 $ \s1 -> printer f2 $ \s2 -> k (s1++s2)

type SScanf f = String -> Maybe (TParser f (), String)

type family TParser f x
type instance TParser L x = x
type instance TParser (V val) x = (x,val)
type instance TParser (C f1 f2) x = TParser f2 (TParser f1 x)

sscanf :: F f -> SScanf f
sscanf fmt inp = parser fmt () inp

parser :: F f -> a -> String -> Maybe (TParser f a, String)
parser f v s = case f of
  Lit str     -> parseLit str v s
  Val reads _ -> parseVal reads v s
  Cmp f1 f2   -> case parser f1 v s of
    Just (v1,s1) -> parser f2 v1 s1
    Nothing      -> Nothing

parseLit :: String -> a -> String -> Maybe (a,String)
parseLit str v s = case prefix str s of
  Nothing -> Nothing
  Just s' -> Just (v, s')

prefix :: String -> String -> Maybe String
prefix xs ys = case go xs ys of (True,zs) -> Just zs; (False,_) -> Nothing
  where
    go as bs = case (as,bs) of
      (c:cs,d:ds) | c == d    -> go cs ds
                  | otherwise -> (False,[])
      ([]  ,[]  ) -> (True,bs)
      ([]  ,_   ) -> (True,bs)
      (_   ,_   ) -> (False,bs)

parseVal :: Parser b -> a -> String -> Maybe ((a,b),String)
parseVal rs v s = case rs s of
  [(v',s')] -> Just ((v,v'),s')
  _         -> Nothing

newtype Dollars = MkD Int deriving (Show)

dollars :: F (V Dollars)
dollars = Val read_dol show_dol where
  read_dol ('$':s) = [(MkD d, s) | (d,s) <- reads s]
  read_dol _ = []
  show_dol (MkD d) = '$':show d
