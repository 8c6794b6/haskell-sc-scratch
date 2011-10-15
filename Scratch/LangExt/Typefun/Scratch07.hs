{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
From /Appendix C. Sprintf revisited/.

-}
module Scratch07 where

lit :: String -> (String -> String)
lit str = \s -> s ++ str

int :: String -> (Int -> String)
int s = \x -> s ++ show x

data I

class FCompose a where
  type Result a
  (<>) :: (String -> a) -> (String -> b) -> (String -> TApply (Result a) b)

instance FCompose String where
  type Result String = I
  f1 <> f2 = \s -> f2 (f1 s)

instance FCompose c => FCompose (a->c) where
  type Result (a->c) = a -> Result c
  f1 <> f2 = \s -> \x -> ((\y -> f1 y x) <> f2) s

type family TApply f x
type instance TApply I x = x
type instance TApply (a->b) x = a -> TApply b x

sprintf :: (String -> t) -> t
sprintf = ($ "")

{-

ghci> sprintf (int <> lit " apples and " <> int <> lit " bananas.") 8 3
"8 apples and 3 bananas."

-}