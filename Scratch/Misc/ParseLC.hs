{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable
-}

module ParseLC where

import Data.Attoparsec

data LC
  = Fun (LC -> LC)
  | Aapp LC LC
  | TBool Bool
  | TChar Char
  | TInt Int
  | TDouble Double
  | TList LC

e01 = "3"
e02 = "true"
e03 = "\\x -> x"
