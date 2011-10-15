{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Playing with UnicodeSyntax language pragma.

Most non-ascii symbols are entered in emacs with using TeX input mehtod.

In emacs, hit 'C-\\' or 'M-x set-input-method' and choose TeX mode.
To see available symbols and sequence to type them, see
'M-x describe-input-method'. For instance, '\\lambda' will insert λ .

-}
module Unicode where

f1 :: ∀ a. a → a
f1 α = α
