{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Functions and data types for emoving manual node id book keeping.

Synth node /A/ in particular group may refer to synth node /B/ defined in
another group, with mapping @out@ parameters to @in@ parameters.

It will be nice if we could mention to particular parameter of synth
node, for later reuse.

Also, it will be nice if we can avoid manual book keeping of synth
node id.  It is not always better to have automatic node id
assignment, there could be good enough reason to specify node id
manually.

This module provides these 2 features:

  1. Get parameter of specified node

  2. Remove manual book keeping of synth node id

SCNodes could be written in expression like:

> nodes =
>   let foo = syn "foo" ["out"*=10,"freq"*<-100,"amp"*=1]
>       bar = syn "bar" ["out"*=1,"a_in"*<=prmv foo "out"]
>   in  grp 0
>        [grp 1
>          [foo, bar]]

Note the absence of node ids, and bar is referring value of foo with

> prmv foo "out"

Applying a function 'nodify' will convert above to SCNode:

> nodify nodes

will result in:

> Group 0
>   [Group 1
>     [Synth 1000 "foo" ["out":=10,"freq":<-100,"amp":=1]
>     ,Synth 1001 "bar" ["out":=1,"a_in":<=10]]]


Group nodes need their Id values specified, and Synth nodes could be
written without Node id. When node id has given, the specified value
would be used.  Other wise, enumerated value from node id of parent
group * 1000 will be used.

-}
module Sound.SC3.Lepton.Tree.Nd
  ( -- * Types
    Nd(..), Prm(..), PrmVal(..)

    -- * Nd to SCNode Converter
  , nodify

    -- * Builder functions
  , grp, syn, syn', prmv, (*=), (*<-), (*<=), (-*), abus, cbus
  ) where

import Sound.SC3.Lepton.Tree.Tree

data Nd
  = Grp Int [Nd]
  | Syn (Maybe Int) String [Prm]
  deriving (Eq,Show)

data Prm = Prm String PrmVal
  deriving (Eq,Show)

data PrmVal
  = Dval Double
  | Ival Int
  | Cbus PrmVal
  | Abus PrmVal
  deriving (Eq,Show)

grp :: Int -> [Nd] -> Nd
grp = Grp

syn :: String -> [Prm] -> Nd
syn = Syn Nothing

syn' :: Int -> String -> [Prm] -> Nd
syn' i = Syn (Just i)

prmv :: Nd -> String -> PrmVal
prmv n k = case n of
  Syn _ def xs -> go xs where
    go ys = case ys of
      ((Prm k' prm):zs) | k == k' -> prm | otherwise -> go zs
      _ -> error $ def ++ " does not have " ++ k
  _ -> error "prmv not work for Grp"

(*=),(*<=),(*<-) :: String -> PrmVal -> Prm
k *=  v = Prm k v
k *<= v = Prm k (abus v)
k *<- v = Prm k (cbus v)

infixl 5 *=
infixl 5 *<-
infixl 5 *<=

-- | Infix variant of 'prmv'.
(-*) :: Nd -> String -> PrmVal
(-*) = prmv
infixl 6 -*

cbus :: PrmVal -> PrmVal
cbus p = case p of
  Dval x -> Cbus (Ival (ceiling x))
  Ival _ -> Cbus p
  Abus x -> cbus x
  Cbus _ -> p

abus :: PrmVal -> PrmVal
abus p = case p of
  Dval x -> Abus (Ival (ceiling x))
  Ival _ -> Abus p
  Abus _ -> p
  Cbus x -> abus x

nodify :: Nd -> SCNode
nodify n = go 0 n where
  go i m = case m of
    Grp j ns -> Group j (zipWith go [j*1000..] ns)
    Syn Nothing def vs -> Synth i def (map unPrmVal vs)
    Syn (Just j) def vs -> Synth j def (map unPrmVal vs)

unPrmVal :: Prm -> SynthParam
unPrmVal p@(Prm k v) = case v of
  Dval x -> k := x
  Ival x -> k := fromIntegral x
  Cbus (Ival x) -> k :<- x
  Abus (Ival x) -> k :<= x
  _ -> error $ "Undecidable param" ++ show p

instance Num PrmVal where
  (+) = error "+"
  (*) = error "*"
  (-) = error "-"
  abs = error "abs"
  negate n = case n of
    Abus _ -> error "negate used for Abus"
    Cbus _ -> error "negate used for Cbus"
    Dval x -> Dval (negate x)
    Ival x -> Ival (negate x)
  signum = error "signum"
  fromInteger = Dval . fromInteger

instance Fractional PrmVal where
  (/) = error "/"
  recip = error "recip"
  fromRational = Dval . fromRational

{-

Sample node.

> *Nd> putStrLn $ drawSCNode $ nodify nodes
> 0 group
>    1 group
>       1000 foo
>         out: 10.0 freq: c100 amp: 1.0
>       1001 bar
>         out: 1.0 a_in: a10

nodes :: Nd
nodes =
  let foo = syn "foo" ["out"*=10, "freq"*<-100, "amp"*=1]
      bar = syn "bar" ["out"*=1, "a_in"*<=prmv foo "out"]
  in  grp 0
       [grp 1
         [foo,bar]]

-}
