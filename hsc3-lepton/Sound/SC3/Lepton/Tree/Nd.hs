{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Functions and data types for emoving manual node id book keeping.

Below is a SCNode data used for huh piece.

> n0 :: SCNode
> n0 =
>   g 0
>     [g 1
>      [g 10
>       [s 1000 "lfsin" []
>       ,s 1001 "cf2drn"
>       ["out":=20,"gate":=1, "amp":=0]
>       ,s 1002 "cf2drn"
>       ["out":=22,"gate":=1, "amp":=0]]
>      ]
>     ,g 2
>      [s 2000 "cf2rev" -- huh1
>       ["out":=10,"a_in":<=10,"dlyt":=0.2,"dcyt":=4.8,"mix":=0.85]
>      ,s 2001 "cf2rev" -- snr
>       ["out":=14,"a_in":<=14,"dlyt":=0.02,"dcyt":=0.8,"mix":=0.85]
>      ,s 2002 "cf2dly" -- bell
>       ["out":=18,"a_in":<=18,"maxdt":=0.8]
>      ]
>     ,g 8
>      [s 8000 "cf2mix" -- huh1
>       ["out":=0,"a_in":<=10,"amp":=1.6,"pan":=0]
>      ,s 8001 "cf2mix" -- huh2
>       ["out":=0,"a_in":<=11,"amp":=1.0,"pan":=(-0.8)]
>      ,s 8002 "cf2mix" -- huh3
>       ["out":=0,"a_in":<=12,"amp":=1.0,"pan":=0.8]
>      ,s 8003 "cf2mix" -- kik
>       ["out":=0,"a_in":<=13,"amp":=0.8,"pan":=0.03]
>      ,s 8004 "cf2mix" -- snr
>       ["out":=0,"a_in":<=14,"amp":=0.55,"pan":=(-0.1)]
>      ,s 8005 "cf2mix" -- hat
>       ["out":=0,"a_in":<=15,"amp":=0.1,"pan":=(-0.2)]
>      ,s 8006 "cf2mixm" -- pu right
>       ["out":=0,"a_in":<=16,"amp":=1]
>      ,s 8007 "cf2mixm" -- pu left
>       ["out":=1,"a_in":<=17,"amp":=1]
>      ,s 8008 "cf2mix"  -- bell
>       ["out":=0,"a_in":<=18,"amp":=0.8,"pan":=0.1]
>      ,s 8009 "cf2mix" -- drn 1
>       ["out":=0,"a_in":<=20,"amp":=0.9,"pan":=(-0.25)]
>      ,s 8010 "cf2mix" -- drn 2
>       ["out":=0,"a_in":<=22,"amp":=0.9,"pan":=0.25]
>      ]
>     ,g 9
>      [s 9000 "cf2mst"
>       ["out_l":=0, "out_r":=1, "amp":=1]]]
>   where
>     g = Group
>     s = Synth

Synth nodes in group 8 is referring to synth nodes defined in group 2,
with mapping out parameters to in parameters.

It will be nice if we could mention to particular parameter of synth
node, for later reus.

Also, it will be nice if we can avoid manual book keeping of synth
node id.  It is not always better to have automatic node id
assignment, there could be good enough reason to specify node id
manually.

This scratch is a challenge to accomplish these 2 goals:

  1. To get parameter of specified node

  2. To remove manual book keeping of synth node id

This module enable to write SCNodes in expression like:

> nodes =
>   let foo = syn "foo" ["out"*=10,"freq"*<-100,"amp"*=1]
>       bar = syn "bar" ["out"*=1,"a_in"*<=prmv foo "out"]
>   in  grp 0
>        [grp 1
>          [foo, bar]]

Note the absence of node ids, and bar is referring to value of foo with

> prmv foo "out"

Applying a function 'nodify' will convert above to SCNode:

> nodify nodes

will result in:

> Group 0
>   [Group 1
>     [Synth 1000 "foo" ["out":=10,"freq":<-100,"amp":=1]
>     ,Synth 1001 "bar" ["out":=1,"a_in":<=10]]]


Group nodes need their Id values specified, and Synth nodes could be
written without Node id. When give, the specified value would be used.
Other wise, enumerated value from node id of parent group * 1000 will
be used.

-}
module Sound.SC3.Lepton.Tree.Nd
  ( -- * Types
    Nd(..), Prm(..), PrmVal(..)

    -- * Nd to SCNode Converter
  , nodify

    -- * Builder functions
  , grp, syn, syn', prmv, (*=), (*<-), (*<=), abus, cbus
  ) where

import Sound.SC3.Lepton.Tree.Tree

data Nd
  = Grp Int [Nd]
  | Syn (Maybe Int) String [Prm] deriving Show

data Prm = Prm String PrmVal deriving Show

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