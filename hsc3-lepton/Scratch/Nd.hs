{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch for removing manual node id book keeping, take 1.

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

Suppose that, in our desired syntax, we can write something like:

> nodes =
>   let foo = syn "foo" ["out" *= dval 10, "freq" *= cbus 100, "amp" *= dval 1]
>       bar = syn "bar" ["out" *= dval 1, "a_in" *<= getp "out" foo]
>   in  grp 0
>        [grp 1
>          [foo, bar]]

Note that bar is referring to value of foo with

> getp "out" foo

Applying a function, say 'toNode' will convert above to SCNode:

> toNode nodes

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
module Scratch.Nd where

import Sound.SC3
import Sound.SC3.Lepton.Tree

data Nd = Grp Int [Nd] | Syn (Maybe Int) String [Prm] deriving Show

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

getp :: String -> Nd -> PrmVal
getp k n = case n of
  Syn _ def xs -> go xs where
    go ys = case ys of
      ((Prm k' prm):zs) | k == k' -> prm | otherwise -> go zs
      _ -> error $ def ++ " does not have " ++ k
  _ -> error "getp not work for Grp"

(*=),(*<=),(*<-) :: String -> PrmVal -> Prm
k *=  v = Prm k v
k *<= v = Prm k (abus v)
k *<- v = Prm k (cbus v)

cbus :: PrmVal -> PrmVal
cbus p = case p of
  Dval x -> Cbus (Ival (ceiling x))
  Ival x -> Cbus p
  Abus x -> cbus x
  Cbus _ -> p

abus :: PrmVal -> PrmVal
abus p = case p of
  Dval x -> Abus (Ival (ceiling x))
  Ival x -> Abus p
  Abus _ -> p
  Cbus x -> abus x

nodify :: Nd -> SCNode
nodify n = go 0 n where
  go i n = case n of
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

{-| Sample node.

> *Nd> putStrLn $ drawSCNode $ nodify nodes
> 0 group
>    1 group
>       1000 foo
>         out: 10.0 freq: c100 amp: 1.0
>       1001 bar
>         out: 1.0 a_in: a10

-}
nodes :: Nd
nodes =
  let foo = syn "foo" ["out" *= 10, "freq" *<- 100, "amp" *= 1]
      bar = syn "bar" ["out" *= 1, "a_in" *<= getp "out" foo]
  in  grp 0
       [grp 1
         [foo,bar]]

n0 :: Nd
n0 =
  grp 0
    [grp 1
     [grp 10
      [drn1, drn2]]
    ,grp 2
     [huh1rev,snrrev,bellrev]
    ,grp 8
     [syn "cf2mix" -- huh1
      ["out"*=0,"a_in"*<=getp "out" huh1rev,"amp"*=1.6,"pan"*=0]
     ,syn "cf2mix" -- huh2
      ["out"*=0,"a_in"*<=11,"amp"*=1.0,"pan"*=(-0.8)]
     ,syn "cf2mix" -- huh3
      ["out"*=0,"a_in"*<=12,"amp"*=1.0,"pan"*=0.8]
     ,syn "cf2mix" -- kik
      ["out"*=0,"a_in"*<=13,"amp"*=0.8,"pan"*=0.03]
     ,syn "cf2mix" -- snr
      ["out"*=0,"a_in"*<=getp "out" snrrev,"amp"*=0.55,"pan"*=(-0.1)]
     ,syn "cf2mix" -- hat
      ["out"*=0,"a_in"*<=15,"amp"*=0.1,"pan"*=(-0.2)]
     ,syn "cf2mixm" -- pu right
      ["out"*=0,"a_in"*<=16,"amp"*=1]
     ,syn "cf2mixm" -- pu left
      ["out"*=1,"a_in"*<=17,"amp"*=1]
     ,syn "cf2mix"  -- bell
      ["out"*=0,"a_in"*<=getp "out" bellrev,"amp"*=0.8,"pan"*=0.1]
     ,syn "cf2mix" -- drn 1
      ["out"*=0,"a_in"*<=getp "out" drn1,"amp"*=0.9,"pan"*=(-0.25)]
     ,syn "cf2mix" -- drn 2
      ["out"*=0,"a_in"*<=getp "out" drn2,"amp"*=0.9,"pan"*=0.25]]
    ,grp 9
     [syn "cf2mst"
      ["out_l"*=0,"out_r"*=1,"amp"*=1]]]
  where
    drn1 = syn "cf2drn" ["out"*=20,"gate"*=1,"amp"*=0]
    drn2 = syn "cf2drn" ["out"*=22,"gate"*=1,"amp"*=0]
    huh1rev =
      syn "cf2rev"
      ["out"*=10,"a_in"*<=10,"dlyt"*=0.2,"dcyt"*=4.8,"mix"*=0.85]
    snrrev =
      syn "cf2rev"
      ["out"*=14,"a_in"*<=14,"dlyt"*=0.02,"dcyt"*=0.8,"mix"*=0.85]
    bellrev =
      syn "cf2dly"
      ["out"*=18,"a_in"*<=18,"maxdt"*=0.8]

rgGraph =
  grp 1
    [grp 10
       [trg,lzf]
    ,grp 20
       [syn "wn001" ["out"*=0]]
    ,grp 21
       [syn "hit001"
         ["a_in"*<=nzo,"out"*=o21l,"t_trig"*<-getp "out" trg]
       ,syn "rng001"
         ["a_in"*<=o21l,"out_1"*=o21l,"out_2"*=o21r]
       ,syn "cmb001"
         ["a_in"*<=o21l,"out"*=o21l,"t_trig"*<-getp "out" trg,"mix"*=1
         ,"rmin"*=50,"rmax"*=15]
       ,syn "lmt001"
         ["a_in1"*<=o21l,"a_in2"*<=o21r,"out"*=o21l]
       ,syn "pan001"
         ["a_in"*<=o21l,"out"*=o21l,"amp"*=1.2]]
    ,grp 22
       [syn "hit002"
         ["a_in"*<=nzo,"lout"*=o22l,"mout"*=o22m,"hout"*=o22h
         ,"t_trig"*<-getp "out" trg,"lamp"*=14,"mamp"*=3,"hamp"*=2]
       ,syn "pan002"
         ["a_low"*<=o22l,"a_mid"*<=o22m,"a_high"*<=o22h,"out"*=o22l,"amp"*=1]
       ,syn "lmt002"
         ["a_in1"*<=o22l,"a_in2"*<=o22m,"out"*=o22l,"amp"*=1.0]]
    ,grp 23
       [syn "cmb002"
         ["a_in"*<=nzo,"out"*=o23l,"t_trig"*<-getp "out" trg]
       ,syn "pan003"
         ["a_in"*<=o23l,"out"*=o23l,"amp"*=0{-0.0625-}]]
    ,grp 30
       [syn "mix001"
         ["a_l1"*<=o21l,"a_r1"*<=o21r,"a_l2"*<=o22l,"a_r2"*<=o22m,"a_l3"*<=o23l
         ,"a_r3"*<=o23r,"mamp"*=0.8]]]
  where
    trg = syn "tr001" ["out"*=100,"freq"*=6]
    lzf = syn "lzf001" ["out"*=101]
    nzs = syn "wn001" ["out"*=0]
    nzo = getp "out" nzs
    o21l=2; o21r=3;
    o22l=4; o22m=5; o22h=6;
    o23l=8; o23r=9;
