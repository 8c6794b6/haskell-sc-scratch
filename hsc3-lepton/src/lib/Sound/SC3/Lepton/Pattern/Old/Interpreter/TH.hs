{-# LANGUAGE TemplateHaskell #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Data type to duplicate expression.


-}
module Sound.SC3.Lepton.Pattern.Interpreter.TH where

import Language.Haskell.TH
import Sound.SC3.Lepton.Pattern.Expression

{-

Make instance of pattern classes for Dup data type.

First, class without taking argument, pempty and prandom.

For instance, for class 'Pempty', input will be class name 'Pempty',
desired result is

> instance (Pempty x0, Pempty x1) => Pempty (Dup x0 x1) where
>   pempty = Dup pempty pempty

Type of instanceD is:

> instanceD :: CxtQ -> TypeQ -> [DecQ] -> DecQ

CxtQ is:

> type CxtQ = Q Cxt     -- Defined in Language.Haskell.TH.Lib
> type Cxt = [Pred]     -- Defined in Language.Haskell.TH.Syntax
> data Pred = ClassP Name [Type] | EqualP Type Type

TypeQ is:

> type TypeQ = Q Type   -- Defined in Language.Haskell.TH.Lib

And, DecQ:

> type DecQ = Q Dec     -- Defined in Language.Haskell.TH.Lib

In the body of instance definicion, desired result is:

>   pempty = Dup pempty pempty

Type of valD is:

> valD :: PatQ -> BodyQ -> [DecQ] -> DecQ

Try:

> ppr `fmap` runQ (idup0 ''Show ''Either)
> ppr `fmap` runQ (idup0 ''Read ''Either)

-}

idup0 t c = sequence
  [ idup0' ''Pempty 'pempty t c
  , idup0' ''Prandom 'prandom t c
  ]

idup1 t c = sequence
  [ idup1' ''Pval 'pval t c
  , idup1' ''Prepeat 'prepeat t c
  , idup1' ''Plist 'plist t c
  ]

idup1p t c = sequence
  [ idup1p' ''Pappend 'pappend t c
  , idup1p' ''Preplicate 'preplicate t c
  , idup1p' ''Prange 'prange t c
  ]

idup1ps t c l r = sequence
  [ idup1ps' ''Pconcat 'pconcat t c l r
  , idup1ps' ''Pcycle 'pcycle t c l r
  , idup1ps' ''Pshuffle 'pshuffle t c l r
  ]

idup2pps t c l r = sequence
  [ idup2pps' ''Pseq 'pseq t c l r
  , idup2pps' ''Pchoose 'pchoose t c l r
  , idup2pps' ''Prand 'prand t c l r
  ]

dCxt cname = cxt [classP cname [tyA], classP cname [tyB]]
dTyp cname tcon = [t| $(conT cname) ($(conT tcon) $tyA $tyB) |]

idup0' cname mname tcon con = instanceD (dCxt cname) (dTyp cname tcon) ds
  where
    ds = [valD (varP mname) (normalB bdy) []]
    bdy = [e|$(conE con) $(varE mname) $(varE mname)|]

idup1' cname mname tcon con = instanceD (dCxt cname) (dTyp cname tcon) ds
  where
    ds = [funD mname cls]
    cls = [clause [pA1] (normalB bdy) []]
    bdy = [e| $(conE con) ($(varE mname) $eA1) ($(varE mname) $eA1)|]

idup1p' cname mname tcon con = instanceD (dCxt cname) (dTyp cname tcon) ds
  where
    ds = [funD mname cls]
    cls = [clause [conP con [pA1,pB1], conP con [pA2,pB2]] (normalB bdy) []]
    bdy = [e|$(conE con) ($(varE mname) $eA1 $eA2) ($(varE mname) $eB1 $eB2)|]

idup1ps' cname mname tcon con lf rf =
  instanceD (dCxt cname) (dTyp cname tcon) ds where
    ds = [funD mname cls]
    cls = [clause [pA1] (normalB bdy) []]
    bdy = [| $(conE con)
             ($(varE mname) ($(varE lf) $eA1))
             ($(varE mname) ($(varE rf) $eA1)) |]

idup2pps' cname mname tcon con lf rf =
  instanceD (dCxt cname) (dTyp cname tcon) ds where
    ds = [funD mname cls]
    cls = [clause [conP con [pA1,pA2], pB1] (normalB bdy) []]
    bdy = [| $(conE con)
             ($(varE mname) $eA1 ($(varE lf) $eB1))
             ($(varE mname) $eA2 ($(varE rf) $eB1)) |]

{-
putStrLn $(stringE . show =<< reify 'idup1'')
putStrLn $(stringE . pprint =<< reify 'idup1'')
putStrLn $(stringE . show =<< reify 'idup1')
putStrLn $(stringE . pprint =<< reify 'idup1p')
-}

tyA :: TypeQ
tyA = varT (mkName "a")

tyB :: TypeQ
tyB = varT (mkName "b")

pA = varP (mkName "a")
pB = varP (mkName "b")
pA1 = varP (mkName "a1")
pA2 = varP (mkName "a2")
pB1 = varP (mkName "b1")
pB2 = varP (mkName "b2")

eA = varE (mkName "a")
eB = varE (mkName "b")
eA1 = varE (mkName "a1")
eA2 = varE (mkName "a2")
eB1 = varE (mkName "b1")
eB2 = varE (mkName "b2")
