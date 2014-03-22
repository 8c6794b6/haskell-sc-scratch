module Main where

import Data.Binary (decode, encode)
import Control.Applicative (Applicative(..), (<$>))

import Sound.SC3
import Sound.SC3.Lepton
import Test.QuickCheck

main = quickCheck prop_s_is_s

prop_s_is_s :: Property
prop_s_is_s =
  forAll intP $ \x ->
  forAll doubleP $ \y ->
  let expr =
        ppar [ ptakeT y pat1, pdropT y pat2
             , pmerge pat1 (pconcat [pat1,pat2]) ]

      pat1 = psnew "foo" Nothing AddToTail 1 [("bar",param1)]
      pat2 = pnset 1000 [("buzz",param2),("quux",param3)]
      pat3 = plam (ttoosc tdouble) (pmerge pz pz) `papp`
             psnew "blah" Nothing AddToTail 1 [("bloh", param4)]
      param1 = body1 `papp` x `papp` y'
      body1 =
        plam tint (plam tdouble (
         let a = ps pz; b = pz in
         pcycle [preplicate (prand a [a, a+!pint 2, a*!pint 3]) b]))
      y' = pdrange y (y *@ pdouble 2)
      param2 = pforever (pseq x [pappend y y])
      param3 = plam (ttup tdouble tdouble) body3 `papp` pzip y y
      body3 = pshuffle [pfst pz, pfsm [0] [(psnd pz, [0])]]
      param4 = y

      d = duplicate expr
      s = view (dupl d)
      et = let et' = etree (dupr d) in et' `seq` et'
      pet = ppTree et
      e1 = fromTree (decode $ encode $ et,())
      e2 = t2s et
      e3 = e2s (dupr d)
      l1 = t2l et
      l2 = e2l (dupr d)
      isRight r = pet `seq` case r of Right _ -> True; _ -> False

  in  collect (etSize et) $ case e1 of
    Left err          -> False
    Right (Term _ e') ->
      view e' == s && e2 == s && e3 == s && isRight l1 && isRight l2

ap2 :: (a -> a -> c) -> a -> c
ap2 f = \x -> f x x

intP :: Pint p => Gen (p h Int)
intP =
  elements
  [ ap2 (+!), ap2 (*!), ap2 (-!)
  , piabs, pinegate, pisignum, ap2 pirange
  ] <*> (pint <$> arbitrary)

doubleP :: Pdouble p => Gen (p h Double)
doubleP =
  elements
  [ ap2 (+@), ap2 (*@), ap2 (-@), ap2 pdrange
  , pdabs, pdnegate, pdsignum, ap2 (/@), precip, psqrt, plog
  , psin, ptan, pcos, pasin, patan, pacos, psinh, ptanh, pcosh
  , pasinh, patanh, pacosh, pampDb, pasFloat, pasInt, pbitNot
  , pcpsMIDI, pcpsOct, pcubed, pdbAmp, pdistort, pfrac, pisNil
  , plog10, plog2, pmidiCPS, pmidiRatio, pnotE, pnotNil, poctCPS
  , pramp_, pratioMIDI, psoftClip, psquared, const ppi
  , ap2 (**@), ap2 plogBase
  ] <*> (pdouble <$> arbitrary)
