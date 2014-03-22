{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Tests for comparing nodes written with Nd and SCNode.
-}
module Test.Sound.SC3.Lepton.Tree.Nd where

import Test.QuickCheck

import Sound.SC3.Lepton.Tree

tests =
  [ label "nodify" prop_nodify ]

prop_nodify =
  forAll (elements [1..99]) $ \gid ->
  forAll arbitrary $ \(nname1, nname2, pname1, pname2, pname3) ->
  forAll arbitrary $ \(pval1, pval2) ->
  let -- SCNode with synth node id specified manually.
      -- Synth nodes will have id '(parent group id * 1000) + [1,2,3,..]'
      sc =
        Group gid
        [ Synth (gid * 1000) nname1
          [ pname1 := pval1, pname2 :<- pval2 ]
        , Synth (gid * 1000 + 1) nname2
          [ pname3 :<= (ceiling pval1) ]]

      -- Need explicit Ival and Dval wrapping, since arbitrary values
      -- used above are Int and Double.
      nd1 =
        syn nname1
         [ pname1 *= Dval pval1, pname2 *<- Ival pval2 ]
      nd =
        grp gid
        [ nd1
        , syn' (gid * 1000 + 1)  nname2 [pname3 *<= prmv nd1 pname1 ]]

  in  sc == nodify nd
