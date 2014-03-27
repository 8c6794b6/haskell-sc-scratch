{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Tests for querying SCNode.

-}
module Test.Sound.SC3.Tree.Query where

import Test.HUnit
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Sound.SC3.Tree.Nd
import Sound.SC3.Tree.Type
import Sound.SC3.Tree.Query

-- Sample node from example.

nodes :: Nd
nodes =
  grp 0
    [grp 1
      [grp 10
       [mod1, mod2]
      ,grp 11
       [bar1, bar2]]]

mod1, mod2, bar1, bar2 :: Nd
mod1 = syn "foo" ["out"*=100, "amp"*=100, "freq"*=0.66]
mod2 = syn "foo" ["out"*=101, "amp"*=80, "freq"*=3.33]
bar1 = syn "bar" ["amp"*=0.1, "pan"*=0.75, "freq"*=220, "fmod"*<-mod1-*"out"]
bar2 = syn "bar" ["amp"*=0.1, "pan"*=(-0.75), "freq"*=330, "fmod"*<-mod2-*"out"]

nodes' :: SCNode
nodes' = nodify nodes

case_group_10 :: Assertion
case_group_10 = do
  let ns = queryN (nodeId ==? 10) nodes'
  length ns @?= 1

case_param_fmod :: Assertion
case_param_fmod = do
  let ns = queryN (params (paramName ==? "fmod")) nodes'
  length ns @?= 2

tests :: TestTree
tests = $testGroupGenerator
