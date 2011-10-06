{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Module to run TemplateHaskell helpers.
-}
module Scratch.THRun where

import Data.Binary

import Scratch.Etree
import Scratch.THHelper
import Scratch.PC02
import Scratch.Term00
import Scratch.Type00

import Scratch.Parse8

$(ftTypes "FromTree2" "FromTreeList2")

ft :: forall r. (FromTree2 r -> FromTree2 r) -> FromTree2 r
ft self = undefined

ftl :: forall r. FromTreeList2 r
ftl = undefined

ftd :: forall r. FromTree2 r -> FromTree2 r
ftd self (e,g) = case e of
  Node "pdouble" [Leaf x] ->
    return $ Term tdouble (pdouble $ decode x)
  Node name [e1] -> do
    case name of
      "pexp" -> $(dmatch1 'self 'e1 'g 'pexp)
      "plog" -> $(dmatch1 'self 'e1 'g 'plog)
      "pmidiCPS" -> $(dmatch1 'self 'e1 'g 'pmidiCPS)
      _ -> delegate
  Node name [e1,e2] -> do
    case name of
      "+@" -> $(dmatch2 'self 'e1 'e2 'g '(+@))
      "*@" -> $(dmatch2 'self 'e1 'e2 'g '(*@))
      "-@" -> $(dmatch2 'self 'e1 'e2 'g '(-@))
      "plogBase" -> $(dmatch2 'self 'e1 'e2 'g 'plogBase)
      "**@" -> $(dmatch2 'self 'e1 'e2 'g '(**@))
      _ -> delegate
  _ -> delegate
  where delegate = fti self (e,g)

fti :: forall r. FromTree2 r -> FromTree2 r
fti self (e,g) = case e of
  Node name [e1] -> do
    case name of
      "pinegate" -> $(imatch1 'self 'e1 'g 'pinegate)
      "piabs" -> $(imatch1 'self 'e1 'g 'piabs)
      "pisignum" -> $(imatch1 'self 'e1 'g 'pisignum)
  Node name [e1,e2] -> do
    case name of
      "+!" -> $(imatch2 'self 'e1 'e2 'g '(+!))
      "*!" -> $(imatch2 'self 'e1 'e2 'g '(*!))
      "-!" -> $(imatch2 'self 'e1 'e2 'g '(-!))
