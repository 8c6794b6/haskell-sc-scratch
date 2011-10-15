{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : portable

Pretty printer for UGen
-}
module Sound.SC3.Lepton.UGen.Pretty where

import Text.PrettyPrint

import Sound.SC3

ppug :: UGen -> Doc
ppug ug = case ug of
  Constant v -> double v
  Control r n d t ->
    text "Control" <+>
    (nest 2 $ pprt r <+>
     text n <+>
     double d <+>
     text "triggered:" <+>
     text (show t))
  Primitive r n is os s ui ->
    text n <+> pprt r $+$
    (nest 2 $
     brackets (sep (punctuate comma $ map ppug is)) <+>
     brackets (sep (punctuate comma $ map pprt os)) <+>
     ppsp s <+>
     text (show ui))
  Proxy ug' _ ->
    text "Proxy" <+> ppug ug'
  MCE ugs ->
    text "MCE" <+> sep (map ppug ugs)
  MRG l r ->
    text "MRG" <+> ppug l $+$ ppug r

pprt :: Rate -> Doc
pprt r = case r of
  AR -> text "AR"
  KR -> text "KR"
  IR -> text "IR"
  DR -> text "DR"

ppsp :: Special -> Doc
ppsp (Special i) = text "Special" <+> int i
