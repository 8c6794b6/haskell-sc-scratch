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
import Sound.SC3.UGen.MCE

ppug :: UGen -> Doc
ppug ug = case ug of
    Constant_U (Constant v) -> float v
    Control_U (Control r n f t) ->
      text "Control" <+>
      (nest 2 $ pprt r <+>
       text n <+>
       float f <+>
       text "triggered:" <+>
       text (show t))
    Primitive_U (Primitive r n is os s ui) ->
        text n <+> pprt r $+$
        (nest 2 $
         brackets (sep (punctuate comma $ map ppug is)) <+>
         brackets (sep (punctuate comma $ map pprt os)) <+>
         ppsp s <+>
         text (show ui))
    Proxy_U (Proxy pr _) -> text "Proxy" <+> ppug (Primitive_U pr)
    MCE_U m         -> case m of
        MCE_Unit ug'   -> ppug ug'
        MCE_Vector ugs -> text "MCE" <+> sep (map ppug ugs)
    MRG_U (MRG l r) -> text "MRG" <+> ppug l $+$ ppug r
    Label_U (Label str) -> text "Label" <+> text str


pprt :: Rate -> Doc
pprt r = case r of
  AR -> text "AR"
  KR -> text "KR"
  IR -> text "IR"
  DR -> text "DR"

ppsp :: Special -> Doc
ppsp (Special i) = text "Special" <+> int i
