{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (DeriveDataTypeable, StandaloneDeriving)
--
-- Additional instance declarations for datat types found in Sound.SC3.
--
-- Currently OSC and UGen are instantiated from Data.Typeable, Data.Data.
--
module Sound.SC3.Lepton.Instance () where

import Data.Data

import Sound.SC3
import Sound.OpenSoundControl

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 700
deriving instance Data Datum
deriving instance Typeable Datum

deriving instance Data OSC
deriving instance Typeable OSC

deriving instance Data Time
deriving instance Typeable Time

deriving instance Data Rate
deriving instance Typeable Rate

deriving instance Data Special
deriving instance Typeable Special

deriving instance Data UGen
deriving instance Typeable UGen
#else
instance Typeable Datum where
  typeOf _  = mkTyConApp tc_Datum []

tc_Datum :: TyCon
tc_Datum = mkTyCon "Sound.OpenSoundControl.Datum"

instance Data Datum where
  gfoldl k z (Int i) = z Int `k` i
  gfoldl k z (Float f) = z Float `k` f
  gfoldl k z (Double d) = z Double `k` d
  gfoldl k z (String s) = z String `k` s
  gfoldl k z (Blob b) = z Blob `k` b
  gfoldl k z (TimeStamp t) = z TimeStamp `k` t
  gfoldl k z (Midi m) = z Midi `k` m

  gunfold k z c = case constrIndex c of
                    1 -> k (z Int)
                    2 -> k (z Float)
                    3 -> k (z Double)
                    4 -> k (z String)
                    5 -> k (z Blob)
                    6 -> k (z TimeStamp)
                    7 -> k (z Midi)
                    _ -> undefined

  toConstr (Int _) = con_Int
  toConstr (Float _) = con_Float
  toConstr (Double _) = con_Double
  toConstr (String _) = con_String
  toConstr (Blob _) = con_Blob
  toConstr (TimeStamp _) = con_TimeStamp
  toConstr (Midi _) = con_Midi

  dataTypeOf _ = ty_Datum

con_Int :: Constr
con_Int = mkConstr ty_Datum "Int" [] Prefix

con_Float :: Constr
con_Float = mkConstr ty_Datum "Float" [] Prefix

con_Double :: Constr
con_Double = mkConstr ty_Datum "Double" [] Prefix

con_String :: Constr
con_String = mkConstr ty_Datum "String" [] Prefix

con_Blob :: Constr
con_Blob = mkConstr ty_Datum "Blob" [] Prefix

con_TimeStamp :: Constr
con_TimeStamp = mkConstr ty_Datum "TimeStamp" [] Prefix

con_Midi :: Constr
con_Midi = mkConstr ty_Datum "Midi" [] Prefix

ty_Datum :: DataType
ty_Datum = mkDataType "Sound.OpenSoundControl.Datum"
  [con_Int,con_Float,con_Double,con_String,con_Blob,con_TimeStamp,con_Midi]


instance Typeable Time where
  typeOf _  = mkTyConApp tc_Time []

tc_Time :: TyCon
tc_Time = mkTyCon "Sound.OpenSoundControl.Time"

instance Data Time where
    gfoldl k z (UTCr x) = z UTCr `k` x
    gfoldl k z (NTPr x) = z NTPr `k` x
    gfoldl k z (NTPi x) = z NTPi `k` x

    gunfold k z c = case constrIndex c of
                      1 -> k (z UTCr)
                      2 -> k (z NTPr)
                      3 -> k (z NTPi)
                      _ -> undefined

    toConstr (UTCr _) = con_UTCr
    toConstr (NTPr _) = con_NTPr
    toConstr (NTPi _) = con_NTPi

    dataTypeOf _ = ty_Time

con_UTCr :: Constr
con_UTCr = mkConstr ty_Time "UTCr" [] Prefix

con_NTPr :: Constr
con_NTPr = mkConstr ty_Time "NTPr" [] Prefix

con_NTPi :: Constr
con_NTPi = mkConstr ty_Time "NTPi" [] Prefix

ty_Time :: DataType
ty_Time = mkDataType "Sound.OpenSoundControl.Time"
          [con_UTCr, con_NTPr, con_NTPi]


instance Typeable OSC where
  typeOf _ = mkTyConApp tc_OSC []

tc_OSC :: TyCon
tc_OSC = mkTyCon "Sound.OpenSoundControl.OSC"

instance Data OSC where
  gfoldl k z (Message a b) = z Message `k` a `k` b
  gfoldl k z (Bundle a b) = z Bundle `k` a `k` b

  gunfold k z c = case constrIndex c of
                    1 -> k (k (z Message))
                    2 -> k (k (z Bundle))
                    _ -> undefined

  toConstr (Message _ _) = con_Message
  toConstr (Bundle _ _) = con_Bundle

  dataTypeOf _ = ty_OSC

con_Message :: Constr
con_Message = mkConstr ty_OSC "Message" [] Prefix

con_Bundle :: Constr
con_Bundle = mkConstr ty_OSC "Bundle" [] Prefix

ty_OSC :: DataType
ty_OSC = mkDataType "Sound.OpenSoundControl.OSC" [con_Message,con_Bundle]

instance Typeable Special where
  typeOf _ = mkTyConApp tc_Special []

tc_Special :: TyCon
tc_Special = mkTyCon "Sound.SC3.UGen.Special"

instance Data Special where
  gfoldl k z (Special a) = z Special `k` a
  gunfold k z _ = k (z Special)
  toConstr (Special _) = con_Special
  dataTypeOf _ = ty_Special

con_Special :: Constr
con_Special = mkConstr ty_Special "Special" [] Prefix

ty_Special :: DataType
ty_Special = mkDataType "Sound.SC3.UGen.Special" [con_Special]

instance Typeable Rate where
  typeOf _ = mkTyConApp tc_Rate []

tc_Rate :: TyCon
tc_Rate = mkTyCon "Sound.SC3.UGen.Rate"

instance Data Rate where
  gfoldl _ z IR = z IR
  gfoldl _ z KR = z KR
  gfoldl _ z AR = z AR
  gfoldl _ z DR = z DR

  gunfold _ z c = case constrIndex c of
                    1 -> z IR
                    2 -> z KR
                    3 -> z AR
                    4 -> z DR
                    _ -> undefined

  toConstr IR = con_IR
  toConstr KR = con_KR
  toConstr AR = con_AR
  toConstr DR = con_DR

  dataTypeOf _ = ty_Rate

con_IR :: Constr
con_IR = mkConstr ty_Rate "IR" [] Prefix

con_KR :: Constr
con_KR = mkConstr ty_Rate "KR" [] Prefix

con_AR :: Constr
con_AR = mkConstr ty_Rate "AR" [] Prefix

con_DR :: Constr
con_DR = mkConstr ty_Rate "DR" [] Prefix

ty_Rate :: DataType
ty_Rate = mkDataType "Sound.SC3.UGen.Rate"
          [con_IR,con_KR,con_AR,con_DR]

instance Typeable UGen where
  typeOf _ = mkTyConApp tc_UGen []

tc_UGen :: TyCon
tc_UGen = mkTyCon "Sound.SC3.UGen.UGen"

instance Data UGen where
    gfoldl k z (Constant a) = z Constant `k` a
    gfoldl k z (Control a b c d) = z Control `k` a `k` b `k` c `k` d
    gfoldl k z (Primitive a b c d e f) =
        z Primitive `k` a `k` b `k` c `k` d `k` e `k` f
    gfoldl k z (Proxy a b) = z Proxy `k` a `k` b
    gfoldl k z (MCE a) = z MCE `k` a
    gfoldl k z (MRG a b) = z MRG `k` a `k` b

    gunfold k z c = case constrIndex c of
                      1 -> k (z Constant)
                      2 -> k (k (k (k (z Control))))
                      3 -> k (k (k (k (k (k (z Primitive))))))
                      4 -> k (k (z Proxy))
                      5 -> k (z MCE)
                      6 -> k (k (z MRG))
                      _ -> undefined

    toConstr (Constant _) = con_Constant
    toConstr (Control _ _ _ _) = con_Control
    toConstr (Primitive _ _ _ _ _ _) = con_Primitive
    toConstr (Proxy _ _) = con_Proxy
    toConstr (MCE _) = con_MCE
    toConstr (MRG _ _) = con_MRG

    dataTypeOf _ = ty_UGen

con_Constant :: Constr
con_Constant = mkConstr ty_UGen "Constant" [] Prefix

con_Control :: Constr
con_Control = mkConstr ty_UGen "Control" [] Prefix

con_Primitive :: Constr
con_Primitive = mkConstr ty_UGen "Primitive" [] Prefix

con_Proxy :: Constr
con_Proxy = mkConstr ty_UGen "Proxy" [] Prefix

con_MCE :: Constr
con_MCE = mkConstr ty_UGen "MCE" [] Prefix

con_MRG :: Constr
con_MRG = mkConstr ty_UGen "MRG" [] Prefix

ty_UGen :: DataType
ty_UGen = mkDataType "Sound.SC3.UGen.UGen"
          [con_Constant,con_Control,con_Primitive,con_Proxy,con_MCE,con_MRG]
#endif