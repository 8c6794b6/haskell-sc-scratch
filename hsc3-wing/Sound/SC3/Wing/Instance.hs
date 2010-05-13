--------------------------------------------------------------------------------
-- | Additional instance declarations for datat types found in Sound.SC3.
--
-- Currently OSC and UGen are instantiated from Data.Typeable, Data.Data.
-- 

module Sound.SC3.Wing.Instance () where

import Data.Generics
import Data.Data
import Data.Typeable

import Sound.SC3
import Sound.OpenSoundControl

instance Typeable Datum where
    typeOf _  = mkTyConApp tc_Datum []

tc_Datum = mkTyCon "Sound.OpenSoundControl.Datum"

instance Data Datum where
    gfoldl k z (Int i) = z Int `k` i
    gfoldl k z (Float f) = z Float `k` f
    gfoldl k z (Double d) = z Double `k` d
    gfoldl k z (String s) = z String `k` s
    gfoldl k z (Blob b) = z Blob `k` b
    gfoldl k z (TimeStamp t) = z TimeStamp `k` t

    gunfold k z c = case constrIndex c of
                      1 -> k (z Int)
                      2 -> k (z Float)
                      3 -> k (z Double)
                      4 -> k (z String)
                      5 -> k (z Blob)
                      6 -> k (z TimeStamp)

    toConstr (Int _) = con_Int
    toConstr (Float _) = con_Float
    toConstr (Double _) = con_Double
    toConstr (String _) = con_String
    toConstr (Blob _) = con_Blob
    toConstr (TimeStamp _) = con_TimeStamp

    dataTypeOf _ = ty_Datum

con_Int = mkConstr ty_Datum "Int" [] Prefix
con_Float = mkConstr ty_Datum "Float" [] Prefix
con_Double = mkConstr ty_Datum "Double" [] Prefix
con_String = mkConstr ty_Datum "String" [] Prefix
con_Blob = mkConstr ty_Datum "Blob" [] Prefix
con_TimeStamp = mkConstr ty_Datum "TimeStamp" [] Prefix

ty_Datum = mkDataType "Sound.OpenSoundControl.Datum"
           [con_Int,con_Float,con_Double,con_String,con_Blob,con_TimeStamp]


instance Typeable Time where
    typeOf _  = mkTyConApp tc_Time []

tc_Time = mkTyCon "Sound.OpenSoundControl.Time"

instance Data Time where
    gfoldl k z (UTCr x) = z UTCr `k` x
    gfoldl k z (NTPr x) = z NTPr `k` x
    gfoldl k z (NTPi x) = z NTPi `k` x

    gunfold k z c = case constrIndex c of
                      1 -> k (z UTCr)
                      2 -> k (z NTPr)
                      3 -> k (z NTPi)

    toConstr (UTCr _) = con_UTCr
    toConstr (NTPr _) = con_NTPr
    toConstr (NTPi _) = con_NTPi

    dataTypeOf _ = ty_Time

con_UTCr = mkConstr ty_Time "UTCr" [] Prefix
con_NTPr = mkConstr ty_Time "NTPr" [] Prefix
con_NTPi = mkConstr ty_Time "NTPi" [] Prefix

ty_Time = mkDataType "Sound.OpenSoundControl.Time"
          [con_UTCr, con_NTPr, con_NTPi]


instance Typeable OSC where
    typeOf _ = mkTyConApp tc_OSC []

tc_OSC = mkTyCon "Sound.OpenSoundControl.OSC"

instance Data OSC where
    gfoldl k z (Message a b) = z Message `k` a `k` b
    gfoldl k z (Bundle a b) = z Bundle `k` a `k` b

    gunfold k z c = case constrIndex c of
                      1 -> k (k (z Message))
                      2 -> k (k (z Bundle))

    toConstr (Message _ _) = con_Message
    toConstr (Bundle _ _) = con_Bundle

    dataTypeOf _ = ty_OSC

con_Message = mkConstr ty_OSC "Message" [] Prefix
con_Bundle = mkConstr ty_OSC "Bundle" [] Prefix

ty_OSC = mkDataType "Sound.OpenSoundControl.OSC" [con_Message,con_Bundle]


instance Typeable Special where
    typeOf _ = mkTyConApp tc_Special []

tc_Special = mkTyCon "Sound.SC3.UGen.Special"

instance Data Special where
    gfoldl k z (Special a) = z Special `k` a
    gunfold k z c = k (z Special)
    toConstr (Special _) = con_Special
    dataTypeOf _ = ty_Special

con_Special = mkConstr ty_Special "Special" [] Prefix

ty_Special = mkDataType "Sound.SC3.UGen.Special" [con_Special]


-- instance Typeable UGenId where
--     typeOf _ = mkTyConApp tc_UGenId []

-- tc_UGenId = mkTyCon "Sound.SC3.UGen.UGen"

-- instance Data UGenId where
--     gfoldl k z (UGenId a) = z UGenId `k` a
--     gunfold k z c = k (z UGenId)
--     toConstr (UGenId _) = con_UGenId
--     dataTypeOf _ = ty_UGenId

-- con_UGenId = mkConstr ty_UGenId "UGenId" [] Prefix
-- ty_UGenId = mkDataType "Sound.SC3.UGen.UGen" [con_UGenId]


instance Typeable Rate where
    typeOf _ = mkTyConApp tc_Rate []

tc_Rate = mkTyCon "Sound.SC3.UGen.Rate"

instance Data Rate where
    gfoldl k z IR = z IR
    gfoldl k z KR = z KR
    gfoldl k z AR = z AR
    gfoldl k z DR = z DR

    gunfold k z c = case constrIndex c of
                      1 -> z IR
                      2 -> z KR
                      3 -> z AR
                      4 -> z DR

    toConstr IR = con_IR
    toConstr KR = con_KR
    toConstr AR = con_AR
    toConstr DR = con_DR

    dataTypeOf _ = ty_Rate

con_IR = mkConstr ty_Rate "IR" [] Prefix
con_KR = mkConstr ty_Rate "KR" [] Prefix
con_AR = mkConstr ty_Rate "AR" [] Prefix
con_DR = mkConstr ty_Rate "DR" [] Prefix

ty_Rate = mkDataType "Sound.SC3.UGen.Rate"
          [con_IR,con_KR,con_AR,con_DR]


instance Typeable UGen where
    typeOf _ = mkTyConApp tc_UGen []

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

    toConstr (Constant _) = con_Constant
    toConstr (Control _ _ _ _) = con_Control
    toConstr (Primitive _ _ _ _ _ _) = con_Primitive
    toConstr (Proxy _ _) = con_Proxy
    toConstr (MCE _) = con_MCE
    toConstr (MRG _ _) = con_MRG

    dataTypeOf _ = ty_UGen

con_Constant = mkConstr ty_UGen "Constant" [] Prefix
con_Control = mkConstr ty_UGen "Control" [] Prefix
con_Primitive = mkConstr ty_UGen "Primitive" [] Prefix
con_Proxy = mkConstr ty_UGen "Proxy" [] Prefix
con_MCE = mkConstr ty_UGen "MCE" [] Prefix
con_MRG = mkConstr ty_UGen "MRG" [] Prefix

ty_UGen = mkDataType "Sound.SC3.UGen.UGen"
          [con_Constant,con_Control,con_Primitive,con_Proxy,con_MCE,con_MRG]

--
-- Some tests
--

-- datumList :: [Datum]
-- datumList = [Int 1,
--              Int 2,
--              Float 1,
--              Double 1,
--              String "str",
--              Blob [],
--              TimeStamp (UTCr 1),
--              TimeStamp (NTPr 2),
--              TimeStamp (NTPi 3)
--             ]

-- test1_Datum = everywhere (mkT f) datumList
--     where f (Int i) = Int (i + 3)
--           f x = x

-- test2_Datum = everything (+) (0 `mkQ` f) datumList
--     where f (Int i) = i
--           f _ = 0

-- timeList :: [Time]
-- timeList = [UTCr 0,
--             UTCr 1,
--             NTPr 10,
--             NTPr 20,
--             NTPi 100,
--             NTPi 200]

-- test1_Time = everywhere (mkT f) timeList
--     where f (NTPi x) = NTPi (x + 100)
--           f a = a

-- test2_Time = everything (+) (0 `mkQ` f) datumList
--     where f (UTCr _) = 0
--           f (NTPi a) = fromInteger a
--           f (NTPr a) = a

-- oscMsg = Bundle (NTPi 0)
--          [
--           s_new "foo" 1000 AddToTail 1 [("amp",80),("freq",440)],
--           s_new "poo" 1001 AddToHead 1 [("out",0),("pan",0.2)]
--          ]

-- test1_OSC = everywhere (mkT f) oscMsg
--     where f (String s) | s == "foo" = String "bar"
--           f a = a

-- test2_OSC = everything (++) ([] `mkQ` f) oscMsg
--     where f (String s) = [s]
--           f _ = []

-- ugen = out 0 osc
--     where osc = sinOsc ar freq 0 * env
--           freq = control kr "freq" 440
--           env = xLine kr 1 0.1 1 RemoveSynth

-- test1_UGen = everywhere (mkT f) ugen
--     where f (Constant c) = Constant (c+10)
--           f a = a

-- test2_UGen = everything (+) (0 `mkQ` f) ugen
--     where f AR =  1
--           f _ = 0