--------------------------------------------------------------------------------
-- | Instances.
--

module Instances where

import Data.Generics
import Data.Data
import Data.Typeable

import Sound.SC3
import Sound.OpenSoundControl

-- XXX: Instantiate OSC and UGen from Data and Typeable.

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

instance Typeable Time where
    typeOf _  = mkTyConApp tc_Time []

tc_Time = mkTyCon "Sound.OpenSoundControl.Time"

-- instance Data OSC where
--     gfoldl k z (Message _ _) = undefined
--     gfoldl k z (Bundle _ _) = undefined

-- instance Typeable OSC where
--     typeOf (Message _ _) = undefined
--     typeOf (Bundle _ _) = undefined

--
-- Some tests
--

datumList :: [Datum]
datumList = [Int 1,
             Int 2,
             Float 1,
             Double 1,
             String "str",
             Blob [],
             TimeStamp (UTCr 1),
             TimeStamp (NTPr 2),
             TimeStamp (NTPi 3)
            ]

test1_Datum = everywhere (mkT f) datumList
    where f (Int i) = Int (i + 3)
          f x = x

test2_Datum = everything (+) (0 `mkQ` f) datumList
    where f (Int i) = i
          f _ = 0

timeList :: [Time]
timeList = [UTCr 0,
            UTCr 1,
            NTPr 10,
            NTPr 20,
            NTPi 100,
            NTPi 200]

test1_Time = everywhere (mkT f) timeList
    where f (NTPi x) = NTPi (x + 100)
          f a = a

test2_Time = everything (+) (0 `mkQ` f) datumList
    where f (UTCr _) = 0
          f (NTPi a) = fromInteger a
          f (NTPr a) = a