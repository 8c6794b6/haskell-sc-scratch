{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Synthdef database using acid-state and ixset.

Slower than Kyoto cabinet, though has querying function with conditions.
-}

module SynthDB.Persist where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.Data (Data,Typeable)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), dropExtension)
import System.Environment (getEnv)

import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Data.Acid
import Data.IxSet
import Data.SafeCopy
import Sound.SC3
import Sound.SC3.Lepton (scSynthdefPath)
import Sound.SC3.Lepton.Parser

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M

{-
TODO:

* Add more query condition.
* Profile.
-}

------------------------------------------------------------------------------
-- For querying index.

data SDR = SDR
  { sdrName :: SDName
  , sdrNumUGens :: Int
  , sdrParams :: [PR]
  , sdrUGens :: [UR]
  } deriving (Eq,Ord,Data,Typeable)

instance Show SDR where
  show (SDR (SDName n) _ _ _) = "SDR " ++ C8.unpack n

instance Indexable SDR where
  empty = ixSet
    [ ixFun (return . sdrName)
    , ixFun (map prName . sdrParams)
    , ixFun (map prValue . sdrParams)
    , ixFun (map ugName . sdrUGens)
    , ixFun (map ugRate . sdrUGens) ]

newtype SDName = SDName ByteString
  deriving (Eq,Show,Ord,Data,Typeable)

data PR = PR
  { prName :: ParamName
  , prValue :: ParamValue
  } deriving (Eq,Show,Ord,Data,Typeable)

newtype ParamName = ParamName {unParamName :: ByteString}
  deriving (Eq,Show,Ord,Data,Typeable)

newtype ParamValue = ParamValue Double
  deriving (Eq,Show,Ord,Data,Typeable)

data UR = UR
  { ugName :: UGenName
  , ugRate :: Rate
  , ugCount :: UGenCount
  } deriving (Eq,Show,Ord,Data,Typeable)

newtype UGenName = UGenName {unUGenName :: ByteString}
  deriving (Eq,Show,Ord,Data,Typeable)

newtype UGenCount = UGenCount {unUGenCount :: Int}
  deriving (Eq,Show,Ord,Data,Typeable,Num,Integral,Real,Enum)

$(deriveSafeCopy 0 'base ''UGenName)
$(deriveSafeCopy 0 'base ''Rate)
$(deriveSafeCopy 0 'base ''UGenCount)
$(deriveSafeCopy 0 'base ''UR)
$(deriveSafeCopy 0 'base ''ParamName)
$(deriveSafeCopy 0 'base ''ParamValue)
$(deriveSafeCopy 0 'base ''PR)
$(deriveSafeCopy 0 'base ''SDName)
$(deriveSafeCopy 0 'base ''SDR)

synthdefFileToSDef :: SynthDefFile -> [SDR]
synthdefFileToSDef = map synthdefToSDef . sdDefs

synthdefToSDef :: SynthDef -> SDR
synthdefToSDef sd = SDR (SDName $ C8.pack $ sdName sd) nug prs ugs where
  prs = mkPR (sdConstants sd) (sdParamNames sd)
  ugs = mkUR (sdUGenSpecs sd)
  nug = unUGenCount $ foldr (\ug acc -> ugCount ug + acc) 0 ugs

mkUR :: [UGenSpec] -> [UR]
mkUR uss = M.foldrWithKey g [] $ foldr f M.empty uss where
  g (n,r) v acc = UR (UGenName n) r (UGenCount v):acc
  f us m = M.insertWith (+)
           (C8.pack $ map toLower $ mkUN (usName us) (usSpecial us), toR us) 1 m

mkUN :: Integral a => String -> a -> String
mkUN n s
  | n == "BinaryOpUGen" = binaryName (fromIntegral s)
  | n == "UnaryOpUGen"  = unaryName (fromIntegral s)
  | otherwise           = n

toR :: UGenSpec -> Rate
toR u = case usRate u of
  0 -> IR; 1 -> KR; 2 -> AR; 3 -> DR; _ -> error $ show u

mkPR :: [Double] -> [ParamPair] -> [PR]
mkPR cs ps = map f ps where
  f (ParamPair n i) = PR (ParamName $ C8.pack n) (ParamValue v) where
    v | length cs <= fromIntegral i = 0
      | otherwise                   = cs !! fromIntegral i

withAllSynthDefs :: (String -> SynthDefFile -> ByteString -> IO a) -> IO [a]
withAllSynthDefs act = do
  defDir <- getEnv scSynthdefPath
  files <- getDirectoryContents defDir
  forM (filter (`notElem` [".", ".."]) files) $ \file -> do
    contents <- C8.readFile (defDir </> file)
    case parse synthDefFile contents of
      Done _ sd -> act file sd contents
      _         -> error $ "Failed to parse: " ++ file

------------------------------------------------------------------------------
-- For storing bytestring synthdef contents.

data SDC = SDC
  { sdcName :: SDCName
  , sdcContents :: ByteString
  } deriving (Eq,Show,Ord,Data,Typeable)

newtype SDCName = SDCName ByteString
  deriving (Eq,Show,Ord,Data,Typeable)

instance Indexable SDC where
  empty = ixSet [ixFun (return . sdcName)]

$(deriveSafeCopy 0 'base ''SDCName)
$(deriveSafeCopy 0 'base ''SDC)

------------------------------------------------------------------------------
-- For statistics

data Stat = Stat
 { numSynthDefs :: Int
 , numUGens :: Int
 , ugenCounts :: [(String,Int)]
 , paramCounts :: [(String,Int)]
 } deriving (Eq,Show,Data,Typeable)

$(deriveSafeCopy 0 'base ''Stat)

nullStat :: Stat
nullStat = Stat 0 0 [] []

getStat :: AcidDB -> Stat
getStat = stat

mkStat :: IxSet SDR -> Stat
mkStat ixs = Stat (size ixs) (M.fold (+) 0 umap) urs prs where
  urs = sortBy (flip compare `on` snd) . M.toList $ umap
  umap = foldr f M.empty $ concatMap sdrUGens $ toList ixs
  f u m = M.insertWith (+) (C8.unpack $ unUGenName $ ugName u)
          (unUGenCount $ ugCount u) m
  prs = sortBy (flip compare `on` snd) . M.toList .
        foldr g M.empty $ concatMap sdrParams $ toList ixs
  g p m = M.insertWith (+) (C8.unpack $ unParamName $ prName p) 1 m

------------------------------------------------------------------------------
-- Acid database

data AcidDB = AcidDB
  { keyIx :: IxSet SDR
  , valueIx :: IxSet SDC
  , stat :: Stat
  } deriving (Eq,Show,Data,Typeable)

$(deriveSafeCopy 0 'base ''AcidDB)

loadDB :: Query AcidDB AcidDB
loadDB = ask

saveDB :: AcidDB -> Update AcidDB ()
saveDB = put

$(makeAcidic ''AcidDB ['saveDB, 'loadDB])

getDB :: IO AcidDB
getDB = do
  acid <- openAcidState (AcidDB empty empty nullStat)
  query acid LoadDB

getDef :: AcidDB -> String -> SynthDefFile
getDef db k =
  case getOne $ valueIx db @= (SDCName (C8.pack k)) of
    Nothing -> error $ "No such synthdef: " ++ k
    Just sdc -> case parse synthDefFile $ sdcContents sdc of
      Done _ sd -> sd
      _         -> error $ "Failed to parse synthdef: " ++ k

queryBy :: AcidDB -> (IxSet SDR -> IxSet SDR) -> (IxSet SDR)
queryBy db f = f (keyIx db)

queryUGs :: AcidDB -> String -> [SDR]
queryUGs db q = sortBy (compare `on` scoreUG q) . toList $
             queryBy db (@* (map UGenName . C8.words . C8.pack $ q))

scoreUG :: String -> SDR -> Double
scoreUG ns r = foldr f 0 (sdrUGens r) where
  f ur acc | p ur      = acc + (nUG / fromIntegral (ugCount ur))
           | otherwise = acc
  p ur = unUGenName (ugName ur) `elem` uns
  nUG = fromIntegral (sdrNumUGens r)
  uns = C8.words $ C8.pack ns

initSDefs :: IO (IxSet SDR, IxSet SDC)
initSDefs = do
  sds <- withAllSynthDefs $ \path sd contents -> do
    putStrLn $ "Inserting " ++ path
    return (synthdefFileToSDef sd,
            SDC (SDCName $ C8.pack $ dropExtension path) contents)
  let (is,cs) = unzip sds
      ixs = foldr insert empty $ concat is
      cxs = foldr insert empty cs
  return (ixs, cxs)

------------------------------------------------------------------------------
-- For command line interface.

initDB :: IO ()
initDB = do
  db <- openAcidState (AcidDB empty empty nullStat)
  (ixs,cxs) <- initSDefs
  update db (SaveDB $ AcidDB ixs cxs (mkStat ixs))
  closeAcidState db

search :: String -> IO ()
search str = do
  db <- getDB
  mapM_ print $ queryUGs db str

dump :: String -> IO ()
dump n = do
  db <- getDB
  print . prettyDefFile $ getDef db n
