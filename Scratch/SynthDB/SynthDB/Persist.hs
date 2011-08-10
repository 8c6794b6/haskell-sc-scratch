{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
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

* Specify query condition.
* Score matches, and sort by score.
* Profile.

-}

------------------------------------------------------------------------------
-- For querying index.

data SDR = SDR
  { sdrName :: SDName
  , sdrParams :: [PR]
  , sdrUGens :: [UR]
  } deriving (Eq,Ord,Data,Typeable)

instance Show SDR where
  show (SDR (SDName n) _ _) = "SDR " ++ C8.unpack n

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

newtype ParamName = ParamName ByteString
  deriving (Eq,Show,Ord,Data,Typeable)

newtype ParamValue = ParamValue Double
  deriving (Eq,Show,Ord,Data,Typeable)

data UR = UR
  { ugName :: UGenName
  , ugRate :: Rate
  , ugCount :: UGenCount
  } deriving (Eq,Show,Ord,Data,Typeable)

newtype UGenName = UGenName ByteString
  deriving (Eq,Show,Ord,Data,Typeable)

newtype UGenCount = UGenCount Int
  deriving (Eq,Show,Ord,Data,Typeable)

newtype SDB = SDB (IxSet SDR)
  deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''UGenName)
$(deriveSafeCopy 0 'base ''Rate)
$(deriveSafeCopy 0 'base ''UGenCount)
$(deriveSafeCopy 0 'base ''UR)
$(deriveSafeCopy 0 'base ''ParamName)
$(deriveSafeCopy 0 'base ''ParamValue)
$(deriveSafeCopy 0 'base ''PR)
$(deriveSafeCopy 0 'base ''SDName)
$(deriveSafeCopy 0 'base ''SDR)
$(deriveSafeCopy 0 'base ''SDB)

saveSDB :: IxSet SDR -> Update SDB ()
saveSDB ixs = put (SDB ixs)

loadSDB :: Query SDB (IxSet SDR)
loadSDB = ask >>= \(SDB ixs) -> return ixs

$(makeAcidic ''SDB ['saveSDB, 'loadSDB])

synthdefFileToSDef :: SynthDefFile -> [SDR]
synthdefFileToSDef = map synthdefToSDef . sdDefs

synthdefToSDef :: SynthDef -> SDR
synthdefToSDef sd = SDR (SDName $ C8.pack $ sdName sd) prs ugs where
  prs = mkPR (sdConstants sd) (sdParamNames sd)
  ugs = mkUR (sdUGenSpecs sd)

mkUR :: [UGenSpec] -> [UR]
mkUR uss = M.foldrWithKey g [] $ foldr f M.empty uss where
  g (n,r) v acc = UR (UGenName n) r (UGenCount v):acc
  f us m = M.insertWith (+)
           (C8.pack $ map toLower $ mkUN (usName us) (usSpecial us), toR us) 1 m
  mkUN n s
    | n == "BinaryOpUGen" = binaryName (fromIntegral s)
    | n == "UnaryOpUGen"  = unaryName (fromIntegral s)
    | otherwise           = n
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

initSDefs :: IO (IxSet SDR, IxSet SDC)
initSDefs = do
  sds <- withAllSynthDefs $ \path sd contents -> do
    return (synthdefFileToSDef sd,
            SDC (SDCName $ C8.pack $ dropExtension path) contents)
  let (is,cs) = unzip sds
      ixs = foldr insert empty $ concat is
      cxs = foldr insert empty cs
  return (ixs, cxs)

queryBy :: (IxSet SDR -> IxSet SDR) -> IO (IxSet SDR)
queryBy f = do
  db <- openAcidState (SDB empty)
  r <- do
    ixs <- query db LoadSDB
    return $ f ixs
  closeAcidState db
  return r

queryUGs :: String -> IO [SDR]
queryUGs q = return . toList =<<
  queryBy (@* (map UGenName . C8.words . C8.pack $ q))

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

newtype ContentDB = ContentDB (IxSet SDC)
  deriving (Show,Data,Typeable)

$(deriveSafeCopy 0 'base ''SDCName)
$(deriveSafeCopy 0 'base ''SDC)
$(deriveSafeCopy 0 'base ''ContentDB)

saveContentDB :: IxSet SDC -> Update ContentDB ()
saveContentDB ixs = put (ContentDB ixs)

loadContentDB :: Query ContentDB (IxSet SDC)
loadContentDB = ask >>= \(ContentDB ixs) -> return ixs

$(makeAcidic ''ContentDB ['saveContentDB, 'loadContentDB])

getDef :: String -> IO SynthDefFile
getDef k = do
  db <- openAcidState (ContentDB empty)
  content <- do
    cxs <- query db LoadContentDB
    return $ cxs @= (SDCName (C8.pack k))
  closeAcidState db
  case getOne content of
    Nothing  -> error $ "No such synthdef: " ++ k
    Just sdc -> case parse synthDefFile $ sdcContents sdc of
      Done _ sd -> return sd
      _         -> error $ "Failed to parse synthdef: " ++ k

------------------------------------------------------------------------------
-- For command line interface.

initDB :: IO ()
initDB = do
  idb <- openAcidState (SDB empty)
  cdb <- openAcidState (ContentDB empty)
  (ixs,cxs) <- initSDefs
  update idb (SaveSDB ixs)
  update cdb (SaveContentDB cxs)
  closeAcidState idb
  closeAcidState cdb

search :: String -> IO ()
search str = mapM_ print . toList =<<
  queryBy (@* (map UGenName . C8.words . C8.pack $ str))

dump :: String -> IO ()
dump n = print . prettyDefFile =<< getDef n
