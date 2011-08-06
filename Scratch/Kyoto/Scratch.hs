{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (OverloadedStrings)

TODO:

* Sort by score ... how can we score the match?

* Wrap when line length get long in pretty printing of synthdef.

* Condition DSL? Re-read RWH chapter for traversing file system.

* Web interface? yesod, snap, happstack?

-}
module Kyoto.Scratch where

import Control.Applicative
import Control.Concurrent
import Control.Exception (handle)
import Control.Monad (forever, forM_, when)
import Data.Binary
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), dropExtension)
import System.Environment (getEnv)
import Text.PrettyPrint
import Prelude hiding (catch)

import Database.KyotoCabinet.Db
import Sound.SC3 hiding (Binary)
import Sound.SC3.Lepton (scSynthdefPath)
import Sound.SC3.Lepton.Parser hiding (int,double)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Sound.SC3 as SC3
import qualified Text.PrettyPrint as P

-- | Record for ugen information in synthdef file.
data UR = UR
  { -- | Total number of ugens used in synthdef.
    urNumUGens :: Int
    -- | Total number of IR ugens used in synthdef.
  , urIRs :: Int
    -- | Total number of KR ugens used in synthdef.
  , urKRs :: Int
    -- | Total number of AR ugens used in synthdef.
  , urARs :: Int
    -- | Total number of DR ugens used in synthdef.
  , urDRs :: Int
    -- | Number of identical ugens used in synthdef.
  , urNumIdUGens :: Int
    -- | UGen name and number of its occurance.
  , urMap :: Map String Int
  } deriving (Eq,Show)

instance Binary UR where
  get =
    UR <$> get <*> get <*> get <*> get <*> get <*> get <*>
    (M.fromList <$> (getMany =<< get))
  put r =
    M.foldrWithKey (\k a acc -> acc >> put (k,a))
    (mapM_ (\p -> put $ p r)
      [urNumUGens,urIRs,urKRs,urARs,urDRs,urNumIdUGens,M.size . urMap])
    (urMap r)

-- | Record for parameter information in synthdef file.
data PR = PR
  { -- | Total number of parameters in synthdef.
    prNumParams :: Int
    -- | Parameter name and its default value.
  , prMap :: Map String Double
  } deriving (Eq,Show)

instance Binary PR where
  get =
    PR <$> get <*> (M.fromList <$> (getMany =<< get))
  put r =
    M.foldrWithKey (\k a acc -> acc >> put (k,a))
    (put (prNumParams r) >> put (M.size m)) m
    where m = prMap r

getMany :: Binary a => Int -> Get [a]
getMany n = go [] n where
  go xs 0 = return $! reverse xs
  go xs i = get >>= \x -> x `seq` go (x:xs) (i-1)

-- | Pretty printer for UR.
prettyUR :: UR -> Doc
prettyUR (UR nug irs krs ars drs idug m) =
  text "ugens" <> colon <+> int nug <+>
  parens (int idug <+> text "identical" <> comma <+>
          text "ir" <> colon <> int irs <> comma <+>
          text "kr" <> colon <> int krs <> comma <+>
          text "ar" <> colon <> int ars <> comma <+>
          text "dr" <> colon <> int drs) $$
  M.foldrWithKey (\k v acc -> text k <> colon <+> int v $$ acc) P.empty m

-- | Pretty printer for PR.
prettyPR :: PR -> Doc
prettyPR (PR np m) =
  int np <+> text "params" <> colon $$
  M.foldrWithKey (\k v acc -> text k <> parens (double v) <+> acc) P.empty m

-- | Add each contents of UR. UGen counts will be unioned.
addUR :: UR -> UR -> UR
addUR (UR n1 i1 k1 a1 d1 id1 m1) (UR n2 i2 k2 a2 d2 id2 m2) =
  UR (n1+n2) (i1+i2) (k1+k2) (a1+a2) (d1+d2) (id1+id2) (M.unionWith (+) m1 m2)

{-| Initialize database.

Keys used in the database are:

* @def:NAME@ -- Contents of synthdef file.

* @prm:NAME@ -- Encoded PR data with 'Data.Binary.encode'.

* @ugn:NAME@ -- Encoded UR data with 'Data.Binary.encode'.

-}
initDB :: FilePath -> IO ()
initDB path =
  kcwithdbopen path [] [KCOWRITER,KCOCREATE] $ \db ->
  withAllDefs $ \name content -> do
    putStrLn $ "Inserting: " ++ name
    kcdbset db (B.pack $ "def:" ++ name) content
    case parse synthDefFile content of
      Done _ def -> do
        let (ps,us) = unzip $ defToValue def
        kcdbset db (B.pack $ "prm:" ++ name) $ B.concat ps
        kcdbset db (B.pack $ "ugn:" ++ name) $ B.concat us
      _          -> return ()

defToValue :: SynthDefFile -> [(ByteString,ByteString)]
defToValue sd = concatMap f $ sdDefs sd where
  f def = [(params def, ugens def)]
  params = B.concat . BL.toChunks . encode . mkPR
  ugens = B.concat . BL.toChunks . encode . mkUR

isDefRec :: ByteString -> Bool
isDefRec = B.isPrefixOf "def:"

isUGenRec :: ByteString -> Bool
isUGenRec = B.isPrefixOf "ugn:"

isParamRec :: ByteString -> Bool
isParamRec = B.isPrefixOf "prm:"

-- | Dump all synthdef records in database.
dumpAllSynthDefs :: FilePath -> IO ()
dumpAllSynthDefs path = withKV path $ \key val -> do
  when (isDefRec key) $ do
    case parseSDF val of
      Done _ def -> print $ prettyDefFile def
      _          -> return ()

-- | Parse bytestring value to synthdef.
parseSDF :: ByteString -> Result SynthDefFile
parseSDF = parse synthDefFile

-- | Dump all parameter records in database.
dumpParams :: FilePath -> IO ()
dumpParams path = withKV path $ \key val -> do
  when (isParamRec key) $ do
    putStr $ B.unpack key ++ " "
    print (decode $ BL.fromChunks [val] :: PR)

-- | Dump all ugen records in database.
dumpUGens :: FilePath -> IO ()
dumpUGens path = withKV path $ \key val -> do
  when (isUGenRec key) $ do
    putStr $ B.unpack key ++ " "
    print (decode $ BL.fromChunks [val] :: UR)

-- | Dump specified synthdef in database.
dumpSynthDef :: FilePath -> String -> IO ()
dumpSynthDef path defName = do
  kcwithdbopen path [] [KCOWRITER] $ \db -> do
    content <- kcdbget db (B.pack $ "def:" ++ defName)
    case parseSDF <$> content of
      Just (Done _ def) -> print $ {-# SCC "prettyDefFile" #-} prettyDefFile def
      _                 -> return ()

-- | Make db record for ugen values from synthdefs.
mkUR :: SynthDef -> UR
mkUR d = ur where
  ur = UR nug nir nkr nar ndr nid ucount
  nug = M.fold (+) 0 ustat
  nir = M.findWithDefault 0 0 ustat
  nkr = M.findWithDefault 0 1 ustat
  nar = M.findWithDefault 0 2 ustat
  ndr = M.findWithDefault 0 3 ustat
  nid = M.size ucount
  (ustat,ucount) = foldr g (iniStat,iniUGen) (sdUGenSpecs d)
  iniStat = M.fromList [(0,0),(1,0),(2,0),(3,0)]
  iniUGen = M.empty
  g u (ml,mr) = (M.insertWith (+) (usRate u) 1 ml,M.insertWith (+) (ugKey u) 1 mr)
  ugKey u | un == "BinaryOpUGen" = (map toLower $ binaryName sp) ++ "." ++ rate
          | un == "UnaryOpUGen"  = (map toLower $ unaryName sp) ++ "." ++ rate
          | otherwise            = (map toLower $ un) ++ "." ++  rate
    where
      un = usName u
      sp = fromIntegral $ usSpecial u
      rate = case toEnum $ fromIntegral $ usRate u of
        IR -> "ir"; KR -> "kr"; DR -> "dr"; AR -> "ar"

-- | Make db record for parameter values from synthdef.
mkPR :: SynthDef -> PR
mkPR d = PR num pmap where
  (num,pmap) = foldr f (0,M.empty) (sdParamNames d)
  f pp (n,m) = (n+1,M.insert (ppName pp) (val $ fromIntegral $ ppIndex pp) m)
  val idx | idx < length cs = cs !! idx
          | otherwise = 0
  cs = sdConstants d

-- | Action to get synthdef for testing.
getSD :: String -> IO SynthDefFile
getSD n = do
  let p = "/home/atsuro/share/SuperCollider/synthdefs/" ++ n ++ ".scsyndef"
  res <- parse synthDefFile <$> B.readFile p
  case res of
    Done _ d -> return d
    _        -> error $ "No " ++ n ++ "!"

foo :: IO SynthDefFile
foo = getSD "foo"

foo' :: IO SynthDef
foo' = head . sdDefs <$> foo

hasParam :: FilePath -> ByteString -> IO [ByteString]
hasParam path = matchKV path ("prm:" `B.isPrefixOf`) . hasSubstring

hasUGen :: FilePath -> ByteString -> IO [ByteString]
hasUGen path = matchKV path ("ugn:" `B.isPrefixOf`) . hasSubstring

hasSubstring :: ByteString -> ByteString -> Bool
hasSubstring sub = not . B.null . snd . B.breakSubstring sub

isSubstringOf sub = not . B.null . snd . B.breakSubstring sub

-- search :: FilePath -> String -> IO [Match]
search :: String -> FilePath -> IO [(ByteString, Double)]
search qs path = do
  mv <- newMVar M.empty
  withKV path $ \key value -> do
    forM_ (words $ map toLower qs) $ \query' -> do
      let queryB = B.pack query'
      when (queryB `isSubstringOf` key) $ do
        modifyMVar_ mv $ \m -> do
          let score | queryB == nameFromKey key = 1
                    | otherwise                 = 0.25
          return $ M.insertWith (+) (nameFromKey key) score m
      when (isUGenRec key && queryB `isSubstringOf` value) $ do
        let ur = decode (BL.fromChunks [value]) :: UR
        modifyMVar_ mv $
          return . M.insertWith (+) (nameFromKey key) (urScore ur query')
      when (isParamRec key && queryB `isSubstringOf` value) $ do
        let pr = decode (BL.fromChunks [value]) :: PR
        modifyMVar_ mv $
          return . M.insertWith (+) (nameFromKey key) (prScore pr query')
  return . sortBy (flip compare `on` snd) . M.toList =<< readMVar mv

-- | Calculate score for given query from ugen record.
urScore :: UR -> String -> Double
urScore ur q = f ".ar" urARs + f ".kr" urKRs  + f ".ir" urIRs + f ".dr" urDRs
  where
    urm = urMap ur
    f suf den
      | den ur /= 0 =
        fromIntegral (fromMaybe 0 (M.lookup (q ++ suf) urm)) /
        fromIntegral (den ur)
      | otherwise   = 0

-- | Calculate score for given query from param record.
-- Currently, query is unused.
prScore :: PR -> String -> Double
prScore pr q | den /= 0  = 1 / fromIntegral den
             | otherwise = 0
  where
    prm = prMap pr
    den = prNumParams pr

nameFromKey :: ByteString -> ByteString
nameFromKey str = B.drop 4 str
{-# INLINE nameFromKey #-}

matchKV :: FilePath -> (ByteString -> Bool) -> (ByteString -> Bool)
        -> IO [ByteString]
matchKV path pk pv = do
  m <- newMVar []
  withKV path $ \key value ->
    when (pk key && pv value) $ modifyMVar_ m (return . (key:))
  readMVar m

withKV :: FilePath -> (ByteString -> ByteString -> IO ()) -> IO ()
withKV path act = do
  kcwithdbopen path [] [KCOREADER] $ \db ->
    kcwithdbcursor db $ \cur -> do
      kccurjump cur
      handle ignoreException $ forever $ do
        uncurry act =<< kccurget cur True

paramStat :: FilePath -> IO (Int, M.Map String Int)
paramStat path = do
  mv <- newMVar (0,M.empty)
  withKV path $ \k v ->
    when (isParamRec k) $ do
      modifyMVar_ mv $ \(total,counts) -> do
        let pr = decode (BL.fromChunks [v]) :: PR
        return (total + prNumParams pr,
                foldr (\n m -> M.insertWith (+) n 1 m)
                counts (M.keys $ prMap pr))
      return ()
  readMVar mv

prettyParamStat :: (Int,M.Map String Int) -> Doc
prettyParamStat (total,m) =
  text "params" <> colon <+> int total $$
  M.foldrWithKey (\k num acc -> text k <> colon <+> int num $$ acc)
  P.empty m

ugenStat :: FilePath -> IO UR
ugenStat path = do
  mv <- newMVar (UR 0 0 0 0 0 0 M.empty)
  withKV path $ \k v ->
    when (isUGenRec k) $
      modifyMVar_ mv $ return . addUR (decode (BL.fromChunks [v]))
  readMVar mv

getValue :: FilePath -> ByteString -> IO (Maybe ByteString)
getValue path key = kcwithdbopen path [] [KCOREADER] $ \db ->
  kcdbget db key

ignoreException :: KcException -> IO ()
ignoreException = const $ return ()

withAllDefs :: (String -> ByteString -> IO ()) -> IO ()
withAllDefs act = do
  defDir <- getEnv scSynthdefPath
  files <- getDirectoryContents defDir
  forM_ (filter (`notElem` [".", ".."]) files) $ \file -> do
    contents <- B.readFile (defDir </> file)
    act (dropExtension file) contents
