{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Simple adhoc ixset search indexer and web ui. Document database
contains word chunks of given file, separating the contents with
spaces and couple punctuation characters.  May not so much useful with
documents that not using space as separator.

Since the server is loading the whole set of indice, the first query
after running the server will be slow. From the second query, it will
be better.

When index command has specified with '.html' suffix, the body
contents of html document would be stored in database for searching.

-}
module IxFts where

import Prelude hiding ((.),id,div,head)

import Control.Applicative (Applicative(..), (<$>))
import Control.Category
import Control.Concurrent
import Control.Parallel
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Data (Data,Typeable)
import Data.List hiding (head,insert,find)
import Data.Map (Map)
import Data.Ord
import System.FilePath

import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Data.Acid
import Data.Iteratee (Iteratee, Enumerator, run, stream2stream)
import Data.Iteratee.IO (enumFile)
import Data.IxSet
import Data.SafeCopy hiding (extension)
import Happstack.Server hiding (body, method, port)
import System.Console.CmdArgs hiding (name)
import System.FilePath.Find
import Text.Blaze.Html5 hiding (base,map,summary)
import Text.Blaze.Html5.Attributes hiding
  (dir,id,title,form,style,span,size,summary)
import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, sections)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Happstack.Server as H
import qualified Text.Blaze.Html5.Attributes as A

------------------------------------------------------------------------------
-- Database

data Document = Document
  { docPath :: DocPath
  , docWordCount :: Int
  , docWordMap :: Map ByteString Int
  , docContents :: Contents
  } deriving (Eq,Ord,Data,Typeable)

instance Show Document where
  show (Document (DocPath p) _ _ _) = "Document " ++ p

newtype DocPath = DocPath {unDocPath::FilePath}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Contents = Contents {unContents::ByteString}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Word = Word {unWord::ByteString}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype DocDB = DocDB {docIx :: IxSet Document}
  deriving (Eq,Show,Data,Typeable)

instance Indexable Document where
  empty = ixSet
    [ixFun (return . docPath)
    ,ixFun (map Word . nub . mkChunks . unContents . docContents)]

mkChunks :: ByteString -> [ByteString]
mkChunks = C8.splitWith (`elem` " \t\n.,!?&()[]{}<>;/\"'")

$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocPath)
$(deriveSafeCopy 0 'base ''Contents)
$(deriveSafeCopy 0 'base ''Word)
$(deriveSafeCopy 0 'base ''DocDB)

saveDoc :: IxSet Document -> Update DocDB ()
saveDoc ixs = put (DocDB ixs)

loadDoc :: Query DocDB DocDB
loadDoc = ask

$(makeAcidic ''DocDB ['saveDoc, 'loadDoc])

getDB :: FilePath -> IO DocDB
getDB path = do
  db <- openAcidStateFrom path (DocDB empty)
  query db LoadDoc

------------------------------------------------------------------------------
-- Indexer

index :: FilePath -> Maybe String -> FilePath -> IO ()
index root ext out = do
  db <- openAcidStateFrom out (DocDB empty)
  let mkIx = case ext of
        Just e | "html" `isSuffixOf` e -> mkHtmlDocument
               | otherwise             ->
                 mkDocument (fileName ~~? ('*':e)) C8.pack
        Nothing -> mkDocument always C8.pack
  ixs <- mkIx root
  putStrLn "Creating index .... "
  update db (SaveDoc ixs)
  closeAcidState db
  putStrLn "Done."

mkDocument :: FilterPredicate -> (String -> ByteString)
           -> FilePath -> IO (IxSet Document)
mkDocument cond f root = foldM go empty =<< find always cond root where
  go acc fi = do
    contents <- f <$> work fi
    let dp = DocPath ("static" </> drop (length root) fi)
        dc = Contents contents
        ws = mkChunks contents
        dm = foldr (\w m -> M.insertWith (+) w 1 m) M.empty ws
        doc = Document dp (length ws) dm dc
    return $! doc `par` (acc `pseq` insert doc acc)

work :: FilePath -> IO String
work path = do
  putStrLn $ "Reading: " ++ path
  run =<< enumFile 8192 path stream2stream

htmlBody :: String -> ByteString
htmlBody =
  C8.pack . innerText . join . sections (~== TagOpen "body" []) .
  parseTags . map toLower

mkHtmlDocument :: FilePath -> IO (IxSet Document)
mkHtmlDocument = mkDocument (extension ==? ".html") htmlBody

------------------------------------------------------------------------------
-- Server

serve :: Int -> FilePath -> DocDB -> IO ()
serve p stt db = do
  putStrLn $ "Starting server with port: " ++ show p
  simpleHTTP nullConf {H.port=p} $ msum
    [ dir "favicon.ico" $ notFound $ toResponse ()
    , dir "static" $ serveDirectory EnableBrowsing [] stt
    , searchPage db
    , seeOther "" $ toResponse () ]

searchPage :: DocDB -> ServerPartT IO Response
searchPage db = do
  qs <- getDataFn $ look "q"
  case qs of
    Left _ ->
      ok $ toResponse $ do
        preEscapedString "<!doctype html>"
        html $ do
          head $ do
            title $ toHtml "ixset search"
            css
          body $ do
            div ! class_ (toValue "wrapper") $ inputForm ""
    Right qs' -> do
      let res = docIx db @* (map Word . C8.words . C8.pack $ qs')
      ok $ toResponse $ do
        preEscapedString "<!doctype html>"
        html $ do
          head $ do
            title $ toHtml $ "search result for " ++ qs'
            css
          body $ do
            div ! class_ (toValue "wrapper") $ do
              inputForm qs'
              div ! class_ (toValue "summary") $ toHtml $
                "search result for: \"" ++ qs' ++ "\", " ++
                "hit: " ++ show (size res) ++ ""
              ul $ mapM_ mkLink $ sortBy (comparing $ score qs') $ toList res

score :: String -> Document -> Double
score ws doc = foldr f 0 (C8.words $ C8.pack ws) where
  f w acc = acc + (total / fromIntegral (M.findWithDefault 0 w dmap))
  dmap = docWordMap doc
  total = fromIntegral $ docWordCount doc

mkLink :: Document -> Html
mkLink doc =
  li $ do
    a ! href (toValue path) $ do
      toHtml $ dropWhile (/= '/') path
  where
    path = unDocPath $ docPath doc

inputForm :: String -> Html
inputForm val =
  div ! class_ (toValue "input_form") $ do
    form !
      enctype (toValue "multipart/form-data") !
      method (toValue "GET") !
      action (toValue "/") $ do
        input !
          type_ (toValue "text") !
          name (toValue "q") !
          A.size (toValue "40") !
          value (toValue val) !
          autofocus (toValue "autofocus")
        input !
          type_ (toValue "submit") !
          value (toValue "search")

css :: Html
css = style ! type_ (toValue "text/css") $ toHtml
 "body { font-size: 15px; } \
\input { margin: 5px; border: 1px solid #868686; }\
\div.wrapper { padding: 20px; } \
\div.wrapper ul { list-style: none; padding-left: 10px; } \
\div.wrapper ul li { margin: 5px 0 } \
\div.wrapper ul li a { text-decoration: none; } \
\div.summary { font-size: 75%; padding-left: 20px; }"

------------------------------------------------------------------------------
-- CLI

data IxFts
  = Index { doc :: FilePath
          , ext :: Maybe String
          , out :: String }
  | Serve { port :: Int
          , db :: String
          , static :: FilePath }
    deriving (Eq,Show,Data,Typeable)

commands :: IxFts
commands = modes
  [ Index
      { doc = def &= typDir &=
            help "Path to directory containing target documents"
      , ext = def &= typ "EXTENSION" &=
              help "File extension to read"
      , out = "state" &= typDir &=
              help "Path to output index directory (default:state)"
      } &= help "Index documents under given path"
  , Serve
      { port = 8000 &= typ "PORT" &=
           help "Port number to serve (default:8000)"
      , db = "state" &= typDir &=
           help "Path to index database (default:state)"
      , static = "static" &= typDir &=
                 help "Path to static contents directory (default:static)"
      } &= help "Start HTTP server"
  ] &= summary "ixfts: simple text search indexer and server"

main :: IO ()
main = do
  args <- cmdArgs commands
  case args of
    Index i e o -> index i e o
    Serve p d s -> getDB d >>= \db -> db `seq` serve p s $! db
