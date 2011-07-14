------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Parser for commands used in SCNode interactive prompt.
--
module Sound.SC3.Lepton.CLI.Parser
  ( parseCmd
  , updateParams
  ) where

import Data.List (unionBy)
import Data.Function (on)
import Text.Parsec
import Text.Show.Functions ()

import Sound.SC3 hiding (free, status)
import Sound.SC3.Lepton

import Sound.SC3.Lepton.CLI.SCZipper
import Sound.SC3.Lepton.CLI.SCShellCmd

-- | Parser for command.
parseCmd :: String -> Either ParseError Cmd
parseCmd = runParser commands () "interactive" . clean where
  clean = reverse . (nw . reverse) . nw
  nw = dropWhile (== ' ')

commands :: Monad m => ParsecT String u m Cmd
commands = do
  optional (many space)
  result <- choice $ map try commandNames
  optional (many space)
  return result

commandNames :: Monad m => [ParsecT String u m Cmd]
commandNames = [pwd, ls, cd, tree, set, free, new, run, status, mv, refresh]

pwd :: Monad m => ParsecT String u m Cmd
pwd =  string "pwd" >> return Pwd

ls :: Monad m => ParsecT String u m Cmd
ls = do
  string "ls" >> optional (many space)
  fmap Ls paths

cd :: Monad m => ParsecT String u m Cmd
cd = do
  string "cd" >> optional (many space)
  fmap Cd paths

tree :: Monad m => ParsecT String u m Cmd
tree = do
  string "tree" >> optional (many space)
  fmap Tree paths

set :: Monad m => ParsecT String u m Cmd
set = do
  string "set" >> many space
  nid <- integer
  many space
  ps <- synthParam `sepBy` (many space)
  return $ Set (read nid) ps

free :: Monad m => ParsecT String u m Cmd
free = do
  string "free" >> many1 space
  nid <- integer `sepBy` (many space)
  return $ Free $ map read nid

new :: Monad m => ParsecT String u m Cmd
new = do
  string "new" >> many1 space
  nid <- integer
  many space
  synthNode <- optionMaybe nodeParam
  return $ New (read nid) synthNode

run :: Monad m => ParsecT String u m Cmd
run = do
  string "run" >> optional (many space)
  bool <- looseBool
  return $ Run bool

status :: Monad m => ParsecT String u m Cmd
status = string "status" >> return Status

refresh :: Monad m => ParsecT String u m Cmd
refresh = string "refresh" >> return Refresh

mv :: Monad m => ParsecT String u m Cmd
mv = do
  string "mv" >> optional (many space)
  addAct <- addActionNoReplace
  many1 space
  sourceId <- integer
  many1 space
  targetId <- integer
  return $ Mv addAct (read sourceId) (read targetId)

addAction :: Monad m => ParsecT String u m AddAction
addAction = try shortOpt <|> try longOpt
  where
    shortOpt = do
      char '-'
      choice [toHeadS, toTailS, beforeS, afterS, replaceS]
    longOpt = do
      string "--"
      choice [toHeadL, toTailL, beforeL, afterL, replaceL]

addActionNoReplace :: Monad m => ParsecT String u m AddAction
addActionNoReplace = try shortOpt <|> try longOpt
  where
    shortOpt = do
      char '-'
      choice [toHeadS, toTailS, beforeS, afterS]
    longOpt = do
      string "--"
      choice [toHeadL, toTailL, beforeL, afterL]

toHeadS, toTailS, beforeS, afterS, replaceS
  :: Monad m => ParsecT String u m AddAction
toHeadS  = char 'h' >> return AddToHead
toTailS  = char 't' >> return AddToTail
beforeS  = char 'b' >> return AddBefore
afterS   = char 'a' >> return AddAfter
replaceS = char 'r' >> return AddReplace

toHeadL, toTailL, beforeL, afterL, replaceL
  :: Monad m => ParsecT String u m AddAction
toHeadL  = string "head" >> return AddToHead
toTailL  = string "tail" >> return AddToTail
beforeL  = string "before" >> return AddBefore
afterL   = string "after" >> return AddAfter
replaceL = string "replace" >> return AddReplace

paths :: Monad m => ParsecT String u m [Step]
paths = try (absolutePath <|> relativePath) <|> return []

absolutePath :: Monad m => ParsecT String u m [Step]
absolutePath = do
  char '/'
  ps <- relativePath
  return $ GoTop : ps

relativePath :: Monad m => ParsecT String u m [Step]
relativePath = do
  res <- (pathUp <|> pathDown) `sepEndBy` char '/'
  return res

pathUp :: Monad m => ParsecT String u m Step
pathUp = string ".." >> return GoUp

pathDown :: Monad m => ParsecT String u m Step
pathDown = many1 digit >>= return . GoDown . read

generalName :: Monad m => ParsecT String u m String
generalName = many1 (alphaNum <|> oneOf "-_.")

nodeParam :: Monad m => ParsecT String u m (String,[SynthParam])
nodeParam = do
  synName <- generalName
  many space
  synParams <- synthParam `sepBy` (many1 space)
  return $ (synName,synParams)

synthParam :: Monad m => ParsecT String u m SynthParam
synthParam = do
  paraName <- generalName
  optional (many space) >> char '=' >> optional (many space)
  f <- choice [val, cmap, amap]
  optional (many space)
  return $ f paraName
  where
    val = do
      v <- float
      return $ (:= read v)
    cmap = do
      char 'c'
      b <- integer
      return $ (:<- read b)
    amap = do
      char 'a'
      b <- integer
      return $ (:<= read b)

integer :: Monad m => ParsecT String u m String
integer = do
  sign <- optionMaybe (char '-')
  val <- many1 digit
  case sign of
    Just sign' -> return $ sign':val
    Nothing    -> return val

float :: Monad m => ParsecT String u m String
float = do
  sign <- optionMaybe (char '-')
  val <- many1 (digit <|> oneOf ".e-")
  case sign of
    Just sign' -> return $ sign':val
    Nothing    -> return val

looseBool :: Monad m => ParsecT String u m Bool
looseBool = do
  bool <- try (true <|> false)
  return bool
  where
    true = do
      try (string "t" <|> string "T" <|> string "1" <|> string "on")
      return True
    false = do
      try (string "f" <|> string "F" <|> string "0" <|> string "off")
      return False

updateParams :: [SynthParam] -> SCNode -> SCNode
updateParams ps node = case node of
  Synth i n ps' -> Synth i n (unionBy ((==) `on` paramName) ps ps')
  g             -> g

paramName :: SynthParam -> ParamName
paramName x = case x of
  (n := _)  -> n
  (n :<- _) -> n
  (n :<= _) -> n
