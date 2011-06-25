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
module Sound.SC3.Lepton.CLI.Parser (Cmd(..), parseCmd) where

import Data.List (unionBy)
import Data.Function (on)
import Text.Parsec
import Text.Show.Functions ()

import Sound.SC3 hiding (free, status)
import Sound.SC3.Lepton

import Sound.SC3.Lepton.CLI.SCZipper

-- | Commands for sc shell.
data Cmd = Pwd                                      -- ^ view current status
         | Ls NodePath                              -- ^ view node list
         | Cd NodePath                              -- ^ move around tree
         | Mv AddAction NodeId NodeId               -- ^ move nodes
         | Tree NodePath                            -- ^ view node tree
         | Status                                   -- ^ show server status
         | Refresh                                  -- ^ refresh synth tree
         | Set NodeId (SCNode->SCNode)              -- ^ n_set
         | Run Bool                                 -- ^ n_run
         | Free [NodeId]                            -- ^ n_free
         | New NodeId (Maybe (String,[SynthParam])) -- ^ s_new and g_new
         deriving (Show)

type NodePath = SCZipper -> SCZipper

-- | Parser for command
parseCmd :: String -> Either ParseError Cmd
parseCmd = runParser commands () "interactive"

commands :: Monad m => ParsecT String u m Cmd
commands = do
  optional (many space)
  result <- choice $ map try commandNames
  optional (many space)
  return result

commandNames :: Monad m => [ParsecT String u m Cmd]
commandNames =
  [pwd, ls, cd, tree, set, free, new, run, status, mv, refresh]

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
  nid <- many1 digit
  many space
  ps <- synthParam `sepBy` (many space)
  return $ Set (read nid) (updateParams ps)

free :: Monad m => ParsecT String u m Cmd
free = do
  string "free" >> many1 space
  nid <- many1 digit `sepBy` space
  return $ Free $ map read nid

new :: Monad m => ParsecT String u m Cmd
new = do
  string "new" >> many1 space
  nid <- many1 digit
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
  addAct <- addAction
  many1 space
  sourceId <- many1 digit
  many1 space
  targetId <- many1 digit
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
    toHeadS  = char 'h' >> return AddToHead
    toHeadL  = string "head" >> return AddToHead
    toTailS  = char 't' >> return AddToTail
    toTailL  = string "tail" >> return AddToTail
    beforeS  = char 'b' >> return AddBefore
    beforeL  = string "before" >> return AddBefore
    afterS   = char 'a' >> return AddAfter
    afterL   = string "after" >> return AddAfter
    replaceS = char 'r' >> return AddReplace
    replaceL = string "replace" >> return AddReplace

paths :: Monad m => ParsecT String u m NodePath
paths = try (absolutePath <|> relativePath) <|> return id

absolutePath :: Monad m => ParsecT String u m NodePath
absolutePath = do
  char '/'
  f <- relativePath
  return $ f . goTop

relativePath :: Monad m => ParsecT String u m NodePath
relativePath = do
  res <- (pathUp <|> pathDown) `sepEndBy` char '/'
  return $ foldr (.) id $ reverse res

pathUp :: Monad m => ParsecT String u m NodePath
pathUp = string ".." >> return goUp

pathDown :: Monad m => ParsecT String u m NodePath
pathDown = many1 digit >>= return . goDown . read

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
  return $ f paraName
  where
    val = do
      v <- many1 (digit <|> oneOf "-.")
      return $ (:= read v)
    cmap = do
      char 'c'
      b <- many1 digit
      return $ (:<- read b)
    amap = do
      char 'a'
      b <- many1 digit
      return $ (:<= read b)

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
