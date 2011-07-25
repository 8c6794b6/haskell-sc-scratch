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
module Sound.SC3.Lepton.CLI.HscShell.Parser
  ( -- * Commands syntax in BNF like expression
    -- $syntax
    parseCmd
  , parsePaths
  ) where

import Control.Applicative ((<$>))
import Text.Parsec
import Text.Show.Functions ()

import Sound.SC3 hiding (free, status)
import Sound.SC3.Lepton

import Sound.SC3.Lepton.CLI.HscShell.Cmd

-- $syntax
--
-- > commands ::= cd | free | gnew | ls | mv | pwd | quit
-- >            | refresh | run | set | snew | status | tree
-- >
-- > cd       ::= "cd" spaces paths
-- > paths    ::= path | path "/" paths
-- > path     ::= "." | ".." | "/" | nodeid
-- >
-- > free     ::= "free" spaces nodeids
-- > nodeids  ::= nodeid | nodeid spaces nodeids
-- > nodeid   ::= integer
-- >
-- > gnew               ::= "gnew" spaces nodeid-opt-actions
-- > nodeid-opt-actions ::= nodeid-opt-action
-- >                      | nodeid-opt-action spaces nodeid-opt-actions
-- > nodeid-opt-action  ::= nodeid | nodeid action nodeid
-- > action             ::= "a" | "b" | "h" | "r" | "t"
-- >
-- > ls ::= "ls" | "ls" spaces paths
-- >
-- > mv          ::= "mv" spaces action-flag nodeid nodeid
-- > action-flag ::= "-a" | "--after" | "-b" | "--before"
-- >               | "-h" | "--head"  | "-t" | "--tail"
-- >
-- > pwd ::= "pwd"
-- >
-- > quit ::= "quit"
-- >
-- > refresh ::= "refresh"
-- >
-- > run  ::= "run" spaces bool
-- > bool ::= "t" | "f"
-- >
-- > set         ::= "set" spaces param-pairs
-- >               | "set" spaces nodeid param-pairs
-- > param-pairs ::= param-pair | param-pair spaces param-pairs
-- > param-pair  ::= paramname opt-spaces "=" opt-spaces value
-- > paramname   ::= param name in synthdef
-- > value       ::= float | "c" integer | "a" integer
-- >
-- > snew    ::= "snew" spaces defname nodeid
-- >           | "snew" spaces defname nodeid param-pairs
-- > defname ::= synthdef name in SC_SYNTHDEF_PATH
-- >
-- > status ::= "status"
-- >
-- > tree         ::= "tree"
-- >                | "tree" spaces paths
-- >                | "tree" spaces verbose-flag
-- >                | "tree" spaces verbose-flag paths
-- > verbose-flag ::= "-v" | "--verbose"
-- >
-- > integer            ::= integer number
-- > float              ::= float number
-- > spaces             ::= " " | " " spaces
-- > opt-spaces         ::= ""  | " " opt-spaces
-- >

-- | Parser for command.
parseCmd :: String -> Either ParseError Cmd
parseCmd = runParser command () "parseCmd" . clean where
  clean = reverse . (nw . reverse) . nw
  nw = dropWhile (== ' ')

-- | Parser for path.
parsePaths :: String -> Either ParseError [Step]
parsePaths = runParser paths () "parsePaths" . clean where
  clean = reverse . (nw . reverse) . nw
  nw = dropWhile (== ' ')

command :: Monad m => ParsecT String u m Cmd
command = do
  optional (many space)
  result <- choice $ map try commands
  optional (many space)
  return result

commands :: Monad m => [ParsecT String u m Cmd]
commands =
  [pwd, ls, cd, tree, set, free, snew, gnew, run, status, mv, refresh]

pwd :: Monad m => ParsecT String u m Cmd
pwd = string "pwd" >> return Pwd

ls :: Monad m => ParsecT String u m Cmd
ls = string "ls" >> optional (many space) >> Ls <$> paths

cd :: Monad m => ParsecT String u m Cmd
cd = string "cd" >> optional (many space) >> Cd <$> paths

tree :: Monad m => ParsecT String u m Cmd
tree = do
  string "tree" >> optional (many space)
  verbose <- verboseFlag
  optional (many space)
  Tree verbose <$> paths

verboseFlag :: Monad m => ParsecT String u m Bool
verboseFlag =
  ((try (string "-v" >> many1 space) <|>
    try (string "--verbose")) >>
   return True) <|>
  return False

set :: Monad m => ParsecT String u m Cmd
set = do
  string "set" >> many space
  nid <- optionMaybe integer
  many space
  ps <- synthParam `sepBy` (many space)
  return $ Set nid ps

free :: Monad m => ParsecT String u m Cmd
free = do
  string "free" >> many1 space
  nid <- integer `sepBy` (many space)
  return $ Free nid

snew :: Monad m => ParsecT String u m Cmd
snew = do
  string "snew" >> many1 space
  defName <- generalName
  many1 space
  (nid,aaPair) <- nidWithAddAction
  optional (many space)
  synParams <- synthParam `sepBy` (many space)
  return $ Snew defName nid aaPair synParams

gnew :: Monad m => ParsecT String u m Cmd
gnew = string "gnew" >> many1 space >>
  Gnew <$> (nidWithAddAction `sepBy` (many space))

run :: Monad m => ParsecT String u m Cmd
run = string "run" >> optional (many space) >> Run <$> bool

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
  return $ Mv addAct sourceId targetId

nidWithAddAction :: Monad m =>
                    ParsecT String u m (NodeId,Maybe (AddAction,NodeId))
nidWithAddAction = do
  nid <- integer
  pair <- optionMaybe addActionAndTarget
  return (nid,pair)

addActionAndTarget :: Monad m => ParsecT String u m (AddAction,NodeId)
addActionAndTarget = do
  a <- choice [char 'a' >> return AddAfter
              ,char 'b' >> return AddBefore
              ,char 'h' >> return AddToHead
              ,char 'r' >> return AddReplace
              ,char 't' >> return AddToTail]
  i <- integer
  return (a,i)

addActionNoReplace :: Monad m => ParsecT String u m AddAction
addActionNoReplace = try shortOpt <|> try longOpt
  where
    shortOpt = do
      char '-'
      choice [toHeadS, toTailS, beforeS, afterS]
    longOpt = do
      string "--"
      choice [toHeadL, toTailL, beforeL, afterL]

toHeadS, toTailS, beforeS, afterS
  :: Monad m => ParsecT String u m AddAction
toHeadS  = char 'h' >> return AddToHead
toTailS  = char 't' >> return AddToTail
beforeS  = char 'b' >> return AddBefore
afterS   = char 'a' >> return AddAfter

toHeadL, toTailL, beforeL, afterL
  :: Monad m => ParsecT String u m AddAction
toHeadL  = string "head" >> return AddToHead
toTailL  = string "tail" >> return AddToTail
beforeL  = string "before" >> return AddBefore
afterL   = string "after" >> return AddAfter

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
generalName =
  notFollowedBy digit >> many1 (alphaNum <|> oneOf "-_.")

synthParam :: Monad m => ParsecT String u m SynthParam
synthParam = do
  paraName <- generalName
  optional (many space) >> char '=' >> optional (many space)
  f <- choice [fval, cmap, amap]
  optional (many space)
  return $ f paraName
  where
    fval = float >>= return . flip (:=)
    cmap = char 'c' >> integer >>= return . flip (:<-)
    amap = char 'a' >> integer >>= return . flip (:<-)

integer :: Monad m => ParsecT String u m Int
integer = do
  sign <- optionMaybe (char '-')
  val <- many1 digit
  case sign of
    Just sign' -> return $ read $ sign':val
    Nothing    -> return $ read $ val

float :: Monad m => ParsecT String u m Double
float = do
  sign <- optionMaybe (char '-')
  val <- many1 (digit <|> oneOf ".e-")
  case sign of
    Just sign' -> return $ read $ sign':val
    Nothing    -> return $ read val

bool :: Monad m => ParsecT String u m Bool
bool = do
  b <- try (true <|> false)
  return b
  where
    true = do
      try (string "t" <|> string "T" <|> string "1" <|> string "on")
      return True
    false = do
      try (string "f" <|> string "F" <|> string "0" <|> string "off")
      return False
