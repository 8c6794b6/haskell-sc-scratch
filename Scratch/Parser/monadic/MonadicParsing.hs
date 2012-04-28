{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch written while reading: /FUNCTIONAL PEARLS Monadic Parsing in
Haskell/.

-}
module MonadicParsing where

import Control.Applicative
import Control.Monad
import Data.Char

--------------------------------------------------------------------------
-- A type for parsers

newtype Parser a = Parser (String -> [(a, String)])

-- | Unwrapper for parser. Run the parser with given string.
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

-- --------------------------------------------------------------------------
-- A monad of parsers

item :: Parser Char
item = Parser $ \cs -> case cs of
  ""     -> []
  (c:cs) -> [(c,cs)]

instance Monad Parser where
  return a = Parser $ \cs -> [(a,cs)]
  p >>= f  = Parser $ \cs -> concat [parse (f a) cs'|(a, cs') <- parse p cs]

-- | Example parser to consume three characters, throw away the second
-- character and return the other two as a pair.
p :: Parser (Char, Char)
p = do {a <- item; item; b <- item; return (a,b)}

-- | 'mzero' is empty list, and 'mplus' is concatenation of parsed results.
instance MonadPlus Parser where
  mzero = Parser $ \_ -> []
  p `mplus` q = Parser $ \cs -> parse p cs ++ parse q cs

-- --------------------------------------------------------------------------
-- Non monadic instances: Functor, Applicative, and Alternative.

instance Functor Parser where
  fmap f p = Parser $ \cs -> [(f a, cs')|(a,cs') <- parse p cs]

instance Applicative Parser where
  pure x = Parser $ \cs -> [(x,cs)]
  p <*> q = Parser $ \cs ->
    [(f a,cs'') |(f,cs') <- parse p cs, (a,cs'') <- parse q cs']

instance Alternative Parser where
  p <|> q = Parser $ \cs -> case parse p cs of
    [] -> parse q cs
    rs -> rs
  empty = Parser $ \_ -> []

-- --------------------------------------------------------------------------
-- Combinator functions

-- | Variant of 'mplus', which returns only the first element from
-- 'mplus' result.
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case parse (p `mplus` q) cs of
  []   -> []
  x:xs -> [x]

-- | A combinator that takes a predicate, and yields a parser that
-- consumes a single character if it satisfies the
-- predicate. Implementation is using 'item' and simply comparing the
-- consumed input, and returning 'zero' when not satisfied.
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

-- | A parser for specific characters.
char :: Char -> Parser Char
char c = sat (== c)

-- | Parse a specific string.
--
-- Recursive call to itself is written in the body of this function.
--
string :: String -> Parser String
string cs = case cs of
  []     -> return []
  c:rest -> char c *> string rest *> return cs

{-
Instead of making our own 'many' and 'many1', we can use the same
function from Applicative.

'many' parse repeated applications of given parser. Permits zero or more
applications. 'many1' permits one or more applications.

-- many :: Parser a -> Parser [a]
-- many p = many1 p +++ return []

-- many1 :: Parser a -> Parser [a]
-- many1 p = do {a <- p; as <- many p; return (a:as)}
-}

-- | Parse repeated applications of first parser, separated by
-- applications of a second parser. Permits zero or mor applications.
sepby ::
  Parser a    -- ^ Repeatedly applied parser.
  -> Parser b -- ^ Separation parser, result will be thrown away.
  -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

-- | Parse repeated applications of first parser, separated by
-- applications of a second parser. Permits one or mor applications.
sepby1 ::
  Parser a    -- ^ Repeatedly applied parser.
  -> Parser b -- ^ Separation parser, result will be thrown away.
  -> Parser [a]
p `sepby1` sep = (:) <$> p <*> many (sep *> p)

-- | Parse repeated applications of first parser, separated by
-- applications of second parser whose result is an operator that is
-- assumed to associate to the left, and which is used to combine the
-- results from the first parsers. Permits zero or more applications.
chainl ::
  Parser a
  -- ^ Repeatedly applied parser.
  -> Parser (a -> a -> a)
  -- ^ Parser for separation operator. Result will applied
  --  to result of first parser.
  -> a
  -- ^ Default value.
  -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- | Variant of chainl, permits one or more applications.
p `chainl1` op = p >>= rest where
  rest a = (do {f <- op; b <- p; rest (f a b)}) <|> return a

-- | Parse a string of spaces, tabs, and newlines.
space :: Parser String
space = many (sat isSpace)

-- | Parse a token using given parser, throwing away any /trailing/ space.
token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

-- | Parse a symbolic token
symb :: String -> Parser String
symb cs = token (string cs)

-- | Apply given parser, throwing away any /leading/ space.
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

-- --------------------------------------------------------------------------
-- Example
--
-- Standard grammer for arithmetic expressions built up from single
-- digits using the operators +, -, * and /, together with parentheses.
--
--   expr ::= expr addop term | term
--   term ::= term mulop factor | factor
--   factor ::= digit | (expr)
--   digit ::= 0 | 1 | ... | 9
--
--   addop ::= + | -
--   mulop ::= * | /
--
-- Sample run:
--
--   ghci> parse expr "1 - 2 * 3 + 4"
--   [(-1,"")]

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit <|> (symb "(" *> expr <* symb ")")

digit :: Parser Int
digit = (\x -> ord x - ord '0') <$> token (sat isDigit)

addop :: Parser (Int -> Int -> Int)
addop = (symb "+" *> return (+)) <|> (symb "-" *> return (-))

mulop :: Parser (Int -> Int -> Int)
mulop = (symb "*" *> return (*)) <|> (symb "/" *> return div)
