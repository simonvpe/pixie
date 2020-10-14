{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Control.Applicative
import Control.Monad
import Data.Char

-- Structurally a parser is a function which takes an input stream of character
-- yields a parse tree by applying the parser logic over sections of the
-- character stream (called lexemes) to build up a composite data structure
-- for the AST.
newtype Parser a = Parser {parse :: String -> [(a, String)]}

-- Running the function will result in traversing the stream of characters
-- yielding a value of type `a` that usually represents the AST for the parsed
-- expression, or failing with a parse error for malformed input, or failing
-- by not consuming the entire stream of input. A more robust implementation
-- would track the position information of failures for error reporting.
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Parser did not consume entire stream."
    _ -> error "Parser error."

-- We advance the parser by extracting a single character from the parser
-- stream and returning in a tuple containing itself and the rest of the
-- stream. The parser logic will then scrutinize the character and either
-- transform it in some portion of the output or advance the stream and
-- proceed.
item :: Parser Char
item = Parser $ \case
  [] -> []
  (c : cs) -> [(c, cs)]

-- A bind operation for our parser type will take one parse operation and
-- compose it over the result of second parse function. Since the parser
-- operation yields a list of tuples, composing a second parser function
-- simply maps itself over the resulting list and concat's the resulting
-- nested list of lists into a single flat list in the usual list monad
-- fashion. The unit operation injects a single pure value as the result,
-- without reading from the parse stream.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- As the terminology might have indicated this is indeed a Monad (also
-- a Functior and Applicative)
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

-- Of particular importance is that this particular monad has a zero
-- value (failure), namely the function which halts reading the stream
-- and returns the empty stream. Together this forms a monoidal structure
-- with a secondary operation (combine) which applies two parser functions
-- over the same stream and concatenates the result. Together these give
-- rise to both the Alternative and MonadPlus class instances which encode
-- the logic for trying multiple parse functions over the same stream and
-- handling failure and rollover.
--
-- The core operator introduced here is the (<|>) operator for combining
-- two optional paths of parser logic, switching to the second path if the
-- first fails with the zero value.

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

-- Derived automatically from the Alternative typeclass definition are the
-- `many` and `some` functions. Many takes a single function argument and
-- repeatedly applies it until the function fails and then yields the
-- collected results up to that point.

-- One or more.
some :: Alternative f => f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- Zero or more.
many :: Alternative f => f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- On top of this we can add functionality for checking whether the current
-- character in the stream matches a given predicate (i.e. is it a digit,
-- is it a letter, a specific word, etc).
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
    then unit c
    else Parser $ const []

-- Essentially this 50 lines of code encodes the entire core of the parser
-- combinator machinery. All higher order behavior can be written on top of
-- just this logic. Now we can write down several higher level functions
-- which operate over sections of the stream. `chain` parses one or more
-- occurances of `p`, separated by `op` and returns a value obtained by
-- recursing until failure on the left hand side of the stream. This can
-- be used to parse left-recursive grammar.
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p; rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a
