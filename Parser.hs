-- Monadic Parsing
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P {parse :: String -> [(a, String)]}

-- consumes a single character if the input string is non-empty
item :: Parser Char
item = P $ \case
  [] -> []
  (x : xs) -> [(x, xs)]

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P $ \inp -> case parse p inp of
    [] -> []
    [(v, out)] -> [(g v, out)]

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P $ \inp -> [(v, inp)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P $ \inp -> case parse pg inp of
    [] -> []
    [(g, out)] -> parse (fmap g px) out

instance Monad Parser where
  -- return v always succeeds with the result value v
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \inp -> case parse p inp of
    [] -> []
    [(v, out)] -> parse (f v) out

instance Alternative Parser where
  -- many :: f a -> f [a]
  -- many x = some x <|> pure []

  -- some :: f a -> f [a]
  -- some x = pure (:) <*> x <*> many x

  -- always fails
  empty :: Parser a
  empty = P $ const []

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]

-- parser for single characters that satisfy the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- parser for single digits
digit :: Parser Char
digit = sat isDigit

-- parser for lower-case letters
lower :: Parser Char
lower = sat isLower

-- parser for upper-case letters
upper :: Parser Char
upper = sat isUpper

-- parser for arbitrary letters
letter :: Parser Char
letter = sat isAlpha

-- parser for alphanumeric characters
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- parser for specific characters
char :: Char -> Parser Char
char x = sat (== x)

-- parser for specific strings
string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- parser for identifiers
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

-- parser for natural numbers
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- parser for spaces
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

-- parser for integers
int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- parse token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- parse identifier
identifier :: Parser String
identifier = token ident

-- parse natural
natural :: Parser Int
natural = token nat

-- parse integer
integer :: Parser Int
integer = token int

-- parse symbol
symbol :: String -> Parser String
symbol xs = token (string xs)

