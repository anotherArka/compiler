module Typed_parser where

import Typed_lambda
--import Untyped_Parser

import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.Bool
import Data.Maybe

data Context_Entry = Entry {
    name :: String ,
    ttype :: TType ,
    value :: Term
  }

-- These characters should not be part of a word
invalid :: [Char]
invalid = "!" 

-- contains list to_check checks if there is a character in to_check
-- which is also in list 
contains :: String -> String -> Bool
contains [] to_check = False
contains (ch : chs) to_check =
  if (elem ch to_check) then True else (contains chs to_check)

-- -- counts number of occurences of an element in a list
-- count :: Eq a => a -> [a] -> Int
-- count x [] = 0
-- count x (y : ys) = if (x == y) then (1 + (count x ys)) else (count x ys)

-- Continues to take a string until sees a particular character
take_until :: Eq a => a -> [a] -> [a]
take_until x [] = []
take_until x (y : ys) =
  if (x == y) then [] else (y : (take_until x ys))

-- spaces :: Parser ()
-- spaces = skipMany1 space

with_skip :: Parser a -> Parser a
with_skip p = do
  skipMany space
  p

parse_type :: Parser TType
parse_type = do
  skipMany space 
  x <- (try parse_constant 
        <|> try parse_identifier
        <|> try parse_function)
  skipMany space
  return x

parse_constant :: Parser TType
parse_constant = do
    x <- many1 letter
    case x of
      "Singleton" -> return Singleton
      "Empty" -> return Empty
      _ -> fail ("Unexpected name \"" ++ x ++ "\"")

parse_identifier :: Parser TType
parse_identifier = do
  char '('
  x <- parse_type
  op <- anyChar
  y <- parse_type
  char ')'
  case op of
    '+' -> return (Sum x y)
    '*' -> return (Product x y)
    _ -> fail ("Unexpected operator \"" ++ [op] ++ "\"")

parse_function :: Parser TType
parse_function = do
  char '('
  x <- parse_type
  string "->"
  y <- parse_type
  char ')'
  return (Function x y)
    