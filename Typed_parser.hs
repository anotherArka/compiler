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
invalid = "!#" -- # is reserved for bound variables

enclosed_in :: Char -> Char -> Parser a -> Parser a
enclosed_in a b p = do
  -- skipMany space
  char a
  -- skipMany space
  x <- p
  -- skipMany space
  char b
  -- skipMany space
  return x

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

---------- parsing of types ------------------------------------------------------

-- the parse_type function gives a parser of the type based on the context
parse_type :: [(String, TType)] -> (Parser TType)
parse_type context = do
  -- skipMany space 
  x <- (try (parse_constant context) 
    <|> try (parse_identifier context)
    <|> try (parse_function context))
  -- skipMany space
  return x

parse_constant :: [(String, TType)] -> (Parser TType)
parse_constant context = do
    x <- many1 letter
    case x of
      "Singleton" -> return Singleton
      "Empty" -> return Empty
      _ -> case (find (\el -> (fst el) == x) context) of
          (Just t) -> return (snd t)
          Nothing -> fail ("Not found type " ++ x ++ " in context") 

parse_identifier :: [(String, TType)] -> (Parser TType)
parse_identifier context = do
  char '('
  x <- (parse_type context)
  op <- anyChar
  y <- (parse_type context)
  char ')'
  case op of
    '+' -> return (Sum x y)
    '*' -> return (Product x y)
    _ -> fail ("Unexpected operator \"" ++ [op] ++ "\"")

parse_function :: [(String, TType)] -> (Parser TType)
parse_function context = do
  char '('
  x <- (parse_type context)
  string "->"
  y <- (parse_type context)
  char ')'
  return (Function x y)

---------- parsing of terms ------------------------------------------------------

-- parse_term parses a term based on a context and a list of
-- name for bound variables 
parse_term :: [(String, Term)] -> [String] -> Parser Term 
parse_term context bound_name = do
  -- skipMany space
  x <- (try (parse_constant_term context bound_name) 
    <|> try (parse_app_term context bound_name)
    <|> try (parse_sum_term context bound_name)
    <|> try (parse_pair_term context bound_name))
    -- <|> try (parse_lambda_term   context bound_context term_type)
    -- <|> try (enclosed_in '(' ')' (parse_term context)))
  -- skipMany space  
  return x 

parse_constant_term :: [(String, Term)] -> [String] -> Parser Term
parse_constant_term context bound_name = do
  x <- many1 letter
  y <- many (letter <|> digit) 
  case (x ++ y) of
    "unit" -> return Unit
    "void" -> return Void     
    _ -> case (find (\el -> ((fst el) == (x ++ y))) context) of
      (Just found_def) -> case (elemIndex (x ++ y) bound_name) of
        (Just bound_index) -> fail ("Repeated use of name " ++ (x ++ y))
        Nothing -> return (snd found_def)                                
      Nothing -> case (elemIndex (x ++ y) bound_name) of
        (Just bound_index) -> return (Bound bound_index)
        Nothing -> fail ("Not found definition of " ++ (x ++ y))

parse_pair_term :: [(String, Term)] -> [String] -> Parser Term
parse_pair_term context bound_name = do
    char '('
    x <- parse_term context bound_name
    char ','
    y <- parse_term context bound_name
    char ')'
    return (Pair x y)

parse_sum_term :: [(String, Term)] -> [String] -> Parser Term
parse_sum_term context bound_name = do
    x <- many letter
    case x of
      "inr" -> do
        y <- enclosed_in '(' ')' (parse_term context bound_name)
        return (Inr y)
      "inl" -> do
        y <- enclosed_in '(' ')' (parse_term context bound_name)
        return (Inl y)
      _ -> fail ("Unrecongnised constructor " ++ x) 

parse_lambda_term :: [(String, Term)] -> [String] -> Parser Term
parse_lambda_term context bound_name = do
  char '\\'
  var_name <- many (letter <|> digit)
  char '.'
  inside <- parse_term context (var_name : bound_name)
  return (Lambda inside)

parse_app_term :: [(String, Term)] -> [String] -> Parser Term
parse_app_term context bound_name = do
  char '('
  x <- (parse_term context bound_name)
  skipMany1 space
  y <- (parse_term context bound_name)
  char ')'
  return (App x y)
