module Typed_parser where

import Typed_lambda
--import Untyped_Parser

import Data.Char
-- import Control.Monad.Fail
-- import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Token
-- import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.Bool
import Data.Maybe

data Context_Entry = Entry {
    name :: String ,
    ttype :: TType ,
    value :: Term
  }
----------------------------------------------------------------------------------------------------
-- data With_error a b = Parser (Either a b)

-- try_all :: [Parser (Either a b)] -> (Parser (Either a b))
-- try_all [] input = return (Left "Parse error on input : " ++ input)
-- try_all (p : ps) input = case (p input) of
--   (Left error) -> (try_all ps input)
--   (Right answer) -> (Right answer)

----------------------------------------------------------------------------------------------------
-- These characters should not be part of a word
invalid :: [Char]
invalid = "!#" -- # is reserved for bound variables

allowed :: Parser Char
allowed = oneOf "_"

enclosed_in :: Char -> Char -> Parser a -> Parser a
enclosed_in a b p = do
  char a
  -- skipMany space
  x <- p
  -- skipMany space
  char b
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

----------------------------------------------------------------------------------
---------- parsing of types ------------------------------------------------------
----------------------------------------------------------------------------------

-- the parse_type function gives a parser of the type based on the context
parse_type :: [(String, TType)] -> (Parser TType)
parse_type context = do
  x <- (try (parse_constant context)
    <|> try (parse_atomic context)
    <|> try (parse_identifier context))
  return x

parse_constant :: [(String, TType)] -> (Parser TType)
parse_constant context = do
    x <- letter
    y <- many (letter <|> digit <|> allowed)
    let
      z = (x : y)
    case z of
      "Singleton" -> return Singleton
      -- "Empty" -> return Typed_lambda.Empty
      _ -> case (find (\el -> (fst el) == z) context) of
          (Just t) -> return (snd t)
          Nothing -> unexpected ("Type " ++ z ++ " is not defined") 

parse_identifier :: [(String, TType)] -> (Parser TType)
parse_identifier context = do
  char '('
  x <- (parse_type context)
  skipMany1 space
  op <- anyChar
  skipMany1 space
  y <- (parse_type context)
  skipMany space
  char ')'
  case op of
    '+' -> return (Sum x y)
    '*' -> return (Product x y)
    '>' -> return (Function x y)
    _ -> unexpected ("Unexpected operator \"" ++ [op] ++ "\"")

parse_atomic :: [(String, TType)] -> (Parser TType)
parse_atomic context = do
  char '$'
  x <- letter
  y <- many (letter <|> digit <|> allowed)
  let
    z = (x : y)
  case (find (\el -> (fst el) == z) context) of
    (Just t) -> unexpected ("Type " ++ z ++ " is already defined")
    Nothing -> return (Atomic z)
    

-- parse_function :: [(String, TType)] -> (Parser TType)
-- parse_function context = do
--   char '('
--   x <- (parse_type context)
--   string "->"
--   y <- (parse_type context)
--   char ')'
--   return (Function x y)

----------------------------------------------------------------------------------
---------- parsing of terms ------------------------------------------------------
----------------------------------------------------------------------------------

data Parse_term_error = 
  Unknown |
  Undefined String |
  Repeated_usage String

-- compose_with_error :: ((Parser a) -> (Parser (Either Parse_term_error b))) ->
--   ((Parser (Either Parse_term_error a)) -> (Parser (Either Parse_term_error b)))
-- compose_with_error p input = case input of
--   (Left error) -> (Left error)
--   (Right inside) -> (p inside)

-- parse_term parses a term based on a context and a list of
-- name for bound variables 

type Parser = Parsec String ()

parse_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term 
parse_term types context bound_name = do
  -- skipMany space
  x <- (try (parse_constant_term types context bound_name)
    <|> try (parse_app_term types context bound_name) 
    <|> try (parse_sum_term types context bound_name)
    <|> try (parse_pair_term types context bound_name)
    <|> try (parse_lambda_term types context bound_name)
    -- <?> "Parse error")
    )
  -- skipMany space 
  return x 

parse_constant_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term
parse_constant_term types context bound_name = do
  x <- letter
  y <- many (letter <|> digit)        
  case (find (\el -> ((fst el) == ([x] ++ y))) context) of
    (Just found_def) -> case (elemIndex ([x] ++ y)  bound_name) of
      (Just bound_index) -> unexpected ("repeated use of name " ++ ([x] ++ y))
      Nothing -> return (snd found_def)
    Nothing -> case (elemIndex ([x] ++ y) bound_name) of
      (Just bound_index) -> return (Bound bound_index)
      Nothing -> fail ("Not found definition of " ++ ([x] ++ y))
      
parse_pair_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term
parse_pair_term types context bound_name = do
  char '('
  skipMany space
  x <- parse_term types context bound_name
  skipMany space
  char ','
  skipMany space
  y <- parse_term types context bound_name
  skipMany space
  char ')'
  return (Pair x y)

parse_sum_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term
parse_sum_term types context bound_name = do
    char '('
    x <- many1 letter
    skipMany1 space
    case x of
      "inr" -> do
        y <- parse_term types context bound_name
        skipMany space
        char '+'
        skipMany space
        t <- parse_type types
        char ')'
        return (Inr y t)
      "inl" -> do
        y <- parse_term types context bound_name
        skipMany space
        char '+'
        skipMany space
        t <- parse_type types
        skipMany space
        char ')'
        return (Inl y t)
      _ -> unexpected ("constructor " ++ x)

parse_lambda_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term
parse_lambda_term types context bound_name = do
  char '/'
  x <- many1 letter
  y <- many (letter <|> digit <|> allowed)
  skipMany space
  char ':'
  t <- parse_type types
  skipMany space
  char '.'
  skipMany space
  inside <- parse_term types context ((x ++ y) : bound_name)
  case (find (== (x ++ y)) (((fmap fst) context) ++ bound_name)) of
    (Just found) -> fail ("repeated use of name " ++ (x ++ y))
    Nothing -> return (Lambda t inside)

parse_app_term :: [(String, TType)] -> [(String, Term)] -> [String] -> Parser Term
parse_app_term types context bound_name = do
  char '('
  skipMany space
  x <- parse_term types context bound_name
  skipMany1 space
  y <- parse_term types context bound_name
  skipMany space
  char ')'
  return (App x y)

starting_context = [("unit", Unit)]  

basic_term_parser = parse ((parse_term [] starting_context []) <* eof) "error:"