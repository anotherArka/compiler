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
  skipMany space
  char a
  skipMany space
  x <- p
  skipMany space
  char b
  skipMany space
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
  skipMany space 
  x <- (try (parse_constant context) 
    <|> try (parse_identifier context)
    <|> try (parse_function context))
  skipMany space
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

-- parse_term parses a term based on a contexts 
parse_term :: [(String, Term)] -> Parser Term 
parse_term context = do
  skipMany space
  x <- (try (parse_constant_term context) 
    <|> try (parse_app_term context)
    <|> try (parse_sum_term context)
    <|> try (parse_pair_term context))
    -- <|> try (parse_lambda_term   context bound_context term_type)
    -- <|> try (enclosed_in '(' ')' (parse_term context)))
  skipMany space  
  return x 

parse_constant_term :: [(String, Term)] -> Parser Term
parse_constant_term context = do
  x <- many1 letter
  y <- many (letter <|> digit) 
  case (x ++ y) of
    "unit" -> return Unit
    "void" -> return Void
      -- case term_type of
      -- (Function Empty t) -> return (Void t)
      -- _ -> fail ("Unexpected type " ++ (show term_type) ++ " of void")
    _ -> case (find (\el -> ((fst el) == (x ++ y))) context) of
      (Just found) -> return (snd found)
        -- if ((fst (snd found)) == term_type)
        -- then (return (snd (snd found)))
        -- else fail ("Expected type : " ++ (show term_type) ++
        --   "\n Found type : " ++ (show (fst (snd found))))                                                 
      Nothing -> fail ("Not found definition of " ++ (x ++ y))

parse_pair_term :: [(String, Term)] -> Parser Term
parse_pair_term context = do
    char '('
    x <- parse_term context
    char ','
    y <- parse_term context
    char ')'
    return (Pair x y)

parse_sum_term :: [(String, Term)] -> Parser Term
parse_sum_term context = do
    x <- many letter
    case x of
      "inr" -> do
        y <- enclosed_in '(' ')' (parse_term context)
        return (Inr y)
      "inl" -> do
        y <- enclosed_in '(' ')' (parse_term context)
        return (Inl y)
      _ -> fail ("Unrecongnised constructor " ++ x) 

-- parse_lambda_term :: [(String, (TType, Term))] -> [TType] -> TType -> Parser Term
-- parse_lambda_term context bound_context term_type = case term_type of
--   (Function s t) -> do
--     char '\\'
--     y <- parse_term context (s : bound_context) t
--     return (Lambda s y)

parse_app_term :: [(String, Term)] -> Parser Term
parse_app_term context = do
  char '('
  x <- (parse_term context)
  char '.'
  y <- (parse_term context)
  char ')'
  return (App x y)
