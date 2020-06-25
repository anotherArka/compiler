module Experiment where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf a) = show a
  show (Node a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"

parse_tree :: Parser (Tree String)
parse_tree = do
  x <- (try parse_node
    <|> try parse_leaf)
  return x

parse_leaf :: Parser (Tree String)
parse_leaf = do
  x <- many (letter <|> digit)
  return (Leaf x)

parse_node :: (Parser (Tree String))
parse_node = do
  char '('
  x <- parse_tree
  skipMany1 space
  y <- parse_tree
  char ')'
  return (Node x y)