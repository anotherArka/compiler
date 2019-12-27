module Playground where

import Data.Char
--import Text.ParserCombinators.Parsec
import Untyped
import Data.List
import Data.Bool
import Untyped_Parser

-----------------------------------------------------
my_I = parse_expression ("/x.(x) (x)")
my_K = parse_expression ("/x./y.(x) (y)")
ex_1 = parse_expression ("/x./y./z.((z)(y))(x)")
ex_2 = App ex_1 (Variable "a")
ex_3 = App ex_2 (Variable "a")
ex_4 = parse_expression ("/p./q.q")
ex_5 = App ex_3 ex_4
