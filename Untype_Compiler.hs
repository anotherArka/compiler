import Data.Char
--import Text.ParserCombinators.Parsec
import Untyped
import Data.List
import Data.Bool
import Untyped_Parser

data Command_Type = 
    Define String Term | 
    Print_Term Term | 
    Print_Var String | 
    Evaluate Nat Term


