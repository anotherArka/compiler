import Data.Char
--import Text.ParserCombinators.Parsec
import Untyped
import Data.List
import Data.Bool

--csvFile = endBy line eol
--line = sepBy cell (char '=')
--cell = many (noneOf "=\n")
--eol = char '\n'

--parseCSV :: String -> Either ParseError [[String]]
--parseCSV input = parse csvFile "(unknown)" input

-- 

is_there :: Char -> String -> Bool
is_there a [] = False
is_there a (x : xs) = (x == a) || (is_there a xs)

take_except :: String -> String -> [String] -> [String]    
take_except [] dont_take taken = reverse taken
take_except (x : xs) dont_take [] = 
    if (is_there x dont_take)
    then 
        take_except xs dont_take []
    else
        take_except xs dont_take [[x]]
take_except (x : xs) dont_take ([] : ys) = 
    if (is_there x dont_take) 
    then
        take_except xs dont_take ([] :  ys)
    else
        take_except xs dont_take ([x] : ys)
take_except (x : xs) dont_take ((y : ys) : yss) = 
    if (is_there x dont_take) 
    then
        take_except xs dont_take ([] : ((y : ys) : yss))
    else
        take_except xs dont_take ( ((y : ys) ++ [x]) : yss)
        
                 
