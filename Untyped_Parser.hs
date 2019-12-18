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
--------------------------------------------------------------------------------------------------------------------------------
-- is_there a xs = return true is a is inside xs, false otherwise
is_there :: Char -> String -> Bool
is_there a [] = False
is_there a (x : xs) = (x == a) || (is_there a xs)

--------------------------------------------------------------------------------------------------------------------------------

-- separate_by from dont_take taken 
-- Divides the string "from" using characters in "dont_take" and adds that to "taken"
-- sample output
--  separate_by  "abcd" "bc" ["kl","mn"] = ["mn","kla","d"]
-- separate_by  "abcdefg" "bcf" [] = ["a","de","g"]
separate_by :: String -> String -> [String] -> [String]    
separate_by [] dont_take taken = reverse taken
separate_by (x : xs) dont_take [] = 
    if (is_there x dont_take)
    then 
        separate_by xs dont_take []
    else
        separate_by xs dont_take [[x]]
separate_by (x : xs) dont_take ([] : ys) = 
    if (is_there x dont_take) 
    then
        separate_by xs dont_take ([] :  ys)
    else
        separate_by xs dont_take ([x] : ys)
separate_by (x : xs) dont_take ((y : ys) : yss) = 
    if (is_there x dont_take) 
    then
        separate_by xs dont_take ([] : ((y : ys) : yss))
    else
        separate_by xs dont_take ( ((y : ys) ++ [x]) : yss)
     
------------------------------------------------------------------------------------------------------------------   
        
--  take_except from dont_take
-- filters the characters in "from" which are in "dont_take"       
take_except :: String -> String -> String
take_except [] dont_take = []
take_except (x : xs) dont_take = 
    if (is_there x dont_take)
    then
        take_except xs dont_take 
    else
        x : (take_except xs dont_take)          

-------------------------------------------------------------------------------------------------------------------

-- parses a lambda expression given we know it is a valid one already
parse_lambda :: String -> [String]
parse_lambda input = separate_by input "\\." []

-------------------------------------------------------------------------------------------------------------------

-- match_parenthesis :: String -> Int -> Int -> Int
-- match_parenthesis [] pos height = 0
-- match_parenthesis (x : xs) pos height = 
   -- if (x == ')' && height == 0) then (pos + 1)
   -- else if (x == ')' && height > 0) then match_parenthesis xs (pos + 1) (height - 1)
   -- else if (x == '(') then match_parenthesis xs (pos + 1) (height + 1)
   -- else match_parenthesis xs (pos + 1) height
    
------------------------------------------------------------------------------------------------------------------- 

-- matches parenthesis until the string is finished or no of ')' is more than no of '('   
match_parenthesis :: String -> Int -> [Int] -> [(Int, Int)]
match_parenthesis [] pos not_matched = []
match_parenthesis (x : xs) pos [] = 
    if (x == '(') then match_parenthesis xs (pos + 1) [pos]
    else if (x == ')') then []
    else match_parenthesis xs (pos + 1) []
match_parenthesis (x : xs) pos (n : ns) = 
    if (x == '(') then match_parenthesis xs (pos + 1) (pos : (n : ns))
    else if (x == ')') then ((match_parenthesis xs (pos + 1) ns) ++ [(n, pos)])
    else match_parenthesis xs (pos + 1) (n : ns)
    
-------------------------------------------------------------------------------------------------------------------

get_value_from_key :: Int -> [(Int, ty)] -> ty
get_value_from_key n ((m, a) : xs) = if (n == m) 
    then a
    else get_value_from_key n xs

-------------------------------------------------------------------------------------------------------------------

-- takes the string between index m and n. Notice that it starts counting from zero
take_out_using_index :: String -> (Int, Int) -> String
take_out_using_index [] (m, n) = []
take_out_using_index (x : xs) (m, n) = 
    if (m == 0 && n == 0) then [x]
    else if (m > 0 && n > 0) then take_out_using_index xs (m - 1, n - 1)
    else if (m == 0 && n > 0) then (x : (take_out_using_index xs (0, n -1)))
    else []                  

-------------------------------------------------------------------------------------------------------------------

-- parses the function and argument part of a term, given we know that it is an application
parse_application :: String -> (String, String)
parse_application input = let
    func_index = (0, get_value_from_key 0 (match_parenthesis input 0 []))
    func = take_out_using_index input func_index
    args = take_out_using_index input ( (snd func_index) + 1, (length input) - 1)
    func1 = omit_whitespaces func
    args1 = omit_whitespaces args
    in
    (func1, args1)
-------------------------------------------------------------------------------------------------------------------

omit_whitespaces :: String -> String
omit_whitespaces st = take_except st " "

-------------------------------------------------------------------------------------------------------------------  
 
 
 
 
 
 
 
 
 
 
 
        
                 
