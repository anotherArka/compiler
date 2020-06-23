module Context_free_grammer where

-- Zero and positive intergers correspond to non-terminals,
-- negative integers correspond to terminals. 
-- Although it is not possible to put this checks while
-- while defining the data type
-- using this a rule can be defined as a list of integers.
-- For example the rule A = aaAbB can be written as
-- (1, [-1,-1,1,-2,2])

data Context_free = Grammer {
  terminals :: [Char] ,
  non_terminals :: Int ,
  rules :: [(Int, [Int])]
}

-- Inputs in order are :
-- list of terminals
-- no. of non-terminals 
-- list of rules
-- index of the particular rule to be appiled
-- string to parse
parse_free :: [Char] -> Int -> [(Int, [Int])] -> Int -> String -> (Either String Int)
parse_free ter non_ter rules n [] =
  if ((n < 0) || (n >= length(rules))) then (Left ("Impossible rule no :" ++ (show n)))
  else (Left " ")

-- parse_rule :: [Char] -> Int -> (Int, [Int]) -> String -> (Maybe Int)
-- parse_rule ter non_ter (nt, []) [] = if (non_ter == nt) then (Just nt) else Nothing
-- parse_rule ter non_ter (nt, []) (s : ss) = Nothing
-- parse_rule ter non_ter (nt, (v : vs)) [] = Nothing
-- parse_rule ter non_ter (nt, (v : vs)) (ch : chs) =
--   if ((v > 0) && ((ter !! (- v - 1)) == ch)) then (parse_rule ter non_ter (nt, vs) chs)
--   else Nothing