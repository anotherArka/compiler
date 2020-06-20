module Context_free_grammer where

-- Zero and positive intergers correspond to non-terminals,
-- negative integers correspond to terminals. 
-- Although it is not possible to put this checks while
-- while defining the data type
-- using this a rule can be defined as a list of integers.
-- For example the rule A = aaAbB can be written as
-- (1, [-1,-1,1,-2,2])

data Context_Free = Grammer {
  terminals :: Int ,
  non_terminals :: Int ,
  rules :: [Int, [Int]]
}