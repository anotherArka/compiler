module Junkyard where

data With_error a =
    Correct a |
    Error String
  deriving Show
  
instance Functor With_error where  
    fmap f (Error msg) = Error msg 
    fmap f (Correct x) = Correct (f x)    
    
instance Applicative With_error where  
    pure = Correct  
    (Error msg) <*> _ = Error msg  
    (Correct f) <*> x = fmap f x   
  
instance Monad With_error where
    (Correct val) >>= f = f val
    (Error   msg) >>= f = Error msg
    return x = Correct x
    fail msg = Error msg
    
-- separate_by from dont_take taken 
-- Divides the string "from" using characters in "dont_take" and adds that to "taken"
-- sample output
--  separate_by  "abcd" "bc" ["kl","mn"] = ["mn","kla","d"]
-- separate_by  "abcdefg" "bcf" [] = ["a","de","g"]
separate_by :: String -> String -> [String] -> [String]    
separate_by [] dont_take taken = reverse taken
separate_by (x : xs) dont_take [] = 
    if (elem x dont_take)
    then 
        separate_by xs dont_take []
    else
        separate_by xs dont_take [[x]]
separate_by (x : xs) dont_take ([] : ys) = 
    if (elem x dont_take) 
    then
        separate_by xs dont_take ([] :  ys)
    else
        separate_by xs dont_take ([x] : ys)
separate_by (x : xs) dont_take ((y : ys) : yss) = 
    if (elem x dont_take) 
    then
        separate_by xs dont_take ([] : ((y : ys) : yss))
    else
        separate_by xs dont_take ( ((y : ys) ++ [x]) : yss)    
