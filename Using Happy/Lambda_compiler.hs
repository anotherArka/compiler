module Lambda_compiler where

import Data.List
import Lambda_happy
import Lambda_system
-- Note --
-- Now the parser only accepts alphabets and not numbers inside variable names --

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

-- Tries to compute the term from the raw term computed by the parser,
-- given the context and the list of bound variables

cook_term :: [(String, Term)] -> [String] -> Raw_term -> (With_error Term)
cook_term ctxt bound_ctxt (Var var) =
  case (find (\entry -> ((fst entry) == var)) ctxt) of
    (Just found_def) -> case (elemIndex var bound_ctxt) of
      (Just bound_index) -> Error ("repeated use of name " ++ var)
      Nothing -> Correct (snd found_def)
    Nothing -> case (elemIndex var bound_ctxt) of
      (Just bound_index) -> Correct (Bound bound_index)
      Nothing -> Error (var ++ " is undefined")            
cook_term ctxt bound_ctxt (Raw_app func args) = do
  f <- cook_term ctxt bound_ctxt func
  x <- cook_term ctxt bound_ctxt args
  return (App f x)
cook_term ctxt bound_ctxt (Raw_lambda bound_var inside) =
  if (elem bound_var bound_ctxt) then (Error (bound_var ++ " is already bound"))
  else case (find (\entry -> ((fst entry) == bound_var)) ctxt ) of
    Just entry -> Error (bound_var ++
      " is already defined and can not be used as a bound variable")
    Nothing -> cook_term ctxt (bound_var : bound_ctxt) inside

add_to_context :: [(String, Term)] -> String -> Raw_term -> With_error [(String, Term)]
add_to_context ctxt name raw_term =
  case (find (\entry -> ((fst entry) == name)) ctxt) of
    (Just entry) -> Error (name ++ " is already defined")
    Nothing -> do
      term <- cook_term ctxt [] raw_term
      return ((name, term) : ctxt)
      
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
        
parse_multiple_lines :: [(String, Raw_term)] -> [(String, Term)] -> (With_error [(String, Term)])
parse_multiple_lines [] starting_ctxt = return starting_ctxt
parse_multiple_lines (l : ls) starting_ctxt = do
  new_ctxt <- (add_to_context starting_ctxt (fst l) (snd l))
  (parse_multiple_lines ls new_ctxt)
  
exp_to_pair :: Exp -> (String, Raw_term)
exp_to_pair (Let name raw_term) = (name, raw_term)     
      
file_parser :: String -> (With_error [(String, Term)])
file_parser code = parse_multiple_lines (fmap (exp_to_pair . parse_lambda) (separate_by code ";" [])) []      
      
      
