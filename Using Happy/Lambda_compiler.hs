module Lambda_compiler where

import Data.List
import Junkyard
import Lambda_happy
import Lambda_system  

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
    Nothing -> do
      val <- cook_term ctxt (bound_var : bound_ctxt) inside
      return (Lambda val)

add_to_context :: [(String, Term)] -> String -> Raw_term -> With_error [(String, Term)]
add_to_context ctxt name raw_term =
  case (find (\entry -> ((fst entry) == name)) ctxt) of
    (Just entry) -> Error (name ++ " is already defined")
    Nothing -> do
      term <- cook_term ctxt [] raw_term
      return ((name, term) : ctxt)

eval_def :: [(String, Term)] -> Int -> String -> With_error [(String, Term)]
eval_def [] n name       = Error (name ++ " is undefined")
eval_def (c : cs) n name = if ((fst c) == name)
  then do
    val <- (evaluate_times n (snd c))
    return ((name, val) : cs) 
  else do
    ds <- eval_def cs n name
    return (c : ds)
        
parse_multiple_lines :: [(String, Raw_term)] -> [(String, Term)] -> (With_error [(String, Term)])
parse_multiple_lines [] starting_ctxt = return starting_ctxt
parse_multiple_lines (l : ls) starting_ctxt = do
  new_ctxt <- (add_to_context starting_ctxt (fst l) (snd l))
  (parse_multiple_lines ls new_ctxt)
  
exp_to_pair :: Exp -> (String, Raw_term)
exp_to_pair (Let name raw_term) = (name, raw_term)     
      
file_parser :: String -> (With_error [(String, Term)])
file_parser code = parse_multiple_lines
  (fmap (exp_to_pair . parse_lambda) (filter (\l -> ((length l) > 0))
  (separate_by code ";" []))) []      

execute_command :: Exp -> [(String, Term)] -> (String, [(String, Term)])
execute_command (Let name raw_term) ctxt = case (add_to_context ctxt name raw_term) of
  (Correct new_ctxt) -> ("" , new_ctxt)
  (Error   msg     ) -> (msg, ctxt)
execute_command (Print raw_term) ctxt    = case (cook_term ctxt [] raw_term) of
  (Correct term) -> ((show term), ctxt)
  (Error   msg ) -> (msg        , ctxt)
execute_command (Eval_def n var) ctxt    = ("yet to define", ctxt)
execute_command (Eval n raw_term) ctxt   = case (cook_term ctxt [] raw_term) of
  (Correct term) -> (show (evaluate term), ctxt)
  Error msg      -> (msg                 , ctxt)       
        
repl :: [(String, Term)] -> IO()          
repl ctxt = do
    foo <- putStrLn "little lamb>"
    input <- getLine  
    if (input == ":q") then return()
    else let
           exp = parse_lambda input
           new = execute_command exp ctxt
         in
         (putStrLn (fst new)) >> 
         repl (snd new)
