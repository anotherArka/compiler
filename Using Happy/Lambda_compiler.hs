module Lambda_compiler where

import Data.List
import Data.Char
import Junkyard
import Lambda_happy
import Lambda_system
import MonadE 

-- Tries to compute the term from the raw term computed by the parser,
-- given the context and the list of bound variables

cook_term :: [(String, Term)] -> [String] -> Raw_term -> (E Term)
cook_term ctxt bound_ctxt (Var var) =
  case (find (\entry -> ((fst entry) == var)) ctxt) of
    (Just found_def) -> case (elemIndex var bound_ctxt) of
      (Just bound_index) -> Failed ("repeated use of name " ++ var)
      Nothing -> Ok (snd found_def)
    Nothing -> case (elemIndex var bound_ctxt) of
      (Just bound_index) -> Ok (Bound bound_index)
      Nothing -> Failed (var ++ " is undefined")            
cook_term ctxt bound_ctxt (Raw_app func args) = do
  f <- cook_term ctxt bound_ctxt func
  x <- cook_term ctxt bound_ctxt args
  return (App f x)
cook_term ctxt bound_ctxt (Raw_lambda bound_var inside) =
  if (elem bound_var bound_ctxt) then (Failed (bound_var ++ " is already bound"))
  else case (find (\entry -> ((fst entry) == bound_var)) ctxt ) of
    Just entry -> Failed (bound_var ++
      " is already defined and can not be used as a bound variable")
    Nothing -> do
      val <- cook_term ctxt (bound_var : bound_ctxt) inside
      return (Lambda val)

add_to_context :: [(String, Term)] -> String -> Raw_term -> E [(String, Term)]
add_to_context ctxt name raw_term =
  case (find (\entry -> ((fst entry) == name)) ctxt) of
    (Just entry) -> Failed (name ++ " is already defined")
    Nothing -> do
      term <- cook_term ctxt [] raw_term
      return ((name, term) : ctxt)

eval_def :: [(String, Term)] -> Int -> String -> E ([(String, Term)], Term)
eval_def [] n name       = Failed (name ++ " is undefined")
eval_def (c : cs) n name = if ((fst c) == name)
  then do
    val <- (evaluate_times n (snd c))
    return (((name, val) : cs), val) 
  else do
    ds <- eval_def cs n name
    return ((c : (fst ds)), (snd ds))
        
parse_multiple_lines :: [(String, Raw_term)] -> [(String, Term)] -> (E [(String, Term)])
parse_multiple_lines [] starting_ctxt = return starting_ctxt
parse_multiple_lines (l : ls) starting_ctxt = do
  new_ctxt <- (add_to_context starting_ctxt (fst l) (snd l))
  (parse_multiple_lines ls new_ctxt)

{-  
exp_to_pair :: Exp -> (String, Raw_term)
exp_to_pair (Let name raw_term) = (name, raw_term)     
      
file_parser :: String -> (With_error [(String, Term)])
file_parser code = parse_multiple_lines
  (fmap (exp_to_pair . parse_lambda) (filter (\l -> ((length l) > 0))
  (separate_by code ";" []))) []      
-}
execute_command :: (E Exp) -> [(String, Term)] -> (String, [(String, Term)])
execute_command (Failed msg)             ctxt = (msg, ctxt)
execute_command (Ok (Let name raw_term)) ctxt = case (add_to_context ctxt name raw_term) of
  (Ok new_ctxt) -> (name ++ " = " ++ (show (snd (head new_ctxt))), new_ctxt)
  (Failed  msg) -> (msg                                         , ctxt)
execute_command (Ok (Print raw_term)) ctxt    = case (cook_term ctxt [] raw_term) of
  (Ok term)    -> ((show term), ctxt)
  (Failed msg) -> (msg        , ctxt)
execute_command (Ok (Eval_def n var)) ctxt    = case (eval_def ctxt n var) of
  (Ok result)  -> ((show (snd result)), (fst result))
  (Failed msg) -> (msg                , ctxt)
execute_command (Ok (Eval n raw_term)) ctxt   = case (cook_term ctxt [] raw_term) of
  (Ok term)    -> (show (evaluate term), ctxt)
  (Failed msg) -> (msg                 , ctxt)       
    
delete_preceding_spaces :: String -> String
delete_preceding_spaces [] = []
delete_preceding_spaces (c : cs) =
  if (isSpace c) then (delete_preceding_spaces cs)
  else (c : cs)    
        
repl :: [(String, Term)] -> IO()          
repl ctxt = do
    putStrLn "---------------------"
    input <- getLine
    if ((delete_preceding_spaces input) == ":q") then return()
    else let
           exp = parse_lambda input
           new = execute_command exp ctxt
         in  
         (putStrLn (fst new)) >> 
         repl (snd new)
