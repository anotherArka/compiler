module Lambda_compiler where

import Data.List
import Data.Char
import Control.Monad.Catch
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
{-      
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
  (Ok term)    -> (show (evaluate_times n term), ctxt)
  (Failed msg) -> (msg                 , ctxt)
-- execute_command (Ok (Load_file file_name)) ctxt = do
  --contents <- readFile              
        
repl :: (String, [(String, Term)]) -> IO ()          
repl (msg, ctxt) = do
    putStrLn (">> " ++ msg)
    putStrLn "---------------------"
    input <- getLine
    if ((delete_preceding_spaces input) == ":q") then (return ())
    else if ((delete_preceding_spaces input) == ":l") then catch 
      (  do
         putStrLn ">> File to load?"
         file_name <- getLine
         file_contents <- readFile file_name
         repl (parse_multiple_lines ctxt file_name 1 
           ("Loading file " ++ file_name) 
           (fmap parse_lambda           
           (filter (\el -> (foldl (\acc x -> (acc || (not (isSpace x)))) False el))
           -- takes the lines where there is at least one non-space char
           (separate_by file_contents ";" []))) ctxt))
     (file_error_handler ctxt)         
    else let
           exp = parse_lambda input
           new = execute_command exp ctxt
         in   
         repl new

file_error_handler :: [(String, Term)] -> SomeException -> IO ()
file_error_handler ctxt msg = repl ((show msg), ctxt)

error_msg :: String -> Int -> String -> String
error_msg file_name line_number msg =
  "Error at line " ++ (show line_number) ++ " of " ++ file_name ++ " : " ++ msg  
  
-- parse_multiple_lines original-context file-name line-number print_msg lines context          
parse_multiple_lines :: [(String, Term)] -> String -> Int -> String -> [(E Exp)] -> [(String, Term)] -> (String, [(String, Term)])
parse_multiple_lines original_ctxt file_name line_number print_msg [] ctxt = (print_msg, ctxt)
parse_multiple_lines original_ctxt file_name line_number print_msg ((Failed msg) : ls) ctxt =
  (error_msg file_name line_number msg, original_ctxt)
parse_multiple_lines original_ctxt file_name line_number print_msg ((Ok (Let name raw_term)) : ls) ctxt =
  case (add_to_context ctxt name raw_term) of
    (Ok new_ctxt) -> (parse_multiple_lines original_ctxt file_name (line_number + 1) print_msg ls new_ctxt)
    (Failed  msg) -> (error_msg file_name line_number msg, ctxt)
parse_multiple_lines original_ctxt file_name line_number print_msg ((Ok (Print raw_term)) : ls) ctxt =
  case (cook_term ctxt [] raw_term) of
    (Ok term)    ->
      (parse_multiple_lines original_ctxt file_name (line_number + 1) (print_msg ++ "\n" ++ (show line_number) ++ " :>" ++ (show term)) ls ctxt)
    (Failed msg) -> (error_msg file_name line_number msg, original_ctxt)
parse_multiple_lines original_ctxt file_name line_number print_msg ((Ok (Eval_def n var)) : ls) ctxt = case (eval_def ctxt n var) of
  (Ok result)  ->
    (parse_multiple_lines original_ctxt file_name (line_number + 1) (print_msg ++ "\n" ++ (show line_number) ++ " :>" ++ (show result)) ls ctxt)
  (Failed msg) -> (error_msg file_name line_number msg, original_ctxt)
parse_multiple_lines original_ctxt file_name line_number print_msg ((Ok (Eval n raw_term)) : ls) ctxt = case (cook_term ctxt [] raw_term) of
  (Ok term)    ->
    (parse_multiple_lines original_ctxt file_name (line_number + 1) (print_msg ++ "\n" ++ (show line_number) ++ " :>" ++ (show term)) ls ctxt)
  (Failed msg) -> (error_msg file_name line_number msg, original_ctxt)
