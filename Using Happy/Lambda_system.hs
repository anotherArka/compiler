module Lambda_system where

import Junkyard
import MonadE

data Term =
    Free String |
    Bound Int   | -- we will use De-Bruijn indexing
    App Term Term |
    Lambda Term
  deriving Eq

  
instance Show Term where
  show x = show_helper 0 x

show_helper :: Int -> Term -> String
show_helper level (Free x) = x
show_helper level (Bound n) = "#x" ++ (show (level - n - 1))
show_helper level (Lambda inside) =
  "/" ++ "#x" ++ (show level) ++ "." ++ (show_helper (level + 1) inside)
show_helper level (App x y) =
  "(" ++ (show_helper level x) ++ " " ++ (show_helper level y) ++ ")"
  
-- substitution method for free variables
-- substitute_free "variable to be replaced" "term to be replaced with"
--                 "term to replace in"
substitute_free :: String -> Term -> Term -> Term
substitute_free v term (Free s) = if (v == s) then term else (Free s)
substitute_free v term (Bound n) = Bound n
substitute_free v term (Lambda inside) = Lambda (substitute_free v term inside)
substitute_free v term (App func args) =
  App (substitute_free v term func) (substitute_free v term args)
  
-- substitution method for bound variables
-- substitution depth "term to replace with" "term to replace in"
substitute_bound :: Int -> Term -> Term -> Term
substitute_bound depth term (Free v)  = Free v
substitute_bound depth term (Bound n) =
  if (n == depth) then term
  else if (n > depth) then (Bound (n -1))
  else (Bound n)
substitute_bound depth term (Lambda inside) =
  Lambda (substitute_bound (depth + 1) term inside)      
substitute_bound depth term (App x y) =
  App (substitute_bound depth term x) (substitute_bound depth term y)
    
evaluate :: Term -> Term
evaluate (Free u)        = (Free u)
evaluate (Bound n)       = Bound n
evaluate (Lambda inside) = Lambda (evaluate inside)
evaluate (App (Lambda inside) args) = (substitute_bound 0 args inside)
evaluate (App func args) = (App (evaluate func) (evaluate args))

evaluate_times :: Int -> Term -> (E Term)
evaluate_times n term =
  if (n < 0) then (Failed "Can not evaluate negative times")
  else if (n == 0) then (Ok term)
  else (evaluate_times (n - 1) (evaluate term))

