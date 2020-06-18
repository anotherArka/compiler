module Untyped where

data Nat = Zero | Succ Nat

data Term = 
      Variable String
    | Lambda String Term
    | App Term Term
    
if_else :: Bool -> typ -> typ -> typ
if_else True x y = x
if_else False x y = y
    
substitute :: String -> Term -> Term -> Term
substitute v term (Variable u) = if_else (v == u) term (Variable u)
substitute v term (Lambda u inside) = if_else (v == u) (Lambda u inside) (Lambda u (substitute v term inside))
substitute v term (App func args) = App (substitute v term func) (substitute v term args)

term_to_string :: Term -> String
term_to_string (Variable v) = v
term_to_string (Lambda v inside) = "/" ++ v ++ "." ++ (term_to_string inside)
term_to_string (App func args) = "(" ++ (term_to_string func) ++ ")(" ++ (term_to_string args) ++ ")"

instance Show Term where
    show term = term_to_string term

evaluate :: Nat -> Term -> Term
evaluate Zero term = term
evaluate (Succ n) (Variable u) = (Variable u)
evaluate (Succ n) (Lambda v term) = (Lambda v (evaluate n term))
evaluate (Succ n) (App (Variable u) term) = App (Variable u) (evaluate n term)
evaluate (Succ n) (App (Lambda v inside) term) = evaluate n (substitute v term (evaluate n inside))
evaluate (Succ n) (App (App func left) right) = evaluate n (App (App (evaluate n func) (evaluate n left)) (evaluate n right))

expr1 = Lambda "x" (App (Variable "x") (Variable "x"))

one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five
seven = Succ six
eight = Succ seven
nine = Succ eight
ten = Succ nine
