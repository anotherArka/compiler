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
term_to_string (Lambda v inside) = "\\" ++ v ++ ".(" ++ (term_to_string inside) ++ ")"
term_to_string (App func args) = "(" ++ (term_to_string func) ++ (term_to_string args) ++ ")"

