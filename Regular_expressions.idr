module Regular_expressions

%access public export

data Regex : Type -> Type where
  Eps  : (Regex alphabet)
  Phi  : (Regex alphabet)
  Atom : alphabet -> (Regex alphabet)
  Star : (Regex alphabet) -> (Regex alphabet)
  (+)  : (Regex alphabet) -> (Regex alphabet) -> (Regex alphabet)
  (.)  : (Regex alphabet) -> (Regex alphabet) -> (Regex alphabet)

(Eq alphabet) => Eq (Regex alphabet) where
  Eps == Eps                  = True
  Phi == Phi                  = True
  (Atom a) == (Atom b)        = (a == b)
  (Star left) == (Star right) = (left == right)
  (a + b) == (c + d)          = (a == c) && (b == d)
  (a . b) == (c . d)          = (a == c) && (b == d)
  _ == _ = False

total
simplify_Phi : (Regex alphabet) -> (Regex alphabet)
simplify_Phi Eps            = Eps
simplify_Phi Phi            = Phi
simplify_Phi (Atom a)       = Atom a
simplify_Phi (Star Phi)     = Eps
simplify_Phi (Star expr)    = Star (simplify_Phi expr)
simplify_Phi (Phi + expr)   = simplify_Phi expr
simplify_Phi (expr + Phi)   = simplify_Phi expr
simplify_Phi (left + right) = (simplify_Phi left) + (simplify_Phi right)
simplify_Phi (Phi . expr)   = Phi
simplify_Phi (expr . Phi)   = Phi
simplify_Phi (left . right) = (simplify_Phi left) . (simplify_Phi right)

total
simplify_Eps : (Regex alphabet) -> (Regex alphabet)
simplify_Eps Eps            = Eps
simplify_Eps Phi            = Phi
simplify_Eps (Atom a)       = Atom a
simplify_Eps (Star Eps)     = Eps
simplify_Eps (Star expr)    = Star (simplify_Eps expr)
simplify_Eps (left + right) = (simplify_Eps left) + (simplify_Eps right)
simplify_Eps (Eps . expr)   = (simplify_Eps expr)
simplify_Eps (expr . Eps)   = (simplify_Eps expr)
simplify_Eps (left . right) = (simplify_Eps left) . (simplify_Eps right)

total
simplify_Star : (Eq alphabet) => (Regex alphabet) -> (Regex alphabet)
simplify_Star Eps = Eps
simplify_Star Phi = Phi
simplify_Star (Atom a) = Atom a
simplify_Star (Star (left . right)) = if (left == right) then (Star left)
  else ((simplify_Star left) . (simplify_Star right))  
simplify_Star (Star expr) = Star (simplify_Star expr)
simplify_Star (left + right) = (simplify_Star left) + (simplify_Star right)
simplify_Star ((Star left) . (Star right)) = if (left == right) then (Star left)
  else ((Star (simplify_Star left)) . (Star (simplify_Star right)))
simplify_Star (left . right) = (simplify_Star left) . (simplify_Star right)  
  