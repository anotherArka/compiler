module Regular_expressions

%access public export

data Regex : Type -> Type where
  Eps  : (Regex alphabet)
  Phi  : (Regex alphabet)
  Atom : alphabet -> (Regex alphabet)
  Star : (Regex alphabet) -> (Regex alphabet)
  (+)  : (Regex alphabet) -> (Regex alphabet) -> (Regex alphabet)
  (.)  : (Regex alphabet) -> (Regex alphabet) -> (Regex alphabet)
