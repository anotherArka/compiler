data Nat = Zero | Succ Nat

data Type =
  Empty |
  Singleton |
  Sum Type Type |
  Product Type Type |
  Function Type Type |
  Inductive [(String, (Type, Nat))] 
  -- An inductive type is given by its list of constructors
  -- For eg, a constructor of X, cons : A -> B -> X -> X -> X can be written as
  -- ("cons", (Product A B), 2)) 
  -- For eg List Nat can be defined as
  -- Inductive [("nil", (Singleton, Zero)),("append", (Nat, (Succ Zero)))]


data Term = 
  Var String |
  Unit |
  Inr Term |
  Inl Term |
  Pair Term Term |
  Fst Term |
  Snd Term |
  Lambda String Term |
  Ind String (Term, [Term])

substitution :: String -> Term -> Term -> Term
substitution v term (Var u) = term
substitution v term Unit = Unit
substitution v term (Inr inside) = Inr (substitution v term inside)
substitution v term (Inl inside) = Inl (substitution v term inside)
substitution v term (Pair left right) =
  Pair (substitution v term left) (substitution v term right)
substitution v term (Fst inside) = Fst (substitution v term inside)
substitution v term (Snd inside) = Snd (substitution v term inside)
substitution v term (Lambda u inside) =
  if (v == u) then (Lambda u inside) else (Lambda u (substitution v term inside))

