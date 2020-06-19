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
  Lambda String Term | -- have to add definition of inductive function
  Ind String Term [Term]

substitute :: String -> Term -> Term -> Term
substitute v term (Var u) = term
substitute v term Unit = Unit
substitute v term (Inr inside) = Inr (substitute v term inside)
substitute v term (Inl inside) = Inl (substitute v term inside)
substitute v term (Pair left right) =
  Pair (substitute v term left) (substitute v term right)
substitute v term (Fst inside) = Fst (substitute v term inside)
substitute v term (Snd inside) = Snd (substitute v term inside)
substitute v term (Lambda u inside) =
  if (v == u) then (Lambda u inside) else (Lambda u (substitute v term inside))
substitute v term (Ind cons values nodes) =
  Ind cons (substitute v term values) (fmap (substitute v term) nodes)

instance Eq Term where
  (Var this) == (Var that) = this == that
  Unit == Unit = True
  (Inr this) == (Inr that) = this == that
  (Pair a b) == (Pair c d) = (a == c) && (b == d)
  (Fst (Pair a b)) == (Fst (Pair c d)) = a == c
  (Fst this) == (Fst that) = (this == that)
  (Snd (Pair a b)) == (Snd (Pair c d)) = b == d
  (Snd this) == (Snd that) = (this == that)
  (Lambda a this) == (Lambda b that) = (a == b) && (this == that)
  _ == _ = False
  
  

evaluate :: Term -> Term
evaluate (Var u) = (Var u)
evaluate Unit = Unit
evaluate (Inr inside) = Inr (evaluate inside)
evaluate (Inl inside) = Inl (evaluate inside)
evaluate (Pair left right) = Pair (evaluate left) (evaluate right)  
evaluate (Fst (Pair left right)) = left
evaluate (Snd (Pair left right)) = right
