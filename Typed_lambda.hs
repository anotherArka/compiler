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
  Lambda String Term | 
  App Term Term  
  -- have to add definition of inductive function and application of a function
  -- Ind String Term [Term]

substitute :: String -> Term -> Term -> Term
substitute v term (Var u) = term
substitute v term Unit = Unit
substitute v term (Inr inside) = Inr (substitute v term inside)
substitute v term (Inl inside) = Inl (substitute v term inside)
substitute v term (Pair left right) =
  Pair (substitute v term left) (substitute v term right)
substitute v term (Lambda u inside) =
  if (v == u) then (Lambda u inside) else (Lambda u (substitute v term inside))
substitute v term (App func args) = App (substitute v term func) (substitute v term args)  
-- substitute v term (Ind cons values nodes) =
--   Ind cons (substitute v term values) (fmap (substitute v term) nodes)

instance Eq Term where
  (Var this) == (Var that) = this == that
  Unit == Unit = True
  (Inr this) == (Inr that) = this == that
  (Pair a b) == (Pair c d) = (a == c) && (b == d)
  (Lambda a this) == (Lambda b that) = (a == b) && (this == that)
  (App f x) == (App g y) = (f == g) && (x == y)
  -- (Ind c u s) == (Ind d v t) = (c == d) && (u == v) && (s == t)
  _ == _ = False

-- We have to make sure that evaluation terminates  
evaluate :: Term -> Term
evaluate (Var u) = (Var u)
evaluate Unit = Unit
evaluate (Inr inside) = Inr (evaluate inside)
evaluate (Inl inside) = Inl (evaluate inside)
evaluate (Pair left right) = Pair (evaluate left) (evaluate right)
evaluate (Lambda a inside) = Lambda a (evaluate inside)
evaluate (App (Lambda a inside) args) = evaluate (substitute a args inside)
evaluate (App func args) = let
  func_1 = evaluate func_1
  args_1 = evaluate args
  in
  if ((func == func_1) && (args == args_1)) then (App func args)
    else (evaluate (App func_1 args_1))

--type_check :: Term -> [(String, Type)] -> Type
