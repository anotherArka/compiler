module Typed_lambda where

data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero == Zero = True
  Zero == _ = False
  _ == Zero = False
  (Succ m) == (Succ n) = (m == n)

data TType =
  Empty |
  Singleton |
  Sum TType TType |
  Product TType TType |
  Function TType TType |
  Inductive [(String, (TType, Nat))] 
  -- An inductive type is given by its list of constructors
  -- For eg, a constructor of X, cons : A -> B -> X -> X -> X can be written as
  -- ("cons", (Product A B), 2)) 
  -- For eg List Nat can be defined as
  -- Inductive [("nil", (Singleton, Zero)),("append", (Nat, (Succ Zero)))]

instance Eq TType where
  Empty == Empty = True
  Singleton == Singleton = True
  (Sum a b) == (Sum c d) = (a == c) && (b == d)
  (Product a b) == (Product c d) = (a == c) && (b == d)
  (Function a b) == (Function c d) = (a == c) && (b == d)
  (Inductive c) == (Inductive d) = (c == d)

instance Show TType where
  show Empty = "Empty"
  show Singleton = "Singleton"
  show (Sum a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
  show (Product a b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
  show (Function a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")" 

data Term = 
  Free String | -- free variables
  Bound Int | -- bound variable using De-Bruijn Indexing
              -- while parsing we have to make sure that 
              -- Int is greater than equal to 0
  Unit |
  Inr Term | 
  Inl Term | 
  Pair Term Term |
  Lambda Term | -- We will use De-Bruijn indexing 
  App Term Term |
  Void
  -- have to add definition of inductive function and application of a function
  -- Ind String Term [Term]

instance Eq Term where
  (Free this) == (Free that) = this == that
  (Bound this) == (Bound that) = this == that
  Unit == Unit = True
  (Inr this) == (Inr that) = (this == that)
  (Inl this) == (Inl that) = (this == that)
  (Pair a b) == (Pair c d) = (a == c) && (b == d)
  (Lambda this) == (Lambda that) = (this == that)
  (App f x) == (App g y) = (f == g) && (x == y)
  Void == Void = True
  -- (Ind c u s) == (Ind d v t) = (c == d) && (u == v) && (s == t)
  _ == _ = False

instance Show Term where
  show (Free x) = x
  show (Bound n) = "#x" ++ (show n) ++ "" -- only bound variables will be shown using '#'
  show Unit = "unit"
  show (Inr x) = "Inr(" ++ (show x) ++ ")"
  show (Inl x) = "Inl(" ++ (show x) ++ ")"
  show (Pair x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Lambda inside) = "/" ++ " .(" ++ (show inside) ++ ")"
  show (App x y) = (show x) ++ "(" ++ (show y) ++ ")"
  show (Void) = "void"

-- substitution method for free variables
-- substitute_free "variable to be replaces" "term to be replaced with"
--                 "term to replace in"
-- substitute_free :: String -> Term -> Term -> Term
-- substitute_free v term (Free s) = if (v == s) then term else (Free s)
-- substitute_free v term (Bound n) = Bound n
-- substitute_free v term Unit = Unit
-- substitute_free v term (Inr inside t) = Inr (substitute_free v term inside) t
-- substitute_free v term (Inl inside t) = Inl (substitute_free v term inside) t
-- substitute_free v term (Pair left right) =
--   Pair (substitute_free v term left) (substitute_free v term right)
-- substitute_free v term (Lambda t inside) = Lambda t (substitute_free v term inside)
-- substitute_free v term (App func args) =
--   App (substitute_free v term func) (substitute_free v term args)
-- substitute_free v term (Void t) = Void t  
-- -- substitute v term (Ind cons values nodes) =
-- --   Ind cons (substitute v term values) (fmap (substitute v term) nodes)

-- -- substitution method for bound variables
-- -- substitution depth "term to replace with" "term to replace in"
-- substitute_bound :: Int -> Term -> Term -> Term
-- substitute_bound depth term (Free v) = Free v
-- substitute_bound depth term (Bound n) =
--   if (n == depth) then term
--   else if (n > depth) then (Bound (n -1))
--   else (Bound n)
-- substitute_bound depth term Unit = Unit
-- substitute_bound depth term (Inr inside t) = Inr (substitute_bound depth term inside) t
-- substitute_bound depth term (Inl inside t) = Inl (substitute_bound depth term inside) t
-- substitute_bound depth term (Pair left right) = 
--   Pair (substitute_bound depth term left) (substitute_bound depth term right)
-- substitute_bound depth term (Lambda t inside) =
--   Lambda t (substitute_bound (depth + 1) term inside)      
-- substitute_bound depth term (App x y) =
--   App (substitute_bound depth term x) (substitute_bound depth term y)
-- substitute_bound v term (Void t) = Void t
     

-- We have to make sure that evaluation terminates  
-- evaluate :: Term -> Term
-- evaluate (Free u) = (Free u)
-- evaluate (Bound n) = Bound n
-- evaluate Unit = Unit
-- evaluate (Inr inside t) = Inr (evaluate inside) t
-- evaluate (Inl inside t) = Inl (evaluate inside) t
-- evaluate (Pair left right) = Pair (evaluate left) (evaluate right)
-- evaluate (Lambda t inside) = Lambda t (evaluate inside)
-- evaluate (App (Lambda t inside) args) = evaluate (substitute_bound 0 args inside)
-- evaluate (App func args) = let
--   func_1 = evaluate func
--   args_1 = evaluate args
--   in
--   if ((func == func_1) && (args == args_1)) then (App func args)
--     else (evaluate (App func_1 args_1))
-- evaluate (Void t) = Void t
 
find_type_of_var :: String -> [(String, TType)] -> (Either String TType)
find_type_of_var x [] = Left ("Not found variable" ++ x)
find_type_of_var x (v : vs) =
  if ((fst v) == x) then (Right (snd v)) else (find_type_of_var x vs)

-- -- type_check term "type of free variables" "type of bound variables" 
-- type_check :: Term -> [(String, TType)] -> [TType] -> (Either String TType)
-- type_check (Free x) fs bs = find_type_of_var x fs
-- type_check (Bound n) fs bs = 
--   if (n < 0) then Left("Index of bound variable can not be negative")
--   else if (n >= length(bs)) then Left("Index of bound variable more than number of lambda bindings")
--   else Right (bs !! n)
-- type_check Unit fs bs = Right Singleton
-- type_check (Inr x t) fs bs = (type_check x fs bs) >>= (\s -> Right (Sum t s))
-- type_check (Inl x t) fs bs = (type_check x fs bs) >>= (\s -> Right (Sum s t))
-- type_check (Pair x y) fs bs =
--   (type_check x fs bs) >>= 
--   (\t -> ((type_check y fs bs) >>= 
--   (\s -> Right (Product s t))))
-- type_check (Lambda t inside) fs bs = type_check inside fs (t : bs) 
-- type_check (App x y) fs bs =
--   (type_check x fs bs) >>=
--   (\t -> case t of
--     (Function u v) -> ((type_check y fs bs) >>=
--       (\s -> if (u == s) then Right(v) else
--         Left("The function " ++ (show x) ++ " : " ++ (show (Function u v)) 
--         ++ " has argument " ++ (show y) ++ " : " ++ (show v))))
--     _ -> Left("Not a function : " ++ (show x)))
-- type_check (Void tt) fs bs = Right (Function Empty tt)     