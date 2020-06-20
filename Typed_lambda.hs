module Typed_lambda where

data Nat = Zero | Succ Nat

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

instance Show TType where
  show Empty = "Empty"
  show Singleton = "Unit"
  show (Sum a b) = (show a) ++ " + " ++ (show b)
  show (Product a b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")"
  show (Function a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")" 

data Term = 
  Var String |
  Unit |
  Inr Term TType | -- If x : T then (Inr x S) : Sum S T
  Inl Term TType | -- If x : T then (Inl x S) : Sum T S
  Pair Term Term |
  Lambda String TType Term | 
  App Term Term |
  Void TType 
  -- have to add definition of inductive function and application of a function
  -- Ind String Term [Term]

instance Eq Term where
  (Var this) == (Var that) = this == that
  Unit == Unit = True
  (Inr this s) == (Inr that t) = (this == that) && (s == t)
  (Inl this s) == (Inl that t) = (this == that) && (s == t)
  (Pair a b) == (Pair c d) = (a == c) && (b == d)
  (Lambda a u this) == (Lambda b v that) = (u == v) && (a == b) && (this == that)
  (App f x) == (App g y) = (f == g) && (x == y)
  (Void s) == (Void t) = (s == t)
  -- (Ind c u s) == (Ind d v t) = (c == d) && (u == v) && (s == t)
  _ == _ = False

instance Show Term where
  show (Var x) = x
  show Unit = "unit"
  show (Inr x t) = "Inr(" ++ (show x) ++ ")"
  show (Inl x t) = "Inl(" ++ (show x) ++ ")"
  show (Pair x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Lambda x t inside) = "/" ++ x ++ " : " ++ (show t) ++ " . " ++ (show inside)
  show (App x y) = "(" ++ (show x) ++ (show y) ++ ")"
  show (Void t) = "void"

substitute :: String -> Term -> Term -> Term
substitute v term (Var u) = term
substitute v term Unit = Unit
substitute v term (Inr inside t) = Inr (substitute v term inside) t
substitute v term (Inl inside t) = Inl (substitute v term inside) t
substitute v term (Pair left right) =
  Pair (substitute v term left) (substitute v term right)
substitute v term (Lambda u t inside) =
  if (v == u) then (Lambda u t inside) else (Lambda u t (substitute v term inside))
substitute v term (App func args) = App (substitute v term func) (substitute v term args)
substitute v term (Void t) = Void t  
-- substitute v term (Ind cons values nodes) =
--   Ind cons (substitute v term values) (fmap (substitute v term) nodes)

-- We have to make sure that evaluation terminates  
evaluate :: Term -> Term
evaluate (Var u) = (Var u)
evaluate Unit = Unit
evaluate (Inr inside t) = Inr (evaluate inside) t
evaluate (Inl inside t) = Inl (evaluate inside) t
evaluate (Pair left right) = Pair (evaluate left) (evaluate right)
evaluate (Lambda a t inside) = Lambda a t (evaluate inside)
evaluate (App (Lambda a t inside) args) = evaluate (substitute a args inside)
evaluate (App func args) = let
  func_1 = evaluate func_1
  args_1 = evaluate args
  in
  if ((func == func_1) && (args == args_1)) then (App func args)
    else (evaluate (App func_1 args_1))
evaluate (Void t) = Void t
 
find_type_of_var :: String -> [(String, TType)] -> (Either String TType)
find_type_of_var x [] = Left ("Not found variable" ++ x)
find_type_of_var x (v : vs) =
  if ((fst v) == x) then (Right (snd v)) else (find_type_of_var x vs)

type_check :: Term -> [(String, TType)] -> (Either String TType)
type_check (Var x) vs = find_type_of_var x vs
type_check Unit vs = Right Singleton
type_check (Inr x t) vs = (type_check x vs) >>= (\s -> Right (Sum t s))
type_check (Inl x t) vs = (type_check x vs) >>= (\s -> Right (Sum s t))
type_check (Pair x y) vs =
  (type_check x vs) >>= 
  (\t -> ((type_check y vs) >>= 
  (\s -> Right (Product s t))))
type_check (Lambda x t inside) vs = type_check inside ((x, t) : vs)  
type_check (App x y) vs =
  (type_check x vs) >>=
  (\t -> case t of
    (Function u v) -> ((type_check y vs) >>=
      (\s -> if (u == s) then Right(v) else
        Left("The function " ++ (show x) ++ " : " ++ (show (Function u v)) 
        ++ " has argument " ++ (show y) ++ " : " ++ (show v))))
    _ -> Left("Not a function : " ++ (show x)))
type_check (Void tt) vs = Right (Function Empty tt)     