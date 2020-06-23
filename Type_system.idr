module Type_system

-- mutual 
--   data Univ : (n : Nat) -> Type where
--     Function : (n : Nat) -> (Function_type n) -> (Univ n)
--     Family : (n : Nat) -> (Family_type n) -> (Univ (S n))
--     Inductive : (n : Nat) -> (Inductive_Type n) -> (Univ n)
--     Univ_itself : (n : Nat) -> (Univ (S n))

--   data Function_type : (n : Nat) -> Type where
--     From_to : (Univ n) -> (Univ n) -> (Function_type n)

--   data Family_type : (n : Nat) -> Type where
--     Over : (Univ n) -> (Family_type n)

--   data Inductive_Type : (n : Nat) -> Type where
--     Index : (Family_type n) -> (Inductive_Type n)
  
mutual
  data TTerm : Type where
    Unit : TTerm -- element of the singleton type
    Myth : TType -> TTerm -- function from the empty type
    Bound : Nat -> TTerm -- bound variable
    Free : String -> TTerm
    TPair : TTerm -> TTerm -> TTerm
    App : TTerm -> TTerm -> TTerm

  data TType : Type where
    Singleton : TType
    Empty : TType
    Function : TType -> TType -> TType
    Product : TType -> TType -> TType

  data TTRel : TTerm -> TType -> Type where
    Unit_rule : TTRel Unit Singleton
    Void_rule : (ty : TType) -> (TTRel (Myth ty) (Function Empty ty))
    Product_rule : (x : TTerm) -> (tx : TType) -> (TTRel x tx) ->
                   (y : TTerm) -> (ty : TType) -> (TTRel y ty) ->
                   (TTRel (TPair x y) (Product tx ty))
    Function_rule : (dom : TType) -> (cod : TType) ->
                    (f : TTerm) -> (x : TTerm) ->
                    (TTRel f (Function dom cod)) -> (TTRel x dom) ->
                    (TTRel (App f x) cod)