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
    Myth : TTerm -> TTerm -- function from the empty type
    Bound : Nat -> TTerm -- bound variable
    Free : String -> TTerm
    TPair : TTerm -> TTerm -> TTerm -- is this with the rule in TTRel enough for a dependent pair?
    App : TTerm -> TTerm -> TTerm
    Lambda : TTerm -> TTerm -- by lambda we just bind the variable "Bound 0"

  --data TType : Type where
    Universe : Nat -> TTerm
    Singleton : TTerm
    Empty : TTerm
    Function : TTerm -> TTerm -> TTerm
    Product : TTerm -> TTerm -> TTerm
    Inductive : TTerm -> Nat -> TTerm -> TTerm
    Cons : TTerm -> TTerm  
      
  -- For example in the type Fin : Nat -> Type, Nat is a parameter
  -- In the declaration data Tree : (ty : Type) -> Type where
  --                                 leaf : ty -> Type  
  --                                 node : (Tree ty) -> (Tree ty) -> (Tree ty)
  -- ty is an argument. Because we are using De-Bruijn index it is enough to
  -- specify the number of arguments.
  
  -- specify constructors as functions into the inductive type. Take care of
  -- the evaluation as declaring them as normal forms
  
  -- add extension and contraction rules for contexts
  data TTRel : (List TTerm) -> TTerm -> TTerm -> Type where
    Unit_rule : (ctx : List TTerm) -> TTRel ctx Unit Singleton  
    Void_rule : (ctx : List TTerm) -> (ty : TTerm) -> (TTRel ctx (Myth ty) (Function Empty ty))
    Universe_hierarchy_rule : (ctx : List TTerm) -> (n : Nat) -> (TTRel ctx (Universe n) (Universe (S n)))
    Universe_inclusion_rule : (ctx : List TTerm) -> (n : Nat) -> (t : TTerm) ->
                              (TTRel ctx t (Universe n)) ->  (TTRel ctx t (Universe (S n)))
    Function_type_rule : (ctx : List TTerm) -> (n : Nat) -> (dom : TTerm) -> (cod : TTerm) ->
                         (TTRel ctx dom (Universe n)) -> (TTRel (dom :: ctx) cod (Universe n)) ->
                         (TTRel ctx (Function dom cod) (Universe n))                            
    {-
    Product_rule : (x : TTerm) -> (tx : TTerm) -> (ctx : List TTerm) -> (TTRel ctx x tx) ->
                   (y : TTerm) -> (ty : TTerm) -> (ctx : List TTerm) -> (TTRel y ty) ->
                   (TTRel (TPair x y) (Product tx ty))
    Function_rule : (dom : TTerm) -> (cod : TTerm) ->
                    (f : TTerm) -> (x : TTerm) ->
                    (TTRel f (Function dom cod)) -> (TTRel x dom) ->
                    (TTRel (App f x) cod)
    Lambda_rule : (dom : TTerm) -> (cod : TTerm) ->
                  (inside : TTerm) -> (TTRel (Bound Z) dom) -> (TTRel inside cod) ->
                  (TTRel (Lambda inside) (Function dom cod))
    --------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------- IMPORTANT NOTE ---------------------------------------
    ------------------------------------------------------- IMPORTANT NOTE ---------------------------------------
    --------------------------------------------------------------------------------------------------------------              
    -- Possibly we need more rules to check validity of this inductive type
    -- I don't think that it reflects dependency on the arguments
    -- maybe we need to insert context explicitly for this purpose
    -----------------------------------------------------------------------------------------------------------------------------
    -----------------------------------------------------------------------------------------------------------------------------
    Inductive_rule : (params : TTerm) -> (args : Nat) -> (cons_dom : TTerm) -> -- cons_dom is domain of the constructor
                     (inside : TTerm) -> (TTRel inside cons_dom) ->
                     (TTRel (Cons inside) (Inductive params args cons_dom))  
    -}                         
