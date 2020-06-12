module Type_system

mutual 
  data Univ : (n : Nat) -> Type where
    Function : (n : Nat) -> (Function_type n) -> (Univ n)
    Family : (n : Nat) -> (Family_type n) -> (Univ (S n))
    Inductive : (n : Nat) -> (Inductive_Type n) -> (Univ n)
    Univ_itself : (n : Nat) -> (Univ (S n))

  data Function_type : (n : Nat) -> Type where
    From_to : (Univ n) -> (Univ n) -> (Function_type n)

  data Family_type : (n : Nat) -> Type where
    Over : (Univ n) -> (Family_type n)

  data Inductive_Type : (n : Nat) -> Type where
    Index : (Family_type n) -> (Inductive_Type n)
  
