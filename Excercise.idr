module Excercise

import DFA
import Regular_expressions
import DFA_to_Regex

data Alphabet = A | B 

Eq Alphabet where
  A == A = True
  B == B = True
  _ == _ = False

States : Type
States = (Bool, Bool) 

total
d : States -> States -> (List Alphabet)
d u v = if (((fst u) == (fst v)) && ((snd u) /= (snd v))) then [B]
   else if (((fst u) /= (fst v)) && ((snd u) == (snd v))) then [A]
   else []
   
list_of_states : (List States)
list_of_states = [(True, True), (True, False), (False, True), (False, False)]

answer : Regex Alphabet
answer = dfa_to_regex (Trans d) (True, True) (True, False) list_of_states

multiple_apply : (ty -> ty) -> Nat -> ty -> ty
multiple_apply f Z a = a
multiple_apply f (S n) a = f (multiple_apply f n a)

a1 : Regex Alphabet
a1 = multiple_apply simplify_Phi 16 answer

a2 : Regex Alphabet
a2 = multiple_apply simplify_Eps 16 a1

a3 : Regex Alphabet
a3 = multiple_apply simplify_Star 16 a2

