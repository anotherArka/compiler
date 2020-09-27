module DFA

%access public export

||| technically it's an NFA with no epsilon transition
data DFA : (states : Type) -> (alphabet : Type) -> Type where
  Trans : (f : states -> states -> (List alphabet)) -> (DFA states alphabet)