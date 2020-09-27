import Regular_expressions
import DFA

map_list : (a -> b) -> (List a) -> (List b)
map_list f [] = []
map_list f (x :: xs) = (f x) :: (map_list f xs)

list_to_regex : (List alphabet) -> Regex alphabet
list_to_regex [] = Phi
list_to_regex (x :: xs) = ((Atom x) + (list_to_regex xs))

dfa_to_regex : (DFA states alphabet) -> 
  (start : states) -> (final : states) -> (inter : List states) -> 
  (Regex alphabet)
dfa_to_regex (Trans d) s f [] = list_to_regex (d s f)
dfa_to_regex (Trans d) s f (q :: qs) = (dfa_to_regex (Trans d) s f qs) +
       ((dfa_to_regex (Trans d) s q qs) . 
  (Star (dfa_to_regex (Trans d) q q qs)) . 
        (dfa_to_regex (Trans d) q f qs)) 