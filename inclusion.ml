


let t=(Tidel.ental: ('a Tidel.set) Martian_partial_ordering.t);;

(*we suppose that ll is pre-ordered with respect to inclusion 
in the two functions below *)
let select_minimal_elements ll=
  Ordered_bare_set.safe_set2
  (Martian_partial_ordering.select_minimal_elements t (Ordered_bare_set.forget_order ll));;
let select_maximal_elements ll=
  Ordered_bare_set.safe_set2
  (Martian_partial_ordering.select_maximal_elements t (Ordered_bare_set.forget_order ll));;


(*we do not suppose that ll is pre-ordered with respect to kenver 
in the two functions below *)

let select_minimal_elements_carefully ll=
  Martian_partial_ordering.select_minimal_elements_carefully t ll;;
let select_maximal_elements_carefully ll=
  Martian_partial_ordering.select_maximal_elements_carefully t ll;;


