
type t=Hidden_vector.descriptive_system;; 

let unveil=(Hidden_vector.unveil_descriptive_system:>t-> (Variable.t * Vector.t) list);;
let make=(Hidden_vector.make_descriptive:Relation.t list->t);;                             


let eval=(Hidden_vector.dissect_descriptive_system:>t->Variable.t list->Rational.t list);;
let transform_vector=(Hidden_vector.big_subs:>Vector.t->t->Vector.t);;

let test_for_injectivity=(Hidden_vector.test_for_injectivity:>t->bool);;
let insert=(Hidden_vector.insert_in_descriptive_system:>Relation.t->t->t);;
let insert_many=(Hidden_vector.insert_many_in_descriptive_system:>Relation.t list->t->t);;
let insert_simple_equality=(Hidden_vector.insert_simple_equality_in_descriptive_system:>
                             Variable.t->Rational.t->t->t);;
let insert_several_simple_equalities=(Hidden_vector.insert_several_simple_equalities_in_descriptive_system:>
                             (Variable.t*Rational.t) list->t->t);;       
let the_solution_is_a_line=(Hidden_vector.the_solution_is_a_line:>t->Variable.t list -> bool);;   


let print=(Hidden_vector.print_descriptive_system:t->string);;
let print_out=(Hidden_vector.print_out_descriptive_system:Format.formatter->t->unit);;


