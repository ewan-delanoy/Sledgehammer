
type t=Hidden_vector.relation;;
 
let unveil=(Hidden_vector.unveil_relation:>t-> (Rational.t * Variable.t) list);;
let of_vector=(Hidden_vector.to_relation:>Vector.t->t);;
let isolate=(Hidden_vector.isolate:>t->Variable.t->Vector.t);;


let print=(Hidden_vector.print_relation:t->string);;
let print_out=(Hidden_vector.print_out_relation:Format.formatter->t->unit);;

