
type t=Hidden_vector.t;;

let unveil=(Hidden_vector.unveil:>t->(Rational.t * Variable.t) list);;
let unsafe_vector=(Hidden_vector.unsafe_vector:>(Rational.t * Variable.t) list->t);;
let safe_vector=(Hidden_vector.safe_vector:>(Rational.t * Variable.t) list->t);;
let sum_of_variables=(Hidden_vector.of_list:> Variable.t list->t);;
let of_dlist=(Hidden_vector.of_dlist:>Variable.t list -> Variable.t list -> t);;
let lonely lambda var=
	if Rational.eq lambda Rational.zero 
	then unsafe_vector[] 
	else unsafe_vector[lambda,var];;
let of_rational lambda=lonely lambda Variable.dummy;; 
let of_variable var=lonely Rational.one var;;
let of_string s=of_variable(Variable.of_string s);;
let zero=unsafe_vector [];;
let one=unsafe_vector [Rational.one,Variable.dummy];;

let gauss_multiplier=(Hidden_vector.gauss_multiplier:>t->Rational.t);;
let gauss_operation=(Hidden_vector.gauss_operation:>t->t);;
let sign_preserving_gauss_operation=(Hidden_vector.sign_preserving_gauss_operation:>t->t);; 
let dot=(Hidden_vector.dot:>Rational.t -> t -> t);;
let scalar_product=(Hidden_vector.scalar_product:>t->t->Rational.t);;
let add=(Hidden_vector.add:>t->t->t);;
let big_sum=(Hidden_vector.big_sum:t list->t);;
let sub=(Hidden_vector.sub:>t->t->t);;
let subs=(Hidden_vector.subs:>t -> Variable.t -> t -> t);;
let fold_subs=(Hidden_vector.fold_subs:>(Variable.t * t) list -> t -> t);;

let linear_combination=(Hidden_vector.linear_combination:(Rational.t*t) list->t);;
let write_as_linear_combination=(Hidden_vector.write_as_linear_combination:>
                                   t->t list->Rational.t list option);;
let write_as_a_linear_combination_of_l_variables=
                  (Hidden_vector.write_as_a_linear_combination_of_l_variables:>
                                   t->t list->t option);;                                   
                                   
let extract_basis=(Hidden_vector.extract_basis:>t list->t list);; 
let extract_basis_carefully=(Hidden_vector.extract_basis_carefully:>t list->t list*((t * Rational.t list) list));; 
let extract_named_basis=(Hidden_vector.extract_named_basis:>
    ((Variable.t * t) list ->
    (Variable.t * t) list *
    (Variable.t * t) list)
    );;
  
let test_for_independence=(Hidden_vector.test_for_independence:>t list->bool);;
let defining_equations_for_subspace=(Hidden_vector.defining_equations_for_subspace:>t list->t list);;
let inverse_matrix=(Hidden_vector.inverse_matrix:>t list->(Variable.t * Rational.t list) list);;

let number_of_positive_coefficients=(Hidden_vector.number_of_positive_coefficients:>t->int);;
let coeff=(Hidden_vector.coeff:>t->Variable.t->Rational.t);;
let support=(Hidden_vector.support:>t->Ordered_variable.set);;

let suppress_one_component=(Hidden_vector.suppress_one_component:> t->Variable.t->t);;
let suppress_unity_and_normalize=(Hidden_vector.suppress_unity_and_normalize:> t->Rational.t*t);;

let ocaml_name=(Hidden_vector.ocaml_name:>t->string);;
let print=(Hidden_vector.print:>t->string);;
let print_out=(Hidden_vector.print_out:>Format.formatter->t->unit);;
let print=(Hidden_vector.print:>t->string);;
let print_inequality=(Hidden_vector.print_inequality:>t->string);;


let is_positive=(Hidden_vector.is_positive:>t->bool);;
let cmp=(Hidden_vector.cmp:>t Total_ordering.t);;
let is_zero x=unveil(x)=[];;
let eq x y=is_zero(sub x y);;

let make_zero v var=
   let  c1=coeff v var in
   if Rational.is_zero c1
   then failwith("Division by 0 in make_zero")
   else let v1=lonely c1 var in
        let v2=sub v1 v in
        dot (Rational.inverse c1) v2;;

let maple_syntax=function
[]->"0"
|(a,anv1)::peurrest->
  let s1=(fun ()->
      let sa=Rational.print_leftmost(a) in
      if ((sa="")||(sa="-"))
      then sa^anv1
      else sa^"*"^anv1)()
  and ttempf1=(fun (b,anv)->
      let sb=Rational.print_in_between(b) in
      if ((sb="+")||(sb="-"))
      then sb^anv
      else sb^"*"^anv) in
     let s2=String.concat("")(Image.image ttempf1 peurrest) in
     s1^s2;; 
 
