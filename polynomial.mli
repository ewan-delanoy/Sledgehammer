type t 
val unveil : t -> Rational.t list

val zero : t
val one : t
val constant : Rational.t -> t
val int_constant : int -> t
type exponent = int
val monomial : Rational.t -> exponent -> t
val constructor : (Rational.t * exponent) list -> t
val short_constructor : Rational.t list -> t
val int_constructor : (int * exponent) list -> t
val short_int_constructor : int list -> t
val constructor_from_roots : Rational.t list -> t
val binomial : int -> t

val add : t -> t -> t
val big_sum : t list -> t
val dot : Rational.t -> t -> t
val linear_combination : (Rational.t * t) list -> t
val sub : t -> t -> t
val mult : t -> t -> t
val compose : t -> t -> t
val gcd : t -> t -> t

val eval : t -> Rational.t -> Rational.t
val eval_at_int : t -> int -> Rational.t
val coeff : t -> exponent -> Rational.t
val degree : t -> exponent
val leading_coefficient : t -> Rational.t
val cmp : t Total_ordering.t
val make_unitary : t -> t
val euclid : t -> t -> t * t
val euclid_quotient : t -> t -> t
val euclid_remainder : t -> t -> t
val exact_division : t -> t -> t
val taylor : t -> Rational.t -> Rational.t list
val derivative : t -> t
val integrate : t -> t
val integrate_and_translate : t -> Rational.t -> t
val gauss_operation : t -> t
val unique_standardizer : t -> Rational.t



val laplace_interpolation :
  Rational.t * (Rational.t * Rational.t) ->
  Rational.t * (Rational.t * Rational.t) -> t

val signs_on_the_integers :
	t -> int -> int -> (int * int * char) list

val current_name_for_x : string ref
val print : t -> string
val print_democratically : t -> string
val print_for_gp : t -> string
val print_for_ocaml : t -> string
val print_out : Format.formatter -> t -> unit
val print_out_for_gp : Format.formatter -> t -> unit
val print_out_for_ocaml : Format.formatter -> t -> unit


