type t 

val zero : t
val one : t

val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t
type positive_int = int
val pow : t -> positive_int -> t
val power_of_ten : int -> t
val is_positive : t -> bool
val is_negative : t -> bool
val is_nonpositive : t -> bool
val is_nonnegative : t -> bool
val is_zero : t -> bool
val lt : t -> t -> bool
val gt : t -> t -> bool
val eq : t -> t -> bool
val leq : t -> t -> bool
val geq : t -> t -> bool
val cmp : t Total_ordering.t
val max : t -> t -> t
val abs : t -> t
type precision = int
val of_int : int -> t
val of_rational : precision -> Rational.t -> t
val of_float : precision -> float -> t
val of_string : string -> t
val to_rational : t -> Rational.t
val to_string : t -> string
val to_float : t -> float
val to_int : t -> int
type interval_step = t
type interval_starting_point = t
type interval_bound_for_endpoint = t
val subdivized_interval :
  interval_starting_point ->
  interval_bound_for_endpoint ->
  interval_step -> interval_starting_point list
val floor : t -> t
val ceil : t -> t
val nearest_integer : t -> t
val big_sum : t list -> t
val precise_sqrt : precision -> Rational.t -> t
val sqrt : t -> t
val precise_inverse : precision -> Rational.t -> t
val inverse : t -> t
val general_floor_transform : precision -> t -> t
val general_rounded_transform : precision -> t -> t
val rounded_transform : t -> t
val rational_mean : t list -> Rational.t
val mean : t list -> t
val standard_mean : t list -> t
val ugly_mean : (Rational.t * t) list -> t
val get_naem_constant : unit -> int
val set_naem_constant : int -> unit
val ugly_naem : t -> (Rational.t * t) list -> t
val quartiles_and_extremities : t list -> t * t * t * t * t
val find_decimal_in_between : Rational.t -> Rational.t -> t
val find_large_decimal_in_between : Rational.t -> Rational.t -> t
val interval : t -> t -> t -> t list 
val int_interval : int -> int -> t -> t list
val evaluate_polynomial : Polynomial.t -> t -> t
val print_out : Format.formatter -> t -> unit
