
type t 
val frac : int -> int -> t
val string_frac : string -> string -> t
val numerator : t -> string 
val denominator : t -> string 
val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t
val div : t -> t -> t
val pow : t -> int -> t
val dot : int -> t -> t
val is_an_integer : t -> bool
val is_negative : t -> bool
val is_zero : t -> bool
val is_nonzero : t -> bool
val is_positive : t -> bool
val sign : t -> char
val is_nonnegative : t -> bool
val is_nonpositive : t -> bool
val minus : t -> t
val compare : t -> t -> bool
val minimize : ('a -> t) -> 'a list -> 'a * t
val maximize : ('a -> t) -> 'a list -> 'a * t
val lt : t -> t -> bool
val eq : t -> t -> bool
val gt : t -> t -> bool
val leq : t -> t -> bool
val geq : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val cmp : t Total_ordering.t
val minus_one : t
val zero : t
val one : t
val opposite : t -> t
val inverse : t -> t
val opposite_of_inverse : t -> t
val floor : t -> t
val ceil : t -> t
val nearest_integer : t -> t
val big_sum : t list -> t
val mean : t list -> t
val to_num : t -> Num.num
val to_int : t -> int
val approx_fix : int -> t -> string
val approx : t -> string
val insider : t -> t -> t
val lower_depth : t -> int
val upper_depth : t -> int
val print_leftmost : t -> string
val print_in_between : t -> string
val print : t -> string
val print_out : Format.formatter -> t -> unit
val of_int : int -> t
val ocaml_name : t -> string
val latex_name : t -> string 
val gauss_multiplier : t list -> t
val gauss_operation : (t * 'a) list -> (t * 'a) list
val sign_preserving_gauss_operation : (t * 'a) list -> (t * 'a) list
val big_product : t list -> t
val sum_of_products : t list list -> t

