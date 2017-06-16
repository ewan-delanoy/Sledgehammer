type t 
val unveil :
  t ->
  Vector.t * Simplex_relation.t list * Variable.t list *
  Descriptive_system.t * Descriptive_system.t * Vector.t
val main_vector : t -> Vector.t
val constraints : t -> Simplex_relation.t list
val visible_variables : t -> Variable.t list
val descriptive_system : t -> Descriptive_system.t
val initial_descriptive_system : t -> Descriptive_system.t
val cleanup : t -> t
type tt = TR of (int * Variable.t) list * t
val switch : Variable.t -> int -> t -> t
val switch_and_remember : Variable.t -> int -> tt -> tt
val uncurrified_switch : t -> int * Variable.t -> t
val uncurrified_switch_and_remember : tt -> int * Variable.t -> tt
val fold_switch : t -> (int * Variable.t) list -> t
val deduce_standard_preserving_lines_from_pivot : t -> Variable.t -> int list
val basic_variables : t -> Ordered_variable.set
val standard_preserving_pairs : t -> (int * Variable.t) list
val is_strongly_progressive : t -> 'a * Variable.t -> bool
val is_progressive : t -> int * Variable.t -> bool
val progressive_pairs : t -> (int * Variable.t) list
val find_best_weakly_progressive_pair :
  t -> (int * Variable.t) list -> int * Variable.t
val find_best_strongly_progressive_pair :
  t -> (int * Variable.t) list -> int * Variable.t
val find_best_progressive_pair : t -> (int * Variable.t) option
val has_correct_pair : t -> bool
val number_of_positive_coefficients : t -> int
val first_chain : t -> (int * Variable.t) list
val first_breakthrough : t -> t
val double_presentation :
  (Variable.t * Rational.t) list -> string * string * string
val optimal_binding : t -> (Variable.t * Rational.t) list
val solve :
  t ->
  Rational.t * Vector.t * Rational.t * Vector.t *
  (Variable.t * Rational.t) list *
  (Rational.t * Variable.t * Rational.t) list
val deduce_latex_solution :
  Rational.t * Vector.t * Rational.t * Vector.t *
  (Variable.t * Rational.t) list * (Rational.t * 'a * Rational.t) list ->
  string * string * string * string
val latex_solution : t -> string * string * string * string
val impose_simple_equality_naively : Variable.t -> Rational.t -> t -> t
val impose_several_simple_equalities_naively :
  (Variable.t * Rational.t) list -> t -> t
val impose_several_simple_equalities :
  (Variable.t * Rational.t) list -> t -> t
val impose_simple_equality : Variable.t -> Rational.t -> t -> t
val impose_extremal_simple_equalities :
  Variable.t list -> Variable.t list -> t -> t
val uncurrified_impose_simple_equality : t -> Variable.t * Rational.t -> t
val impose_equalities : (Variable.t * Vector.t) list -> t -> t
val see_solution : t -> t
val well_at_point : t -> Variable.t -> Simplex_relation.t list
val gifts : t -> Simplex_relation.t list
val eval_variable : t -> Variable.t -> Vector.t
val eval_vector : t -> Vector.t -> Vector.t
type int_set = int Tidel.set
val isolate_items : int_set -> t -> t
val remove_items : int_set -> t -> t
val read : t -> string
val print_out : Format.formatter -> t -> unit
val a_below_1 : int -> Simplex_relation.t
val original_descr : Simplex_relation.t list -> Descriptive_system.t
type int_interdiction = int Nontranslatable_interdiction.t
val general_theodoracopulos_rel : int_interdiction -> Simplex_relation.t
type int_variety = int Theodoracopulos.variety
val system_from_theodoracopulos : int_variety -> t
val set_value_of_ne_element : t -> int -> Rational.t -> t
val add_n1_element : t -> int -> t
val add_e1_element : t -> int -> t
val add_n1_elements : int list -> t -> t
val add_e1_elements : int list -> t -> t
val add_ne_elements : int list * int list -> t -> t
val system_from_theodoracopulos_and_inert_variables :
  int_variety -> int_set -> t
val system_from_impure_theodoracopulos_and_inert_variables :
  int_variety -> int_set -> t
val change_main_vector : t -> Vector.t -> t
val rewrite_main_vector : t -> Vector.t -> t
val flatten : t -> t
val standard_constructor : Vector.t list -> Vector.t -> t
val cumulative_van_der_waerden : int -> t
val cumulative_van_der_waerden_with_constraints :
  int -> (Nontranslatable_interdiction.int_set * int) list -> t
val constructor_from_absolute_values :
  (Vector.t * Rational.t) list -> Vector.t -> t
val maximum_value_from_first_breakthrough : t -> Rational.t
val maximum_value : t -> Rational.t