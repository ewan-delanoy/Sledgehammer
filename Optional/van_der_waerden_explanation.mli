    type triple=Tr of int * int * int
    type triple_starting_point = int
    type triple_step = int
    type list_of_elements_outside =
        Binary_constraint.list_of_elements_outside
    type list_of_elements_inside =
        Binary_constraint.list_of_elements_inside
    type explanation =
        Isolated_point
      | End_segment of triple
      | Light_triangle of triple
      | Heavy_triangle of triple * triple * triple
      | Rhomb of (int*int*int*int)*(triple list)
      | Pentagon of (int*int*int*int*int)*(triple list)
      | Saw of (int*int*int*int*int)*(triple list)
      | Orange of (int*int*int*int*int)*(triple list)
      | Bike of (int*int*int*int*int*int)*(triple list)
      | Long of int * int Tidel.set * int * int * int Tidel.set * int
    val bound_from_explanation :  explanation -> int
    val symmetrized_explanation : int -> explanation -> explanation
    val find_explanation_for_cleaned_set :
      int ->
      Binary_constraint.t ->
      triple_starting_point Tidel.set -> explanation
    val imperfect_measure_for_cleaned_set :
      int ->
      Binary_constraint.t -> triple_starting_point Tidel.set -> int
    val imperfect_measure :
      int ->
      Binary_constraint.t -> triple_starting_point Tidel.set -> int
