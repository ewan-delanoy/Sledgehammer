(*

#use"Php_analizer/php_generalizer.ml";;

The values returned by the pair function below should
be compatible with Php_short_selector.all_string_constants list
and with the Php_constructible_recognizer.pair_for_disjunction
and Php_constructible_recognizer.associator_for_disjunction values.


   
*)

type t=
   Zero_or_one
  |Zero_or_more
  |One_or_more
  |One_or_more_with_right_end_removed;;
  
let all=
  [Zero_or_one;Zero_or_more;One_or_more;One_or_more_with_right_end_removed];;  
  
let pair=function
   Zero_or_one->("_l_","_r?_")
  |Zero_or_more->("_l_","_r*_")
  |One_or_more->("_l_","_r+_")
  |One_or_more_with_right_end_removed->("_l_","_r~_");;  
  
 let all_pairs=Image.image pair all;; 
