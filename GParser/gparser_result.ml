(*

#use"GParser/hparser_result.ml";;

*)


type t={
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
   disjunction_index : int option;
};;

let whole_range x=x.whole_range;;
let important_ranges x=x.important_ranges;;
let final_cursor_position x=x.final_cursor_position;;
let disjunction_index x=x.disjunction_index;;

let veil b c d e={
   whole_range =b;
   important_ranges =c;
   final_cursor_position =d; 
   disjunction_index=e;
};;


