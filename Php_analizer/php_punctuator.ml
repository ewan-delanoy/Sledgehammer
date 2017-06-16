(*

#use"Php_analizer/php_punctuator.ml";;

*)



(* 

from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#operators-and-punctuators 
I do not consider =& as a single token, but as a composite =+&.

*)

type t=
     T_LPARENTHESIS
    |T_RPARENTHESIS
    |T_LBRACE
    |T_RBRACE
    |T_ARROW
    |T_SEMICOLON
    |T_COMMA
    |T_COLON_COLON;;

let to_string=function
     T_LPARENTHESIS->"("
    |T_RPARENTHESIS->")"
    |T_LBRACE->"{"
    |T_RBRACE->"}"
    |T_ARROW->"->"
    |T_SEMICOLON->";"
    |T_COMMA->","
    |T_COLON_COLON->"::";;




let all_punctuators =
[
     T_LPARENTHESIS;
     T_RPARENTHESIS;
     T_LBRACE;
     T_RBRACE;
     T_ARROW;
     T_SEMICOLON;
     T_COMMA;
     T_COLON_COLON

];;
 
 
exception Unknown_punctuator_string of string;; 

let of_prudent_string s=
  Option.find_it (fun oprtr->to_string(oprtr)=s) all_punctuators;; 
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_punctuator_string(s))
  |Some(oprtr)->oprtr;;
  
let all_strings=Image.image to_string all_punctuators;;      
 
  
  
   
