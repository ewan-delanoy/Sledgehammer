(*

Concrete values of type My_str.regexp.

#use"my_str_example.ml";;

*)

let capital_letter=My_str.veil "[A-Z]";;
let letters=My_str.veil "[A-Za-z1-9_']*";;
let nonletter=My_str.veil "[^A-Za-z1-9_']";;
let white=My_str.veil "[ \n\r\t]";;
let maybe_whites=My_str.star white;;
let some_whites=My_str.plus white;;
 

let delimited_module_name=My_str.big_concat
  [
    nonletter;capital_letter;letters;nonletter
  ];;

let bare_module_name=My_str.big_concat
  [
    capital_letter;letters
  ];;

let include_case=
  let left_part=My_str.veil "[ \n\r\t]+include[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let open_case=
  let left_part=My_str.veil "[ \n\r\t]+open[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let moodle_case=
  let left_part=My_str.big_concat 
  [some_whites;My_str.veil"module";some_whites;
   bare_module_name;My_str.veil"=";maybe_whites]
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let pointed_case=
  let left_part=nonletter
  and center_part=bare_module_name 
  and right_part=My_str.regexp_string "." in
  My_str.create_centered_regexp left_part center_part right_part;; 

let moodle_cases=[include_case;open_case;moodle_case;pointed_case];;
let index_for_include_case=1;;
let index_for_open_case=2;;
let index_for_moodle_case=3;;
let index_for_pointed_case=4;;


(*
My_str.centered_regexp_match include_case " include Peggy;; " 1;;
My_str.centered_regexp_match include_case " include_once;; " 1;;
My_str.centered_regexp_match moodle_case " module Amy=Lawson " 1;;
My_str.centered_regexp_match pointed_case " 57+Everybody.talking-78 " 4;;
*)

 let capital_letter=My_str.veil "[A-Z]";;
 
 let alphanumeric=
    My_str.big_or
      [ 
     	My_str.veil "[a-z]";
     	My_str.veil "[A-Z]";
     	My_str.veil "[0-9]";
     	My_str.regexp_string "_";
      ];;
 
 let alphanumerics=My_str.plus alphanumeric;;
 
 let beginning_of_module_definition=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string "=";
         some_whites;
         My_str.regexp_string "struct";
         white;
          
      ]);;
      
 let beginning_of_module_reminder=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string ":";
         some_whites;
         My_str.regexp_string "sig";
         white;
          
      ]);;
      
 let beginning_of_module_type_definition=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         My_str.regexp_string "type";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string "=";
         some_whites;
         My_str.regexp_string "sig";
         white;
          
      ]);;           
      
      
 let the_end=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "end";
         white;
          
      ]);;       
      