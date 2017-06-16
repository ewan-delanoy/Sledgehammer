(*

#use"isolated_occurences.ml";;

Used to detect mentions of previously defined names in
the same OCaml module.

An occurrence of a substring is isolated when it 
cannot be extended to a meaningful Ocaml name. So we look at
the surrounding characters, on the left and on the right.


*)

module Private=struct

exception Unclear_left_char of char;;
exception Unclear_right_char of char;;

let rejected_left_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_left_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let rejected_right_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_right_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let test_for_left_admissiblity c=
   if List.mem c rejected_left_chars then false else
   if List.mem c admitted_left_chars then true else
   raise(Unclear_left_char(c));;
   
let test_for_right_admissiblity c=
   if List.mem c rejected_right_chars then false else
   if List.mem c admitted_right_chars then true else
   raise(Unclear_right_char(c));;   
   
let leftmost_small_test  s j=
   if j=0 
   then true 
   else test_for_left_admissiblity (String.get s (j-1));;

let rightmost_small_test  s j=
   if j=((String.length s)+1) 
   then true 
   else test_for_right_admissiblity (String.get s (j-1));;   
   

end;;

let of_in substr s=
  let l_substr=String.length substr 
  and n=String.length(s) in
  let main_test= (
    fun k->
      if ((String.sub s (k-1) l_substr)<>substr)
      then false
      else 
      ( Private.leftmost_small_test s (k-1) )
      &&
      ( Private.rightmost_small_test s (k+l_substr) )
      
  ) in
  Option.filter_and_unpack(
     fun k->
       if main_test k
       then Some(k,k+l_substr-1)
       else None
  ) (Ennig.ennig 1 (n+1-l_substr));;

   
(*   
   
of_in "garfield" 
"let x=garfield in let y=subgarfield and z=garfield2 in";;

of_in "garfield" "garfield is a cat";;

of_in "Boogie.Woogie.c" "48+Boogie.Woogie.c";;


*)   