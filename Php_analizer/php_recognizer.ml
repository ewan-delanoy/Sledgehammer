(*

#use"Php_analizer/php_recognizer.ml";;

*)

type t=( Positioned_php_token_list.t -> 
(Php_char_range.t * Positioned_php_token_list.t) option );;

let recognize (f:t) l=f l;;