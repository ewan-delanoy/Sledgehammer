(*

#use"Php_analizer/php_parser.ml";;

*)

type 'a t= 
((Positioned_php_token_list.t )->
((('a)*Php_char_range.t*(Positioned_php_token_list.t)) option));;

 
let parse (f:'a t) l=f l;;  
 

  
