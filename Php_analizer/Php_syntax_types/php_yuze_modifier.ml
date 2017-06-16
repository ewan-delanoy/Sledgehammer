(*

#use"Php_analizer/Php_syntax_types/php_yuze_modifier.ml";;

*)

type t=
    Function
   |Const
   |All;;
  
   
let from_lexemes l=
  match l with
  []->None
  |a1::l1->
    let p1=Positioned_php_token.fst a1 in
    if p1=Php_token.kwd"function" 
    then Some(Function,l1)
    else 
    if p1=Php_token.kwd"const" 
    then Some(Const,l1)
    else Some(All,l);;
  
    