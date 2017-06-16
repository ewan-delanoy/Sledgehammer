(*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=
     Kwd of Php_keyword.t
    |Punct of Php_punctuator.t
    |Op of Php_operator.t;;

let to_string=function
      (Kwd s)->Php_keyword.to_string s
     |(Punct s)->Php_punctuator.to_string s
     |(Op s)->Php_operator.to_string s;;
  
let all_pairs=
   ( Image.image (fun kwd->(Php_keyword.to_string kwd,Kwd kwd)) Php_keyword.all_keywords)
  @( Image.image (fun punct->(Php_punctuator.to_string punct,Punct punct)) Php_punctuator.all_punctuators)
  @( Image.image (fun op->(Php_operator.to_string op,Op op)) Php_operator.all_operators);;  

let all_string_constants=Image.image fst all_pairs;;

let all=Image.image snd all_pairs;;

exception Unknown of string;;

let of_string s=
   try List.assoc s all_pairs with
   _->raise(Unknown(s));;

let putative_of_string s=try (Some(of_string s)) with _->None;;

let token_category=function
      Kwd(_)           ->Token_category.Keyword
     |Punct(_)         ->Token_category.Punctuator
     |Op(_)            ->Token_category.Operator;;

