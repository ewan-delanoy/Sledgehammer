(*

#use"Php_analizer/php_operator.ml";;

*)


let left=Associativity.Left_associative;;
let right=Associativity.Right_associative;;
let nonassoc=Associativity.Non_associative;;

(* from http://php.net/manual/en/language.operators.precedence.php *)
type t=
     T_CLONE
    |T_NEW
    |T_LBRACKET
    |T_RBRACKET
    |T_STAR_STAR
    |T_PLUS_PLUS
    |T_MINUS_MINUS
    |T_TILDA
    |T_COERCE_TO_INT
    |T_COERCE_TO_FLOAT
    |T_COERCE_TO_STRING
    |T_COERCE_TO_ARRAY
    |T_COERCE_TO_OBJECT
    |T_COERCE_TO_BOOL
    |T_AT
    |T_INSTANCEOF
    |T_EXCLAMATION
    |T_STAR
    |T_DIVIDE
    |T_PERCENTAGE
    |T_PLUS
    |T_MINUS
    |T_DOT
    |T_LESS_LESS
    |T_MORE_MORE
    |T_LESS
    |T_LESS_EQUALS
    |T_MORE
    |T_MORE_EQUALS
    |T_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS
    |T_EQUALS_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS_EQUALS
    |T_LESS_MORE
    |T_AMPERSAND
    |T_CIRCUMFLEX
    |T_VLINE
    |T_AMPERSAND_AMPERSAND
    |T_VLINE_VLINE
    |T_QUESTION
    |T_COLON
    |T_EQUALS
    |T_PLUS_EQUALS
    |T_MINUS_EQUALS
    |T_STAR_EQUALS
    |T_STAR_STAR_EQUALS
    |T_DIVIDE_EQUALS
    |T_DOT_EQUALS
    |T_PERCENTAGE_EQUALS
    |T_AMPERSAND_EQUALS
    |T_VLINE_EQUALS
    |T_CIRCUMFLEX_EQUALS
    |T_LESS_LESS_EQUALS
    |T_MORE_MORE_EQUALS
    |T_EQUALS_MORE
    |T_AND
    |T_XOR
    |T_OR;;

let to_string=function
     T_CLONE->"clone"
    |T_NEW->"new"
    |T_LBRACKET->"["
    |T_RBRACKET->"]"
    |T_STAR_STAR->"**"
    |T_PLUS_PLUS->"++"
    |T_MINUS_MINUS->"--"
    |T_TILDA->"~"
    |T_COERCE_TO_INT->"(int)"
    |T_COERCE_TO_FLOAT->"(float)"
    |T_COERCE_TO_STRING->"(string)"
    |T_COERCE_TO_ARRAY->"(array)"
    |T_COERCE_TO_OBJECT->"(object)"
    |T_COERCE_TO_BOOL->"(bool)"
    |T_AT->"@"
    |T_INSTANCEOF->"instanceof"
    |T_EXCLAMATION->"!"
    |T_STAR->"*"
    |T_DIVIDE->"/"
    |T_PERCENTAGE->"%"
    |T_PLUS->"+"
    |T_MINUS->"-"
    |T_DOT->"."
    |T_LESS_LESS->"<<"
    |T_MORE_MORE->">>"
    |T_LESS->"<"
    |T_LESS_EQUALS->"<="
    |T_MORE->">"
    |T_MORE_EQUALS->">="
    |T_EQUALS_EQUALS->"=="
    |T_EXCLAMATION_EQUALS->"!="
    |T_EQUALS_EQUALS_EQUALS->"==="
    |T_EXCLAMATION_EQUALS_EQUALS->"!=="
    |T_LESS_MORE->"<>"
    |T_AMPERSAND->"&"
    |T_CIRCUMFLEX->"^"
    |T_VLINE->"|"
    |T_AMPERSAND_AMPERSAND->"&&"
    |T_VLINE_VLINE->"||"
    |T_QUESTION->"?"
    |T_COLON->":"
    |T_EQUALS->"="
    |T_PLUS_EQUALS->"+="
    |T_MINUS_EQUALS->"-="
    |T_STAR_EQUALS->"*="
    |T_STAR_STAR_EQUALS->"**="
    |T_DIVIDE_EQUALS->"/="
    |T_DOT_EQUALS->".="
    |T_PERCENTAGE_EQUALS->"%="
    |T_AMPERSAND_EQUALS->"&="
    |T_VLINE_EQUALS->"|="
    |T_CIRCUMFLEX_EQUALS->"^="
    |T_LESS_LESS_EQUALS->"<<="
    |T_MORE_MORE_EQUALS->">>="
    |T_EQUALS_MORE->"=>"
    |T_AND->"and"
    |T_XOR->"xor"
    |T_OR->"or";;



let pre_list_for_precedences=
[
    (nonassoc,[T_CLONE;T_NEW]);
    (left,[T_LBRACKET;T_RBRACKET]);
    (right,[T_STAR_STAR]);
    (right,[T_PLUS_PLUS;T_MINUS_MINUS;T_TILDA;T_COERCE_TO_INT;T_COERCE_TO_FLOAT;T_COERCE_TO_STRING;T_COERCE_TO_ARRAY;T_COERCE_TO_OBJECT;T_COERCE_TO_BOOL;T_AT]);
    (nonassoc,[T_INSTANCEOF]);
    (right,[T_EXCLAMATION]);
    (left,[T_STAR;T_DIVIDE;T_PERCENTAGE]);
    (left,[T_PLUS;T_MINUS;T_DOT]);
    (left,[T_LESS_LESS;T_MORE_MORE]);
    (nonassoc,[T_LESS;T_LESS_EQUALS;T_MORE;T_MORE_EQUALS]);
    (nonassoc,[T_EQUALS_EQUALS;T_EXCLAMATION_EQUALS;T_EQUALS_EQUALS_EQUALS;T_EXCLAMATION_EQUALS_EQUALS;T_LESS_MORE]);
    (left,[T_AMPERSAND]);
    (left,[T_CIRCUMFLEX]);
    (left,[T_VLINE]);
    (left,[T_AMPERSAND_AMPERSAND]);
    (left,[T_VLINE_VLINE]);
    (left,[T_QUESTION;T_COLON]);
    (left,[T_EQUALS;T_PLUS_EQUALS;T_MINUS_EQUALS;T_STAR_EQUALS;T_STAR_STAR_EQUALS;T_DIVIDE_EQUALS;T_DOT_EQUALS;T_PERCENTAGE_EQUALS;T_AMPERSAND_EQUALS;T_VLINE_EQUALS;T_CIRCUMFLEX_EQUALS;T_LESS_LESS_EQUALS;T_MORE_MORE_EQUALS;T_EQUALS_MORE]);
    (left,[T_AND]);
    (left,[T_XOR]);
    (left,[T_OR])
];;

let list_for_precedences=
  let temp1=Ennig.index_everything pre_list_for_precedences in
  Image.image (fun (i,(x,y))->(y,(i,x))) temp1;;


let precedence oprtr=
  let (_,(j,_))=Option.find_really 
  (fun (l,_)->List.mem oprtr l) list_for_precedences in
  j;;

let all_operators =
 List.flatten(Image.image snd pre_list_for_precedences);;
 
exception Unknown_operator_string of string;; 
 
let of_prudent_string s=
   Option.find_it (fun oprtr->to_string(oprtr)=s) all_operators ;;
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_operator_string(s))
  |Some(oprtr)->oprtr;;
  
let level s=
  let p0=precedence(of_string s) in
  List.filter (fun op->precedence(op)=p0) all_operators;;  
  
let all_strings=Image.image to_string all_operators;;   
 
  
  
   
