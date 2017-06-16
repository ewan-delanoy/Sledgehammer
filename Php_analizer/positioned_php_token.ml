(*

#use"Php_analizer/positioned_php_token.ml";;

*)

type t=
    PPL of Php_token.t*(Lexing.position * Lexing.position);; 

let make x y=PPL(x,y);;
let unveil (PPL(x,y))=(x,y);;
let fst (PPL(x,y))=x;;
let snd (PPL(x,y))=y;;

let file (PPL(_,(y1,_)))=y1.Lexing.pos_fname;;

let print (PPL(x,y))=
  let s=Php_token.content x in
  if String.length(s)>50
  then "\xe2\x8c\x98...\xe2\x8c\x98 "
  else "\xe2\x8c\x98 "^s^"\xe2\x8c\x98 ";;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (print x);;


