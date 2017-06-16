(*

#use"Haag1/Haag2/please_test_me.ml";;

*)


(*

This is a string : "*)"

*)


let some_value=5;;

module Boogie=struct

let b=6;;

module Woogie=struct

let parker=7;;

module Andrew=struct

let d=8;;

let first_user=d+1;;

end;;

let second_user=Andrew.d+2;;

let fleury=9;;

end;;

let third_user=Woogie.Andrew.d+3;;

let burp=10;;

end;;



let fourth_user=Boogie.Woogie.Andrew.d+3;;


let g=48+Boogie.Woogie.parker;;
let h=49+some_value;;


(*

let f x=match x.Ocaml_gsyntax_item.category with                                                                     
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->(1,x)
  | Ocaml_gsyntax_category.Module_opener->(2,x)
  | Ocaml_gsyntax_category.Module_closer->(3,x)
  | Ocaml_gsyntax_category.Module_inclusion->(4,x);;

module Mood=struct

type mytype= A |B |C |D |E;;

end;;

let f x=match x with Mood.A|Mood.B|Mood.C->(1,x) |Mood.D->(2,Mood.D) |Mood.E->(3,Mood.E);;


module Weak=struct
let e=6;;
end;;

*)

