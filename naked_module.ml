(*

#use"naked_module.ml";;

A module name, or a candidate for one. Should contain no slashes.

*)

type t=N of string;;

let of_string s=N s;; 
let to_string (N s)=s;;


let ocaml_name w=
  let s=to_string w in
  "Naked_module"^".of_string(\""^s^"\")";;    

  
   