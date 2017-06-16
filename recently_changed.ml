(*

#use"recently_changed.ml";;

*)

type t=RC of string list;;

let of_string_list l=RC l;;
let to_string_list (RC l)=l;;
