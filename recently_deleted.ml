(*

#use"recently_deleted.ml";;

*)

type t=RD of string list;;

let of_string_list l=RD l;;
let to_string_list (RD l)=l;;
