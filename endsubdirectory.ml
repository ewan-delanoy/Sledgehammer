(*

String appearing as a subsubdirectory name, at the moment it is used.

#use"endsubdirectory.ml";;

*)

type t=ESD of string;;

let of_string (ESD s)=s;;

let to_string s=ESD s;;

