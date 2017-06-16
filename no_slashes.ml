(*

#use"no_slashes.ml";;

*)

type t=NS of string;;

exception Slash_at of string*int;;

let to_string(NS s)=s;;

let of_string s=
  let n=String.length s in
  let rec tempf=(fun i->
  if i>n
  then NS s
  else if (String.get s (i-1))='/'
       then raise(Slash_at(s,i))
       else tempf(i+1)
  ) in
  tempf 1;;
  
