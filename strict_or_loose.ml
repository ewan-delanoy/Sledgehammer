(*

#use"strict_or_loose.ml";;
   
*)

type t=Strict |Loose;;

let all=[Strict;Loose];;

let to_string=function
   Strict->"strict"
  |Loose->"loose";;

let test sl x y=match sl with
    Strict->x<y
   |Loose->x<=y;;  