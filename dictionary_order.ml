(*

#use"dictionary_order.ml";;

*)


let dictionary_order=
 ((fun s1 s2->
   let m1=String.length s1
   and m2=String.length s2
   in
   let m=min(m1)(m2) in
   match Option.find_it (fun j->(String.get s1 j)<>(String.get s2 j)) (Ennig.ennig 0 (m-1)) with
   None->Total_ordering.standard m1 m2
   |Some(j)->Total_ordering.standard (String.get s1 j) (String.get s2 j) 
 ) : string Total_ordering.t);;
 
 