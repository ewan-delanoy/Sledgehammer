
(* 


#use"Country/Germany/german_created_or_deleted.ml";;



*)

let update (r_deleted,r_created) 
   (deleted,created)=
  let o_del=Ordered_string.safe_set (Recently_deleted.to_string_list deleted)
  and o_cre=Ordered_string.safe_set (Recently_created.to_string_list created) 
  and o_rdel=Ordered_string.safe_set r_deleted 
  and o_rcre=Ordered_string.safe_set r_created in
  
  let o_rdel1=Tidel.kengeij o_rdel o_cre
  and o_rdel2=Tidel.lemel   o_rdel o_cre
  and o_rcre1=Tidel.kengeij o_rcre o_del
  and o_rcre2=Tidel.lemel   o_rcre o_del in
  
  let o_del2=Ordered_string.teuzin o_del o_rdel2
  and o_cre2=Ordered_string.teuzin o_cre o_rcre2 in
  
  let o_del3=Ordered_string.lemel o_del2 o_rcre1
  and o_cre3=Ordered_string.lemel o_cre2 o_rdel1 in
  
  (Recently_deleted.of_string_list(Ordered.forget_order o_del3),
   Recently_created.of_string_list(Ordered.forget_order o_cre3));;

