(*

#use"list_with_indices.ml";;

*)

exception Bad_set_of_indices;;

let list_with_indices l=
  let n=List.length l in
  let temp1=Ennig.doyle (fun i->Option.find_it(fun p->fst(p)=i) l) 1 n in
  if List.mem None temp1
  then raise(Bad_set_of_indices)
  else
  Ennig.doyle (fun
     i->snd(Option.find_really(fun p->fst(p)=i) l)
  ) 1 n;;

(*

list_with_indices [3,"a";1,"b";2,"c"];;

*)  
