(*

#use"Ocaml_analysis/longest_shared_module.ml";;

Given two ocaml items, find the longest namespace (module, in fact)
in which they are both contained.

For example, for the two values Foo.Baz.Bar.x and Foo.Baz.Barmaid.y, the 
answer is Foo.Baz.

*)

let lsm name1 name2=
  let m=min(String.length name1)(String.length name2) in
  let opt=Option.find_it(fun k->
     (Strung.get name1 k)<>(Strung.get name2 k)
  )(Ennig.ennig 1 m) in
  let j1=(fun ()->if opt=None then m else (Option.unpack opt)-1)() in
  let shared_part=Cull_string.beginning j1 name1 in
  let p1=Substring.rightmost_index_of_in "." shared_part in
  if p1<0 
  then "" 
  else Cull_string.beginning (p1-1) shared_part;;
  
(*

lsm "weapon" "bag";;

lsm "May.weapon" "May.bag";;

lsm "Foo.Baz.Bar.x" "Foo.Baz.Barmaid.y";;

*)  

   
