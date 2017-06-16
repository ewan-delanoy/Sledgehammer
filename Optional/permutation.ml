
type 'a t=('a list)*('a list);;




let enumerate (l:'a list)=
let rec develop_it accu=(function
[]->List.rev(accu)
|(graet,da_ober)::peurrest->match da_ober with
[]->develop_it((l,List.rev(graet))::accu)(peurrest)
|l->
let temp1=List.rev_map(function (x,y)->
(x::graet,y))(Three_parts.complemented_points(l)) in
develop_it(accu)(List.rev_append(temp1)(peurrest))) in
let answer=develop_it [] [([],l)] in
(answer:> ('a t) list);;


let action (perm:'a t) l=
 let associator=List.combine(fst perm)(snd perm) in
 Image.image (function 
  x->List.assoc x associator) l;;

let compose (perm1:'a t) (perm2:'a t)=
  let answer=(fst perm2,action perm1 (snd perm2)) in
  (answer:> ('a t));;
