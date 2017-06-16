
type result=Lower |Equal |Greater;;

type 'a t=('a->'a->result);;

let leq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let lt (computer:'a t) x y=(computer(x)(y)=Lower);;   
 
 let geq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let gt (computer:'a t) x y=(computer(x)(y)=Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Lower
     else if f(y)(x)
          then Greater
          else Equal
   ) in
   (temp1:'a t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
        else if g(x)(y)
             then Equal
             else if x<y
                  then Lower
                  else Greater
  ) in
  (answer: 'a t);;
 
 let standard=((fun x y->
    if x=y
    then Equal
    else if x<y
         then Lower
         else Greater
 ): 'a t);;
 
 let completion f (g:'a t)=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
         else g(x)(y)
  ) in
  (answer: 'a t);;
 
 let product (f:'a t) (g:'b t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Equal 
     then t
     else g y1 y2
 ): ('a*'b) t);;
 
 let triple_product (f:'a t) (g:'b t) (h:'c t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) t);;
 
 let rec lex_compare (f:'a t)=
  let rec tempf=(
    fun l1 l2->
     if l1=l2 then Equal else
     if l1=[] then Lower else
     if l2=[] then Greater else
     let t=f(List.hd l1)(List.hd l2) in
     if t<>Equal then t else
     tempf (List.tl l1) (List.tl l2)) in
     (tempf:>( ('a list) t));;
 


let silex_compare (f:'a t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) t));;
 

let from_list (l:'a list)=
  let tempc=(fun x y->
  let rec tempf=(function
   []->(x<y)
   |u::peurrest->if u=x then List.mem(y)(peurrest)
                 else if u=y then false
                 else tempf(peurrest)
  ) in
  tempf l) in
  from_lt tempc;;

let min (f:'a t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a t));;


let for_characters=let tempf=(fun x y->
  if x<y then Lower else
  if y<x then Greater else
  Equal
  ) in (tempf:>char t);;



 
 
 
 
 
 
