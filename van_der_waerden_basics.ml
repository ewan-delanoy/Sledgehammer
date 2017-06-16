
type maximal_value_for_r=int

type int_set= int Ordered.old_set;;
type int_set_set=int Ordered_bare_set.set2;;
type ii= (int,int) Memoized.map;;

let individual n (r:maximal_value_for_r)=
  let bound=((n-1)/2) in
  let real_r=
  (function ()->if r<1 then bound else min(r)(bound) )() in 
  let temp=Ennig.doyle(function j->Tidel.safe_set[n-(2*j);n-j;n])(1)(real_r) in
  ((Ordered_bare_set.diforchan temp):int_set_set);;


let semi_global n1 n2 (r:maximal_value_for_r)=
  let temp1=Ennig.doyle (function small_n->individual small_n r) n1 n2 in
  ((Ordered_bare_set.big_teuzin temp1):int_set_set);;
  
let global n r=semi_global 3 n r;;  
  

let test_for_admissibility (z:int_set)=
   let temp1=Uple.list_of_triples(Ordered.forget_order z) in
   Option.find_it (fun (i,j,k)->j-i=k-j) temp1;;
   
 let is_admissible (z:int_set)=((test_for_admissibility z)=None);;  
   
 let completers (old_i,old_j)=  
    let i=min(old_i)(old_j) and j=max(old_i)(old_j) in
    if i=j then [] else
    let bowl=((j-i) mod 2 =0) in
    let temp1=[(2*i-j,true);((i+j)/2,bowl);(2*j-i,true)] in
    let temp2=List.filter snd temp1 in
    let temp3=Image.image fst temp2 in
    List.filter (function x->x>0) temp3;;
    
 let local_completers n (z:int_set)=
   let temp1=Uple.list_of_pairs(Ordered.forget_order z) in
   let temp2=Image.image (fun (i,j)->
      Tidel.safe_set(List.filter (fun x->x<=n) (completers (i,j)))
   ) temp1 in
   ((Tidel.big_teuzin temp2): int_set);;
   
let completed_binary_constraint n rbc=
   let (old_n1,e1)=Binary_constraint.unveil rbc in
   let temp1=local_completers n e1 in
   let new_n1=Tidel.teuzin temp1 old_n1 in
   Binary_constraint.usual_constructor (new_n1,e1);;   

let mini_table_for_h=  
[0;1;2;2;3;4;4;4;4;5;5;6;6;7;8;8;8;8;8;8;9;9;9;9;10;10;11;11;11;11;12];;

let mini_h=((Memoized.make(function j->
   List.nth(mini_table_for_h)(j))):ii);;
   
let is_not_deducible_from_below n=
 let c=mini_h(n) in
 Ennig.for_all(function j->mini_h(j)+mini_h(n-j)>c)(1)(n-1);;

let is_not_deducible_from_above n=mini_h(n)<mini_h(n+1);;

let is_irreducible n=
 (is_not_deducible_from_below n)&&
 (is_not_deducible_from_above n);;
 

 let irreducible_intervals n=
   let temp1=Int_uple.list_of_pairs(n) 
   and tester=(function (x,y)->
     let l=y-x+1 in
     if l=n 
     then false (*in this case (x,y)=(1,n), and we cannot argue in a circle. *) 
     else
     if is_not_deducible_from_below(l)
     then if is_not_deducible_from_above(l)
          then true
          else (l=n-1)
     else false
   ) in
   let temp2=List.filter(tester)(temp1) in
   temp2;;
   
let all_feline_intervals n=
  let interval_for_l=Ennig.ennig(4)((n+1)/2) in
  let temp1=Image.image(function l->
      Ennig.doyle(function r->(l,r))(2)((n-1)/(l-1))  )(interval_for_l) in
  let range_for_lr=List.flatten(temp1) in
  let temp2=Image.image(function (l,r)->
      Ennig.doyle(function i->(i,r,l))(1)(n-(l-1)*r)  )(range_for_lr) in
  List.flatten(temp2);;
 
let feline_interval_is_irreducible n (i,r,l)=
 if (i,r,l)=(1,1,n) then false else
 if is_not_deducible_from_below(l)
 then if is_not_deducible_from_above(l)
      then true
      else (i-r<1)&&(i+l*r>n)
 else false;;
 
let irreducible_feline_intervals n=
  List.filter(feline_interval_is_irreducible n)
  (all_feline_intervals n);;
    
let test_from_below x0 a b (l:int_set)=
 let tempf=(function j->Tidel.ental (Tidel.safe_set [j;2*j-x0]) l) in
 List.for_all(tempf)(Ennig.ennig a ((x0+b)/2) );;


  
