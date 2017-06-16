(*

Computes for any integer interval, the largest subset avoiding a given list of pattern
(the patterns are expressed by the Translatable_interdiction module). Such subsets are called
admissible.


There are two main subfunctions : 

extended_solutions (d,n) : enumerates all admissible subsets of [1(...)n] with cardinality
 m(n)-d, where m(n) is the largest possible size.
 
convolution(d,a,b) : enumerates all admissible subsets of [1(...)(a+b)] with cardinality m(a)+m(b)-d.

Some priority rules : 

extended_solutions(d,n) can be computed only after extended_solutions(d’,n) has been computed
for all d<d’.

convolution(d,a,b) can be computed only after convolution(d’,a,b) has been computed
for all d<d’, and only if those former values are empty.


*)


type depth=int;;
type interval_length=int;;
type claw=int*(int list);;

exception NotPreComputed of int;;
exception ForgottenCase of Binary_constraint.t;;
exception NonStandardConstraint of int list;;

type int_set=int Tidel.set;;

type t=
   {
     forbidden_subsets : (Translatable_interdiction.t) list;
     hashtbl_for_solutions : (interval_length,int_set list) Hashtbl.t;
     hashtbl_for_claws : (claw,int_set list) Hashtbl.t;
   };;
   
 let make l=
   let forb=Image.image (fun (lz,m)->
     Translatable_interdiction.make (Tidel.safe_set lz) m
   ) l in
   let a1=Hashtbl.create 100
   and a2=Hashtbl.create 100
   in
   let _=Hashtbl.add a1 0 [Tidel.empty_set] in
   let _=Hashtbl.add a1 1 [Tidel.singleton 1] in
   let _=Hashtbl.add a1 2 [Ordered.S[1;2]] in
    {
     forbidden_subsets =forb;
     hashtbl_for_solutions =a1;
     hashtbl_for_claws =a2;
   };;
   
   
 let solutions cptr (n:interval_length)=
   if Hashtbl.mem cptr.hashtbl_for_solutions n
  then Hashtbl.find cptr.hashtbl_for_solutions n
  else raise(NotPreComputed n);;
 
   
 let measure cptr n=Tidel.length(List.hd(solutions cptr n));;  
   
 
 let direct_sum cptr a b=
  let s=a+b in
  if Hashtbl.mem cptr.hashtbl_for_solutions s
  then  let meas=measure cptr in
        if meas(s)=meas(a)+meas(b)
        then Hashtbl.find cptr.hashtbl_for_solutions s
        else failwith("Sum is not direct")
  else 
  let ans_a=Hashtbl.find cptr.hashtbl_for_solutions a
  and old_ans_b=Hashtbl.find cptr.hashtbl_for_solutions b in
  let ans_b=Image.image (fun (Ordered.S lz)->Ordered.S(Image.image (fun j->j+a) lz)) old_ans_b in
  let y=Translatable_interdiction.explicit_watched_product cptr.forbidden_subsets ans_a ans_b in
  if y=[]
  then failwith("Sum is not direct (worst case)")
  else
  let _=Hashtbl.add cptr.hashtbl_for_solutions s y in
  y;;
  
let compute_claw cptr ((n,old_l):claw)=
  if Hashtbl.mem cptr.hashtbl_for_claws (n,old_l)
  then Hashtbl.find cptr.hashtbl_for_claws (n,old_l)
  else
  let l=0::(old_l@[n+1]) in
  let temp1=Listennou.universal_delta_list l in
  let temp2=List.filter (fun (a,b)->b>a+1) temp1 in
  let temp3=Image.image (
     fun (a,b)->
      let temp1=solutions cptr (b-a-1) 
      and tempf=(fun (Ordered.S lz)->Ordered.S(Image.image (fun j->j+a) lz) ) in
      Image.image tempf temp1
  ) temp2 in
  let y=Translatable_interdiction.explicit_watched_big_product cptr.forbidden_subsets temp3 in
  let _=Hashtbl.add cptr.hashtbl_for_claws (n,old_l) y in
  y;;
  
let measure_claw cptr ((n,old_l):claw)=
  let l=0::(old_l@[n+1]) in
  let temp1=Listennou.universal_delta_list l in
  let temp2=List.filter (fun (a,b)->b>a+1) temp1 in
  let temp3=Image.image (
     fun (a,b)->
      let temp1=solutions cptr (b-a-1) in
      Tidel.length (List.hd temp1)
  ) temp2 in
 Basic.big_sum temp3;;  
  
let test_for_nonstandard_constraint l=
   if List.length(l)<>3 then true else
   let lf=(fun i->List.nth l (i-1)) in
   let a=lf 1 and b=lf 2 and c=lf 3 in
   c-b<>b-a;;
   
let check cptr lz=
  let z=Tidel.safe_set lz in
  Translatable_interdiction.big_check cptr.forbidden_subsets z;;
  
   
let indirect_sum cptr n cases other_side=
  let temp1=Image.image (fun l->Binary_constraint.handy_constructor (l,[])) cases
  and temp2=Image.image (fun l->Binary_constraint.handy_constructor ([],l)) other_side
  in
  let opt=Logical_subdivision.find_forgotten_case (temp1@temp2) in
  if opt<>None
  then raise(ForgottenCase(Option.unpack opt))
  else
  let opt2=Option.find_it (check cptr) other_side in
  if opt2<>None
  then raise(NonStandardConstraint(Option.unpack opt2))
  else
  let temp1=Image.image (fun l->(l,measure_claw cptr (n,l)) ) cases in
  let (_,temp2)=Max.maximize_it_with_care snd temp1 in
  let temp3=Image.image (fun (l,m)->Ordered_bare_set.safe_set2(compute_claw cptr (n,l))) temp2 in
  let temp4=Ordered_bare_set.big_teuzin temp3 in
  let y=Ordered.forget_order temp4 in
  let _=Hashtbl.add cptr.hashtbl_for_solutions n y in
  (y:int_set list);;  
   
let decompose cptr n=
  let (_,temp1)=Min.minimize_it_with_care (fun j->
   (measure cptr j)+(measure cptr (n-j)) ) (Ennig.ennig 1 (n-1)) in
  let j0=List.hd temp1 in
  direct_sum cptr j0 (n-j0);;
   
   
(*   
  

  
  
 *)  