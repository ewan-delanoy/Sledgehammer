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
type pusher=int;;
type int_set=int Tidel.set;;

type t=
   {
     forbidden_subsets : (Translatable_interdiction.t) list;
     hashtbl_for_extsol : (depth*interval_length,int_set list) Hashtbl.t;
     hashtbl_for_convolution : (depth*interval_length*interval_length,int_set list) Hashtbl.t;
     hashtbl_for_partial_convolution : (depth*interval_length*interval_length*int,int_set list) Hashtbl.t;
     hashtbl_for_levels : (interval_length,int_set list) Hashtbl.t;
     hashtbl_for_measure : (interval_length,int) Hashtbl.t;
     hashtbl_for_complexity : (interval_length*interval_length,int) Hashtbl.t;
   };;
   
 exception Unknown_Extsol of depth*interval_length;;  
   
   
 let make l=
   let temp1=Image.image (fun (x,m)->
      let stx=Tidel.safe_set x in
      Translatable_interdiction.make stx m ) l in
   let a1=Hashtbl.create 100
   and a2=Hashtbl.create 100
   and a3=Hashtbl.create 100
   and a4=Hashtbl.create 100
   and a5=Hashtbl.create 100
   and a6=Hashtbl.create 100
   in
   {
     forbidden_subsets =temp1;
     hashtbl_for_extsol =a1;
     hashtbl_for_convolution =a2;
     hashtbl_for_partial_convolution =a3;
     hashtbl_for_levels =a4;
     hashtbl_for_measure =a5;
     hashtbl_for_complexity =a6;
   };;
   
   
 let check x z=Translatable_interdiction.big_check x.forbidden_subsets z;;
 
 let checker x z=
    if check x z
    then Some z
    else None;;
    
    
   
 let max_level_index=15;;  
   
 let level=
    let rec level0=(fun (x:t) n->
    if n>max_level_index
    then failwith("Levels can be computed up to n="^(string_of_int max_level_index))
    else
    if n<1
    then [(Tidel.empty_set:int_set)]
    else 
    if Hashtbl.mem x.hashtbl_for_levels n
    then Hashtbl.find x.hashtbl_for_levels n
    else 
    let temp1=level0 x (n-1) in
    let rem=Translatable_interdiction.remainder x.forbidden_subsets n in
    let tester=Translatable_interdiction.remainder_check rem in
    let temp2=Option.filter_and_unpack(
        fun z->if tester z
        then Some(Tidel.insert n z)
        else None
    ) temp1 in
    let temp3=Ordered_bare_set.safe_set2 temp1
    and temp4=Ordered_bare_set.safe_set2 temp2 in
    let temp5=Ordered_bare_set.teuzin temp3 temp4 in
    let ans=Ordered.forget_order temp5 in
    let _=Hashtbl.add x.hashtbl_for_levels n ans in
    ans) in
    (level0:>(t -> interval_length -> int_set list));;
  
 let measure x n=
    if Hashtbl.mem x.hashtbl_for_measure n
    then Hashtbl.find x.hashtbl_for_measure n
    else 
    let temp1=level x n in
    let temp2=List.rev temp1 in
    let ans=Tidel.length(List.hd temp2) in
    let _=Hashtbl.add x.hashtbl_for_measure n ans in
    ans;; 
  
  
 let extsol=((fun (x:t) (d:depth) n->
    if Hashtbl.mem x.hashtbl_for_extsol (d,n)
    then Hashtbl.find x.hashtbl_for_extsol (d,n)
    else 
    if n>max_level_index
    then raise(Unknown_Extsol(d,n))
    else
   let temp1=level x n in
   let p=(measure x n)-d in
   let ans=List.filter (fun z->Tidel.length z=p) temp1 in
   let _=Hashtbl.add x.hashtbl_for_extsol (d,n) ans in
    ans
   ):>t -> depth -> interval_length -> int_set list);;
   
  let extsol_undefined x n d=
    try (fun _->false) (extsol x d n) with _->true;;
   
   
  let merge_in_convolution x (a:pusher) (uu:int_set list) (vv:int_set list)=
      let better_vv=Image.image (fun z->Tidel.safe_set(Tidel.image (fun i->i+a) z) ) vv in
      let temp1=Cartesian.product uu better_vv in
      let temp2=Option.filter_and_unpack(
         fun (u,v)->checker x (Tidel.teuzin u v) 
      ) temp1 in
      temp2;;
    
  let partial_convolution=((fun (x:t) (d:depth) a b j->
    if Hashtbl.mem x.hashtbl_for_partial_convolution (d,a,b,j)
    then Hashtbl.find x.hashtbl_for_partial_convolution (d,a,b,j)
    else 
    let temp1=extsol x j a
    and temp2=extsol x (d-j) b in
    let temp3=Ordered_bare_set.safe_set2(merge_in_convolution x a temp1 temp2)  in
    let ans=Ordered.forget_order temp3 in
    let _=Hashtbl.add x.hashtbl_for_partial_convolution (d,a,b,j) ans in
    ans));;   
    
   
  let convolution=((fun (x:t) (d:depth) a b->
    if Hashtbl.mem x.hashtbl_for_convolution (d,a,b)
    then Hashtbl.find x.hashtbl_for_convolution (d,a,b)
    else 
    let see_depth=Ennig.ennig 1 d in
    let opt1=Option.find_it(extsol_undefined x a) see_depth in
    if opt1<>None 
    then let d1=Option.unpack(opt1) in
         raise(Unknown_Extsol(d1,a))
    else 
    let opt2=Option.find_it(extsol_undefined x b) see_depth in
    if opt2<>None 
    then let d2=Option.unpack(opt2) in
         raise(Unknown_Extsol(d2,b))
    else 
    let tempf=(fun j->
        Ordered_bare_set.safe_set2(partial_convolution x d a b j) 
    ) in
   let temp1=Ennig.doyle tempf 0 d in
   let temp2=Ordered_bare_set.big_teuzin temp1 in
   let ans=Ordered.forget_order temp2 in
   let _=Hashtbl.add x.hashtbl_for_convolution (d,a,b) ans in
    ans));;
    
    
  let complexity x a b=
    if Hashtbl.mem x.hashtbl_for_complexity (a,b)
    then Hashtbl.find x.hashtbl_for_complexity (a,b)
    else 
    let rec tempf=(fun d->
       if convolution x d a b<>[]
       then d
       else tempf(d+1)
    ) in
    let ans=tempf 0 in
    let temp1=convolution x ans a b in
    let m=Tidel.length(List.hd(temp1)) in
    let _=Hashtbl.add x.hashtbl_for_extsol (0,a+b) temp1 in
    let _=Hashtbl.add x.hashtbl_for_measure (a+b) m in
    let _=Hashtbl.add x.hashtbl_for_complexity (a,b) ans in
    ans;;
    
    

    
  let use_sum x (d:depth) a b=
     let c=complexity x a b in
     let ans=convolution x (c+d) a b in
     let _=Hashtbl.add x.hashtbl_for_extsol (d,a+b) ans in
     c;;
     
 
    
  let upper_measure x n=
    Min.minimize_it_with_care(fun j->
        (measure x j)+(measure x (n-j))    
    ) (Ennig.ennig 1 (n-1));;

 let explicit_merge_in_convolution x (a:pusher) (uu:int_set list) (vv:int_set list)=
      let better_vv=Image.image (fun z->Tidel.safe_set(Tidel.image (fun i->i+a) z) ) vv in
      let tempf=(
         fun (u,v)->checker x (Tidel.teuzin u v) 
      )  in
      Explicit.map_on_cartesian_product tempf uu better_vv;;
      
    
  let explicit_partial_convolution=((fun (x:t) (d:depth) a b j->
    if Hashtbl.mem x.hashtbl_for_partial_convolution (d,a,b,j)
    then Hashtbl.find x.hashtbl_for_partial_convolution (d,a,b,j)
    else 
    let temp1=extsol x j a
    and temp2=extsol x (d-j) b in
    let temp3=Ordered_bare_set.safe_set2(explicit_merge_in_convolution x a temp1 temp2)  in
    let ans=Ordered.forget_order temp3 in
    let _=Hashtbl.add x.hashtbl_for_partial_convolution (d,a,b,j) ans in
    ans));;   
    
   
  