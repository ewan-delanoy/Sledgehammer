

type 'a variety=Impossible |V of 
('a Tidel.set)*('a Tidel.set)*(('a Nontranslatable_interdiction.t) Ordered.old_set);;

(* V(n1,e1,c) represents the set of all X's such that 
  no element of n1 is in X,
   e1 is included in X, 
  for each (A,m) in c, we have |A\cap X| \leq m.  
  Conditions :
  (i) The sets n1,e1,union(A| (A,m) in c)  must be pairwise disjoint.
  (ii) For each (A,m) in c, we have 0<m<|A|.
  (iii) For any pair (A,m),(AÕ,mÕ) in c, we do not have
    |AÕ\A| \leq mÕ-m. (In that case (AÕ,mÕ) is superfluous).
*)

let unveil=function
Impossible->failwith("The impossible cannot be unveiled")
|V(n1,e1,c)->(n1,e1,c);;


let determinations=function
Impossible->Ordered.unsafe_set []
|V(n1,e1,lc)->lc;;


let e1_determinations=function (V(n1,e1,c))->e1 |Impossible->Tidel.empty_set;;
let n1_determinations=function (V(n1,e1,c))->n1 |Impossible->Tidel.empty_set;;

let add_n1_element x=function
 Impossible->Impossible
 |V(n1,e1,c)->if Tidel.elfenn(x)(e1)
              then Impossible
              else let tempf=(function cond->match Nontranslatable_interdiction.unveil(cond) with
                   (w,m,anv)->if Tidel.elfenn(x)(w)
                              then if m=Tidel.length(w)-1
                                   then None
                                   else Some(
                                   Nontranslatable_interdiction.unsafe_constructor
                                     (Tidel.outsert(x)(w),m,anv))
                              else Some(cond)
              ) in
              let temp1=Option.filter_and_unpack tempf (Ordered.forget_order c) in
              let new_c=Ordered.diforchan 
                 Nontranslatable_interdiction.total_ordering 
                 (Nontranslatable_interdiction.delete_redundancies(temp1)) in
              V(Tidel.insert x n1,e1,new_c);;
              
let add_n1_elements lx v=
 List.fold_left (fun v x->add_n1_element x v) v lx;;

let add_e1_element x=function
 Impossible->Impossible
 |V(n1,e1,c)->if Tidel.elfenn(x)(n1)
              then Impossible
              else let r0=ref(Tidel.empty_set) in
                   let tempf=(function cond->match Nontranslatable_interdiction.unveil(cond) with
                   (w,m,anv)->if Tidel.elfenn(x)(w)
                              then let new_w=Tidel.outsert(x)(w) in
                                   if m=1
                                   then let _=(r0:=Tidel.teuzin(!r0)(new_w)) in 
                                        None
                                   else Some(
                                   Nontranslatable_interdiction.unsafe_constructor
                                   (new_w,m-1,anv))
                              else Some(cond)
              ) in
              let temp1=Option.filter_and_unpack tempf (Ordered.forget_order c) in
              let new_c=
                Ordered.diforchan 
                 Nontranslatable_interdiction.total_ordering
                (Nontranslatable_interdiction.delete_redundancies(temp1)) in
              let new_v=V(n1,Tidel.insert x e1,new_c) in
              add_n1_elements (Tidel.forget_order(!r0)) new_v;;
              
let add_e1_elements lx v=
 List.fold_left (fun v x->add_e1_element x v) v lx;;              

let add_nontranslatable_interdiction c v=match v with
 Impossible->Impossible
 |V(n1,e1,lc)->
   if List.exists(function y->Nontranslatable_interdiction.redundancy y c)
      (Ordered.forget_order lc)
   then v
   else match Nontranslatable_interdiction.incisive_value(c) with
        Some(z)->add_n1_element z v
        |None->
        let corrected_lc=Ordered.filter(function y->not(Nontranslatable_interdiction.redundancy c y))(lc) in
        let new_lc=Ordered.teuzin 
           Nontranslatable_interdiction.total_ordering (Ordered.singleton c) corrected_lc in
        V(n1,e1,new_lc);;
 
let add_nontranslatable_interdictions lx v=
 List.fold_left (fun v x->add_nontranslatable_interdiction x v) v lx;;          

let add_constraint (a,m) v=
  let obs=Nontranslatable_interdiction.make_with_default_name a m in
  add_nontranslatable_interdiction obs v;;

let add_constraints lx v=
 List.fold_left (fun v x->add_constraint x v) v lx;;         

let add_n2_elements lx v=
   let temp1=Image.image (fun t->(t,(Tidel.length t)-1) ) lx in
   add_constraints temp1 v;;

let add_determinations (dn1,de1,dc) v=
  let v1=add_n1_elements dn1 v in
  let v2=add_e1_elements de1 v1 in
  let v3=add_nontranslatable_interdictions dc v2 in
  v3;;


let empty=V(Tidel.empty_set,Tidel.empty_set,Ordered.empty_set);;

let standard_constructor (n1,e1,c)=
  add_determinations (n1,e1,c) empty;;
  
type int_set=int Tidel.set;; 
type int_set_set=int Ordered_bare_set.set2;; 
type int_variety=int variety;;
type ivil=(int_variety, int list list) Memoized.map;;
  
let short_constructor (l:(int_set*int) list)=
 let temp1=Image.image(function (tesk,bonn)->
    let suitable_name=Detect_arithmetic_progressions.pretty_print(Tidel.forget_order tesk) in
    Nontranslatable_interdiction.make tesk bonn suitable_name
 )(l) in
((add_nontranslatable_interdictions temp1  empty):int_variety);;

let nth_char i=List.nth ["z";"a";"b";"c";"d";"e";"f";"g";"h";"i"] i;;
   
let max_dimension=14;;
 
let determined_part var=match var with
  Impossible->Tidel.empty_set
 |V(n1,e1,c)->Tidel.teuzin(n1)(e1);;

let free_part var=match var with
  Impossible->Tidel.empty_set
 |V(n1,e1,c)->
    let temp1=Tidel.image (Nontranslatable_interdiction.unveil) c in
    let temp2=Image.image (fun (x,y,z)->x) temp1 in
    let supp=Tidel.big_teuzin temp2 in
    supp;;    
 
 let dimension var= 
    Tidel.length(free_part var);;
    
 let is_small_enough var=dimension(var)<max_dimension;;
 
 let central_points (var:int_variety)=
   let temp1=Tidel.forget_order(free_part var) 
   and c=determinations(var) in
   let tempf=(fun i->
      let ttemp1=Tidel.filter ( fun t->
        let (a,m,anv)=Nontranslatable_interdiction.unveil t in
        Tidel.elfenn i a)  c in
        (-Tidel.length ttemp1,-i)
   ) in
   let temp2=Image.image tempf temp1 in
   let temp3=Tidel2.diforchan temp2 in
   Tidel2.image ( fun (x,y)->(-y,x) ) temp3;;
   
 let central_point (var:int_variety) =fst(List.hd(central_points var));;  
 
 let subset_that_makes_the_variety_small_enough (var0:int_variety)=
   let rec tempf=(fun
      (graet,var)->
         if is_small_enough var
         then graet
         else let x=central_point var in
              tempf(Tidel.insert x graet,add_e1_element x var)
   ) in
   (tempf(Tidel.empty_set,var0):int_set);;
   
 let servat_subsets=((Memoized.make(fun var0->
   let rec tempf=(fun
      (graet,var)->
         let x=subset_that_makes_the_variety_small_enough(var) in
         if Tidel.length(x)=0 
         then List.rev graet
         else tempf((Tidel.forget_order x)::graet,add_n2_elements [x] var)
   ) in
   tempf([],var0))):ivil);;  
 
 let find_all_maximal_realizers_naively (v:int_variety) initial_l=
  let ordered_l=Tidel.safe_set(initial_l) in
  if (Tidel.length
     (Tidel.kengeij ordered_l (free_part v))>max_dimension)
  then failwith("Dimension is too large to find solutions easily. Call in Servat. ")
  else
  let forgetful_l=Tidel.forget_order(ordered_l) in
  let rec tempf=(fun
    (u,l)->match l with
       []->let e1=e1_determinations u in
           (Tidel.length(e1),[e1])
       |a::peurrest->
          if Tidel.elfenn a (determined_part u)
          then tempf(u,peurrest)
          else let u1=add_n1_element a u
               and u2=add_e1_element a u in
               let (n1,s1)=tempf(u1,peurrest) and (n2,s2)=tempf(u2,peurrest) in
               if n1=n2 then (n1,s2@s1) else
               if n1<n2 then (n2,s2) else (n1,s1)
    ) in
    match v with
    Impossible->failwith("No maximal realizers")
    |V(n1,e1,c)->
      let corrector=Tidel.kengeij ordered_l in
      let corrected_c=Tidel.filter (fun t->
        let (a,m,anv)=Nontranslatable_interdiction.unveil t in
        Tidel.length(corrector a)>m
      ) c in
      let corrected_u=V(corrector n1,corrector e1,corrected_c ) in
     (Ordered_bare_set.safe_set2(snd(tempf(corrected_u,forgetful_l))):int_set_set);;    
                 
 let find_all_maximal_realizers_using_servat_subsets var0 l=
   let btemp1=servat_subsets(var0) in
   let btemp2=Image.image Tidel.safe_set btemp1 in
   let btemp3=Three_parts.generic(btemp2) in
   let btemp4=Image.image(fun (x1,x2,x3)->
      let tvar1=add_n2_elements x1 var0 in
      add_e1_elements (Tidel.forget_order x2) tvar1
   ) btemp3 in
   let final_var=(add_n2_elements btemp2 var0) in
   let temp1=final_var::btemp4 in
   let temp2=Image.image(fun var->find_all_maximal_realizers_naively var l)(temp1) in
   let (big_m,solutions)=Max.maximize_it_with_care(function 
      Ordered.S(x)->Tidel.length(List.hd x)
   )(temp2) in
   ((Ordered_bare_set.big_teuzin solutions):int_set_set);;                 
                 
 let find_all_maximal_realizers var0 l=
  let ordered_l=Ordered_integer.safe_set(l) in
  if (Tidel.length(Tidel.kengeij(free_part var0) ordered_l)>max_dimension)
  then find_all_maximal_realizers_using_servat_subsets var0 l
  else find_all_maximal_realizers_naively var0 l;;                
                 
let cumulate_intervals_for_vdw n=
 let temp0=Ordered_bare_set.image(function z->
    let suitable_name="a_"^(Pretty_print_intervals_and_felines.interval(Tidel.forget_order z)) in
    Nontranslatable_interdiction.make z 2 suitable_name
 )(Van_der_waerden_basics.global n 0)
 and temp1=Van_der_waerden_basics.irreducible_intervals(n) in
 let temp2=Image.image(function (i,j)->
   let m=Van_der_waerden_basics.mini_h(j-i+1) in
   let itv=Ennig.ennig(i)(j) in
   let suitable_name="a_"^(Pretty_print_intervals_and_felines.interval(itv)) in
   Nontranslatable_interdiction.make(Tidel.safe_set itv)(m)(suitable_name)
 )(temp1) in
 let temp3=Van_der_waerden_basics.irreducible_feline_intervals(n) in
 let temp4=Image.image(function (i,r,l)->
   let m=Van_der_waerden_basics.mini_h(l) 
   and itv=Ennig.doyle(function q->i+q*r)(0)(l-1) in
   let suitable_name=(nth_char r)^"_"^(Pretty_print_intervals_and_felines.interval(Ennig.ennig(i)(i+(l-1)*r))) in
   Nontranslatable_interdiction.make(Tidel.safe_set itv)(m)(suitable_name)
 )(temp3) in
 ((add_nontranslatable_interdictions (temp0@temp2@temp4)
empty):int_variety);;

let cumulate_intervals_and_add_constraints_for_vdw n l=
  let additional_constraints=
     Image.image
       (fun (x,m)->Nontranslatable_interdiction.make_with_telltale_name x m)(l) in
     add_nontranslatable_interdictions additional_constraints (cumulate_intervals_for_vdw n);;

let cumulate_intervals_for_wdw n=
 let temp0=Ordered_bare_set.image(function z->
    let suitable_name="a_"^(Pretty_print_intervals_and_felines.interval(Tidel.forget_order z)) in
    Nontranslatable_interdiction.make z 2 suitable_name
 )(Van_der_waerden_basics.global n 1)
 and temp1=Van_der_waerden_basics.irreducible_intervals(n) in
 let temp2=Image.image(function (i,j)->
   let m=Van_der_waerden_basics.mini_h(j-i+1) in
   let itv=Ennig.ennig(i)(j) in
   let suitable_name="a_"^(Pretty_print_intervals_and_felines.interval(itv)) in
   Nontranslatable_interdiction.make(Tidel.safe_set itv)(m)(suitable_name)
 )(temp1) in
 ((add_nontranslatable_interdictions (temp0@temp2)
empty):int_variety);;

let wdw (n,dn1,de1,other)=
  let var1=cumulate_intervals_for_vdw n in
  let temp1=Image.image (fun (a,m)->Nontranslatable_interdiction.make_with_telltale_name a m) other in
  add_determinations (dn1,de1,temp1) var1;;




 
 (*
 
#use "theodoracopulos.ml";;
    

              
              
                            *)