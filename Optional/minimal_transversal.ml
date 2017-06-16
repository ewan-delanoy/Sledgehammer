(* Given a collection F of sets, T is a transversal for F
when T intersects every set in F. Here we compute minimal
transversals. *)


let rec check_for_transversal tt=function
[]->true
|a::rest_of_f->
if Tidel.kengeij_goullo(a)(tt)
then false
else check_for_transversal( tt)(rest_of_f);;

(*in the next function we assume that transversality has already been checked. *)

let rec check_for_minimal_transversal ff ( tt)=
if Tidel.length(tt)=1 then true else
let temp1=List.rev_map(Tidel.kengeij(tt))(ff) in
let temp2=List.filter(function u->Tidel.length(u)=1)(temp1) in
let temp3=Tidel.diforchan(List.rev_map(Tidel.hd)(temp2)) in
temp3=tt;;

let frail_minimal_transversals ff=
 let lff=Ordered_bare_set.forget_order(ff) in
let rec sub_f=(function
  (da_ober,graet)->match da_ober with
  []->graet
  |x::peurrest->
   let temp1=List.partition(Tidel.kengeij_goullo(x))(graet) in
   let temp2=fst(temp1) and temp3=snd(temp1) in
   let temp4=Cartesian.product(temp2)(Tidel.forget_order x) in
   let temp5=List.rev_map(function (u,v)->Tidel.teuzin(Tidel.singleton(v))(u))(temp4) in
   let temp6=List.filter(check_for_minimal_transversal(lff))(temp5) in
   sub_f(peurrest,List.rev_append(temp6)(temp3)) 
) in
Ordered_bare_set.diforchan(sub_f(lff,[Tidel.empty_set]));;

let pair_of_empty_sets=(Tidel.empty_set,Ordered_bare_set.singleton(Tidel.empty_set));;

let join_with_empty_set x=(x,Ordered_bare_set.singleton(Tidel.empty_set));;

let minimal_transversals ff=
 if (ff=[]) then pair_of_empty_sets else
 let temp1=List.filter(function x->Tidel.length(x)<2)(ff) in
 if List.mem(Tidel.empty_set)(temp1) then pair_of_empty_sets else
 if temp1=[] then (Tidel.empty_set,frail_minimal_transversals(Ordered_bare_set.diforchan(ff))) else
 let temp2=List.rev_map(Tidel.hd)(temp1) in
 let constants=Tidel.diforchan(temp2) in
 let corrected_ff=List.filter(Tidel.kengeij_goullo constants )(ff) in
 if corrected_ff=[] then join_with_empty_set(constants) else
 let temp3=frail_minimal_transversals(Ordered_bare_set.diforchan(corrected_ff)) in
(constants,temp3);; 


let quick_transversal ff=
  if (ff=[]) then Tidel.empty_set else
 if List.mem(Tidel.empty_set)(ff) then Tidel.empty_set else
 let rec tempf=(fun
  (graet,gorre_a_vreman,vv)->
    if vv=[] then Tidel.safe_set(List.rev(graet)) else
    let (temp1,temp2)=List.partition(function x->Tidel.length(x)<2)(vv) in
    let chosen_one=(fun ()->
       if temp1<>[] then Tidel.hd(List.hd temp1) else
       let lgorre=Tidel.forget_order(gorre_a_vreman) in
       let (temp4,temp3)=Max.maximize_it_with_care
         (fun x0->List.length(List.filter (Tidel.elfenn x0) vv) )(lgorre) in
         List.hd temp3
    )() in
    let gorre_nevez=Tidel.outsert(chosen_one)(gorre_a_vreman) in
    let vv_nevez=List.filter(Tidel.nelfenn chosen_one) vv in
    tempf(chosen_one::graet,gorre_nevez,vv_nevez)
 ) in
 tempf([],Tidel.big_teuzin ff,ff);;
 
 



