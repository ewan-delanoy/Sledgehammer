

type period=Period of int;;

type decrement_on_the_right=Decr of int;;

let to_int (Period x)=x;;

let arr_generic_test_for_periodicity arr starting_point end_point (Period t)=
 Ennig.for_all (function j->Array.get(arr)(j)=Array.get(arr)(j+t) ) 
  starting_point end_point;;
 

let arr_test_for_periodicity arr k (Period t)=
arr_generic_test_for_periodicity arr k (Array.length(arr)-t-1) (Period t);;
 
let arr_generic_period_from arr k (Decr d)= 
 let bound=(((Array.length(arr)-d-k)/2)-1) in
  Option.propagate (function x->Period x) (Ennig.find_it
   (function t->arr_generic_test_for_periodicity (arr) k (Array.length(arr)-d-t-1) (Period t)) 
   1 bound) ;;
 
 let arr_period_from arr k=
  arr_generic_period_from arr k (Decr 0);;
  
 let arr_generic_ultimate_period arr (Decr d)=
  let expanded_period_from=(function k->Option.propagate(function per->(k,per))
    (arr_generic_period_from arr k (Decr d) ) )
  in
  Ennig.find_and_stop expanded_period_from 0 (Array.length(arr)/3);; 
  
 
 let arr_ultimate_period arr=
  let expanded_period_from=(function k->Option.propagate(function per->(k,per))
     (arr_period_from arr k) )
  in
  Ennig.find_and_stop expanded_period_from 0 (Array.length(arr)/2);;
  
 let arr_generic_left_periodic_decomposition arr (Decr d)=
   match (arr_generic_ultimate_period arr (Decr d)) with
   None->None
   |Some(j,Period(p))->
     let h=Array.length(arr) in
    Some(Ennig.doyle(Array.get arr)(0)(j-1),
               Ennig.doyle(Array.get arr)(j)(j+p-1),
               Ennig.doyle(Array.get arr)(h-d)(h-1) );;  
  
 let arr_left_periodic_decomposition arr=
   match (arr_ultimate_period arr) with
   None->failwith("Your sequence is *not* ultimately periodic ! ")
   |Some(j,Period(p))->
    (Ennig.doyle(Array.get arr)(0)(j-1),Ennig.doyle(Array.get arr)(j)(j+p-1) );;
  
 let arr_two_sided_periodic_decomposition arr=
  try Option.unpack(Ennig.find_and_stop(function
   d->arr_generic_left_periodic_decomposition arr (Decr d)
  )(0)(Array.length(arr)-2))  with
  _->failwith("Your sequence's center is not even periodic ! ");;
  
  
let arr_period arr=arr_period_from arr 0;;

let list_ultimate_period l=arr_ultimate_period (Array.of_list l);;

let list_period l=arr_period (Array.of_list l);;

let list_left_periodic_decomposition
l=arr_left_periodic_decomposition (Array.of_list l);;

let list_two_sided_periodic_decomposition
l=arr_two_sided_periodic_decomposition (Array.of_list l);;

type modulus=int;;
type transitory_phase=int list;;
type charset=int list;;
type d_charset=int list;;

type set_with_ultimately_periodic_differences=Upd of
  transitory_phase*charset*d_charset*modulus;;

let compute_upd_list (Upd(tr_phase,cs,ds,m)) n=
    let (first_part,i1)=(
      fun ()->
         if tr_phase=[] 
         then ([],1)
         else let m1=List.hd(List.rev tr_phase) in
              if n>=m1
              then (tr_phase,m1+1)
              else (List.filter (fun x->x<=n) tr_phase,n+1)
    )() in
   (first_part)@( List.filter(fun x->List.mem(x mod m) cs)(Ennig.ennig i1 n));;
 
 let short_upd 
   (tr_phase:transitory_phase) 
    (cs:charset) 
     (m:modulus) 
     n=compute_upd_list (Upd(tr_phase,cs,[],m)) n;;
 
 let analyse_upd_list l=
   let dl=Basic.delta_list(l) in
   let (tr_part,per_part)=list_left_periodic_decomposition dl in
   let modulus=Basic.big_sum(per_part) in
   let temp0=Basic.big_head(List.length tr_part)(l)
   and temp1=Basic.big_tail(List.length tr_part)(l) in
   let temp2=List.map (fun x->x mod modulus) temp1 in
   let rec tempf=(fun (a,da_ober)->match da_ober with
     []->failwith("Por el salon")
     |b::peurrest->
        if a>b 
        then da_ober
        else tempf(b,peurrest)
   ) in
   let temp3=tempf(List.hd temp2,List.tl temp2) in
   let temp4=Basic.big_head (List.length per_part) temp3 in
   Upd(temp0,temp4,per_part,modulus);;
   


