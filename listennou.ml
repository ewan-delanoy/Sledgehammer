
(*

#use"listennou.ml";;

*)




let rec uncurrified_rev_append (x,y)=match x with
[]->y
|a::peurrest->uncurrified_rev_append (peurrest,a::y);;

let rec uncurrified_append (x,y)=uncurrified_rev_append (List.rev x,y);;



let didrochan x=
let rec didrochan0=
(function (u,accu1,accu2,bowl)->match u with
 []->(accu1,accu2)
 |a::b->if bowl
        then didrochan0(b,a::accu1,accu2,false)
        else didrochan0(b,accu1,a::accu2,true))  
in
didrochan0(x,[],[],true);;

let find_index x ll=
let rec sub_f=
(function (j,l)->match l with
[]->(-1)      
|u::v->if u=x then j else sub_f(j+1,v)) in
sub_f(1,ll);;



let morzholan f x=
let rec sub_f=(function (u,v)->if u=v then u else sub_f(v,f(v)))
in sub_f(x,f(x));;

let rec morzhol_bihan f k x=
if k=0 then x else morzhol_bihan f (k-1) (f(x));;



let power_set l=
let rec tempf=
(function (da_ober,graet)->match da_ober with
[]->graet
|a::peurrest->tempf(peurrest,graet@(Image.image(function y->a::y)(graet)))
) in
tempf(List.rev(l),[[]]);;

let big_head=Basic.big_head;; 

let big_tail=Basic.big_tail;;


let fold_right f x0 l=List.fold_left(function x->(function a->f a x)) x0 l;;


let assoc l x=
 try Some(List.assoc x l) with
 Not_found->None;; 
 
 let rec r_assoc l x=
   let rec tempf=(function
     []->None
     |(u,v)::peurrest->if v=x
                   then Some(u)
				   else tempf(peurrest)
   ) in
   tempf l;;

let universal_delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((a,b)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;

 
let delete_redundancies r l=
 let rec tempf=(function
   (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->
     if List.exists(function y->r y x)(peurrest)
     then tempf(graet,peurrest)
     else let temp1=List.filter(function y->not(r x y))(peurrest) in
          tempf(x::graet,temp1)
 ) in
 tempf([],l);;

let nonredundant_version l=
  let rec tempf=(
    fun (graet,da_ober)->
      match da_ober with
      []->List.rev graet
      |a::peurrest->if List.mem a graet
                    then tempf(graet,peurrest)
                    else tempf(a::graet,peurrest)
  ) in
  tempf([],l);;


let connected_components_in_intlist l=
  if l=[] then [] else
  let rec tempf=(fun
  (graet,i,j,da_ober)->
     match da_ober with
      []->List.rev((i,j)::graet)
     |a::peurrest->if a=(j+1)
                   then tempf(graet,i,j+1,peurrest)
                   else tempf((i,j)::graet,a,a,peurrest)) in
   tempf([],List.hd l,List.hd l,List.tl l);;

let rev_map f l=
   let rec tempf=(
     fun (graet,da_ober)->match da_ober with
     []->graet
     |a::peurrest->tempf((f a)::graet,peurrest)
   ) in
   tempf([],l);;
   
let constant_slices f l=
  if l=[] then [] else
  let (a0,l0)=(List.hd l,List.tl l) in
  let rec tempf=(fun 
    (graet,current_val,slice,da_ober)->
    match da_ober with
    []->List.rev((List.rev slice)::graet)
    |a::peurrest->
        let va=f(a) in
        if va=current_val
        then tempf(graet,current_val,a::slice,peurrest)
        else tempf((List.rev slice)::graet,va,[a],peurrest) 
  ) in   
  tempf ([],f a0,[a0],l0);;
   

let glued_slices c l=
  if l=[] then [] else
  let rec tempf=(fun 
    (graet,pure_glue,slice,slice_element,da_ober)->
    match da_ober with
    []->( 
          if pure_glue=[]
          then (
                 if slice=[]
                 then List.rev graet
                 else List.rev((List.rev slice)::graet)
               )  
          else List.rev([pure_glue]::graet)
        )  
    |a::peurrest->
        if a=c
        then (
              if pure_glue=[]
              then (
                    if slice_element<>[]
                    then tempf(graet,[c],(List.rev slice_element)::slice,[],peurrest)
                    else tempf(graet,[c],slice,slice_element,peurrest)
                   )
              else if pure_glue=[c]
                   then (
                          if slice=[]
                          then tempf(graet,[c;c],[],[],peurrest)
                          else tempf((List.rev slice)::graet,[c;c],[],[],peurrest)
                        )  
                   else tempf(graet,c::pure_glue,[],[],peurrest)
             )
        else ( 
               if pure_glue=[]
               then tempf(graet,[],slice,a::slice_element,peurrest)
               else (
                     if pure_glue=[c]
                     then (
                           if graet=[]
                           then tempf([[[c]]],[],[],[a],peurrest)
                           else tempf(graet,[],slice,[a],peurrest)
                           )
                     else tempf([pure_glue]::graet,[],[],[a],peurrest)
                    )
             ) 
  ) in   
  tempf ([],[],[],[],l);;

let hi=List.length;;
let rev=List.rev;;

(*
glued_slices 0 [1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;



*)