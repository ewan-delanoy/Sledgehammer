

let list_of_pairs l=
if List.length(l)<2 then [] else
let rec sub_f=(function
(accu,ll)->match ll with
[]->List.rev(accu)
|a::b->sub_f(List.rev_append(List.rev(List.rev_map(function t->(a,t))(b)))(accu),b)
) in
sub_f([],l);;


let list_of_triples l=
 if List.length(l)<3 then [] else
 let rec tempf=(function
 (j1,x1,da_ober1,accu)->
  if j1<2
  then List.rev(accu)
  else let temp1=list_of_pairs(da_ober1) in
       let temp2=Image.image(function (x2,x3)->(x1,x2,x3))(temp1) in
       tempf(j1-1,List.hd(da_ober1),List.tl(da_ober1), List.rev_append(temp2)(accu))
  ) in
  tempf(List.length(l)-1,List.hd(l),List.tl(l),[]);;

let list_of_fourtuples l=
 if List.length(l)<4 then [] else
 let rec tempf=(function
 (j1,x1,da_ober1,accu)->
  if j1<3
  then List.rev(accu)
  else let temp1=list_of_triples(da_ober1) in
       let temp2=Image.image(function (x2,x3,x4)->(x1,x2,x3,x4))(temp1) in
       tempf(j1-1,List.hd(da_ober1),List.tl(da_ober1), List.rev_append(temp2)(accu))
  ) in
  tempf(List.length(l)-1,List.hd(l),List.tl(l),[]);;


let list_of_fiftuples l=
 if List.length(l)<5 then [] else
 let rec tempf=(function
 (j1,x1,da_ober1,accu)->
  if j1<4
  then List.rev(accu)
  else let temp1=list_of_fourtuples(da_ober1) in
       let temp2=Image.image(function (x2,x3,x4,x5)->(x1,x2,x3,x4,x5))(temp1) in
       tempf(j1-1,List.hd(da_ober1),List.tl(da_ober1), List.rev_append(temp2)(accu))
  ) in
  tempf(List.length(l)-1,List.hd(l),List.tl(l),[]);;
  
let list_of_sixtuples l=
 if List.length(l)<6 then [] else
 let rec tempf=(function
 (j1,x1,da_ober1,accu)->
  if j1<5
  then List.rev(accu)
  else let temp1=list_of_fiftuples(da_ober1) in
       let temp2=Image.image(function (x2,x3,x4,x5,x6)->(x1,x2,x3,x4,x5,x6))(temp1) in
       tempf(j1-1,List.hd(da_ober1),List.tl(da_ober1), List.rev_append(temp2)(accu))
  ) in
  tempf(List.length(l)-1,List.hd(l),List.tl(l),[]);;
  
let inclusive_list_of_pairs l=
if List.length(l)<2 then [] else
let rec sub_f=(function
(accu,ll)->match ll with
[]->List.rev(accu)
|a::b->sub_f(List.rev_append(List.rev(List.rev_map(function t->(a,t))(a::b)))(accu),b)
) in
sub_f([],l);;
  
  
 let rec partial_power_set k l=
  if k=0 then [[]] else
  if k=1 then Image.image(function x->[x])(l) else
  if List.length(l)<k then [] else
 let rec tempf=(function
 (j,x,da_ober,graet)->
  if j<(k-1)
  then List.rev(graet)
  else let temp1=partial_power_set(k-1)(da_ober) in
       let temp2=Image.image(function y->x::y)(temp1) in
       tempf(j-1,List.hd(da_ober),List.tl(da_ober), List.rev_append(temp2)(graet))
  ) in
  tempf(List.length(l)-1,List.hd(l),List.tl(l),[]);;
  
 let cumulative_partial_power_set l k=
   let temp1=Ennig.doyle(fun j->partial_power_set j l)(0)(k) in
   List.flatten temp1;;
 
  let rec l_naive_combinations k l=
   if k=0              then [[]] else
   if List.length(l)<k then [] else
   let a=List.hd(l) and peurrest=List.tl(l) in
   let first_part_of_answer=Image.image(function x->a::x)(l_naive_combinations (k-1) peurrest)
   and second_part_of_answer=l_naive_combinations k peurrest in
    first_part_of_answer@second_part_of_answer;;
    
  let naive_combinations k l=
   Image.image Tidel.safe_set (l_naive_combinations k (Tidel.forget_order l));;
	
  let combinations k l=
   (*we suppose that l is already ordred with respect to < *)
   let n=Tidel.length(l) in
   if (k<0)||(k>n) then [] else
   if (2*k>n)
   then List.rev_map (Tidel.lemel l) (naive_combinations (n-k) l)	
   else naive_combinations k l;;
   
   let arrangements_in_above_board_case the_k l=
     let rec tempf=(function k->
    if k=0 then [[]] else
    let temp1=tempf(k-1) in
    let temp2=Cartesian.product l temp1 in
    let temp3=List.filter(function (x,y)->not(List.mem x y) )(temp2) in
    let temp4=Image.image (function (x,y)->x::y)(temp3) in
    temp4) in
    tempf the_k;;
    
   
   let arrangements k l=
   let n=Tidel.length(l) in
   if (k<0)||(k>n) then [] else
   arrangements_in_above_board_case k (Tidel.forget_order l);; 
   
   
   
   
   
  
