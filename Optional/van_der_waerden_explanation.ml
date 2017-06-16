(*******************************************************************************)

(* 
  

   This module formalizes what can be computed on Van der Werden varieties without
   breaking into cases.
   
   
#use "van_der_waerden_explanation.ml";;   

*)

(*******************************************************************************)


 type triple=Tr of int*int*int;;   
 type triple_starting_point=int;;
 type triple_step=int;;
 
 type list_of_elements_outside = Binary_constraint.list_of_elements_outside;;
 type list_of_elements_inside = Binary_constraint.list_of_elements_inside;;
 
 type explanation=
      Isolated_point
     |End_segment of triple
     |Light_triangle of triple
     |Heavy_triangle of triple*triple*triple
     |Rhomb of (int*int*int*int)*(triple list)
     |Pentagon of (int*int*int*int*int)*(triple list)
     |Saw of (int*int*int*int*int)*(triple list)
     |Orange of (int*int*int*int*int)*(triple list)
     |Bike of (int*int*int*int*int*int)*(triple list)
     |Long of int * int Tidel.set * int * int * int Tidel.set * int;;

 let bound_from_explanation=function
      Isolated_point->1
     |End_segment(_)->1
     |Light_triangle(_)->2
     |Heavy_triangle(_,_,_)->1
     |Rhomb(_,_)->2
     |Pentagon(_,_)->2
     |Saw(_,_)->2
     |Orange(_,_)->2
     |Bike(_,_)->2
     |Long(ans,wj0,j0,hj0,v0,lv0)->ans;;

let symmetrized_triple n (Tr(x,y,z))=Tr(n+1-z,n+1-y,n+1-x);;

type int_set=int Tidel.set;;

let symmetrized_set n (z:int_set)=
   (Tidel.safe_set(List.rev(Tidel.image (fun t->n+1-t) z)):int_set);;

let symmetrized_explanation n x=
     let sim=(fun t->n+1-t) in
     let sym=symmetrized_triple n in
     match x with
      Isolated_point->Isolated_point
     |End_segment(t)->End_segment(symmetrized_triple n t)
     |Light_triangle(t)->Light_triangle(symmetrized_triple n t)
     |Heavy_triangle(t1,t2,t3)->Heavy_triangle(sym t1,sym t2,sym t3)
     |Rhomb((i1,i2,i3,i4),l)-> Rhomb((sim i1,sim i2,sim i3,sim i4),Image.image sym l)
     |Pentagon((i1,i2,i3,i4,i5),l)-> Pentagon((sim i1,sim i2,sim i3,sim i4,sim i5),Image.image sym l) 
     |Saw((i1,i2,i3,i4,i5),l)->Saw((sim i1,sim i2,sim i3,sim i4,sim i5),Image.image sym l)
     |Orange((i1,i2,i3,i4,i5),l)->Orange((sim i1,sim i2,sim i3,sim i4,sim i5),Image.image sym l)
     |Bike((i1,i2,i3,i4,i5,i6),l)->Bike((sim i1,sim i2,sim i3,sim i4,sim i5,sim i6),Image.image sym l)
     |Long(ans,wj0,j0,hj0,v0,lv0)->
      let sym=symmetrized_set n in
      Long(ans,sym wj0,j0,hj0,sym v0,lv0);;
   
 
 let construct_triple (x:triple_starting_point) (r:triple_step)=
    Tr(x,x+r,x+2*r);;
      
 let triple_from_elements a b c=
   let temp1=Tidel.safe_set [a;b;c] in
   let temp2=Ordered.forget_order temp1 in
   let a1=List.nth(temp2)(0)
   and a2=List.nth(temp2)(1) in
   construct_triple a1 (a2-a1);;
  
 let careful_triple_from_elements a b c=
   let temp1=Tidel.safe_set [a;b;c] in
   let temp2=Ordered.forget_order temp1 in
   if List.length(temp2)<>3 then None else
   let a1=List.nth(temp2)(0)
   and a2=List.nth(temp2)(1) 
   and a3=List.nth(temp2)(2) in
   if (a3-a2)=(a2-a1)
   then Some(construct_triple a1 (a2-a1))
   else None;;
      
 let reconstruct_triple (e1:list_of_elements_inside) i1 i2=
   let temp1=Tidel.kengeij(Tidel.safe_set e1)(Tidel.safe_set (Van_der_waerden_basics.completers(i1,i2))) in
   if Tidel.length(temp1)=0
   then None
   else let i3=Tidel.hd temp1 in
        Some(triple_from_elements i1 i2 i3);;
      
 type link=int;; 
  
 let largest_link (z:int_set)=
   let lz=Ordered.forget_order z in
   let temp1=Basic.delta_list lz in
   ((Gcd.gcd_for_many temp1):>link);;
      
 let embeddings_for_one_link (r:link) n z=
   let lz=Ordered.forget_order z in
   let a=List.hd(lz) and b=List.hd(List.rev lz) in
   let l=((b-a)/r)+1 in
   let iz=Ennig.doyle(fun t->a+(t-1)*r) 1 l in
   let left_bound=(a-1)/r
   and right_bound=(n-b)/r in
   let temp1=Ennig.doyle (fun j->
     let ttemp1=Ennig.doyle (fun t->a-(j+1-t)*r ) 1 j in
     (j,ttemp1) 
   ) (0) (left_bound) 
   and temp2=Ennig.doyle (fun j->
     let ttemp2=Ennig.doyle (fun t->b+t*r ) 1 j in
     (j,ttemp2) 
   ) (0) (right_bound) in
   let temp3=Cartesian.product temp1 temp2 in
   Option.filter_and_unpack (fun ((j1,l1),(j2,l2))->
     let m=j1+l+j2 in
     if m>=n then None else
     Some(m,Tidel.safe_set(l1@iz@l2))
   ) temp3;;
      
 let embeddings_for_several_links (r:link) n z=
   let temp1=List.filter (fun t->r mod t=0) (Ennig.ennig 1 r) in
   let temp2=Image.image (fun t->   embeddings_for_one_link t n z) temp1 in
   List.flatten temp2;;     
   
 let all_embeddings n z=
   embeddings_for_several_links (largest_link z) n z;;
      
 let rhomb_check rf (i1,i2,i3,i4)=
    let temp1=Image.image (fun 
       (x,y,z)->careful_triple_from_elements x y z) [(i1,i2,i3);(i2,i3,i4)] in
    if List.exists (fun y->y=None) temp1
    then None
    else 
    let temp3=rf i1 i4 in
    if temp3=None 
    then None 
    else
    let temp2=Image.image Option.unpack temp1 in
    Some(Rhomb((i1,i2,i3,i4),temp2@[Option.unpack temp3] ));;     
      
 let pentagon_check rf (i1,i2,i3,i4,i5)=
    let l=[i1;i2;i3;i4;i5;i1] in
    let tf=(fun k->List.nth l (k-1)) in
    let temp1=Ennig.doyle (fun j->rf(tf j)(tf (j+1)) ) 1 5 in
    if List.exists (fun y->y=None) temp1
    then None
    else let temp2=Image.image Option.unpack temp1 in
          Some(Pentagon((i1,i2,i3,i4,i5),temp2 ));;
 
 let saw_check rf (i1,i2,i3,i4,i5)=
    let temp1=Image.image (fun (x,y)->rf x y) [(i5,i3);(i3,i1);(i1,i4);(i4,i2)] in
    if List.exists (fun y->y=None) temp1
    then None
    else 
    let temp3=careful_triple_from_elements i1 i2 i5 in
    if temp3=None 
    then None 
    else
    let temp2=Image.image Option.unpack temp1 in
    Some(Saw((i1,i2,i3,i4,i5),temp2 ));;
          
 let orange_check rf (i1,i2,i3,i4,i5)=
    let temp1=Image.image (fun (x,y)->rf x y) [(i1,i2);(i2,i3);(i3,i5);(i5,i1);(i2,i4)] in
    if List.exists (fun y->y=None) temp1
    then None
    else 
    let temp3=careful_triple_from_elements i1 i3 i4 in
    if temp3=None 
    then None 
    else
    let temp2=Image.image Option.unpack temp1 in
    Some(Orange((i1,i2,i3,i4,i5),temp2 ));;         
          
 let bike_check rf (i1,i2,i3,i4,i5,i6)=
    let temp1=Image.image (fun (x,y)->rf x y) 
       [(i1,i2);(i2,i3);(i3,i5);(i3,i6);(i4,i5);(i4,i6);(i5,i6)] in
    if List.exists (fun y->y=None) temp1
    then None
    else 
    let temp3=careful_triple_from_elements i1 i3 i4 in
    if temp3=None 
    then None 
    else
    let temp2=Image.image Option.unpack temp1 in
    Some(Bike((i1,i2,i3,i4,i5,i6),temp2 ));;         
          
      
 let analysis_in_length_two complete_rbc i1 i2=
   let (_,e1)=Binary_constraint.unveil complete_rbc in
   let l_e1=Ordered.forget_order e1 in
   match reconstruct_triple  l_e1 i1 i2 with
   None-> failwith("Error E1 : Lonely doubleton")
   |Some(t)->End_segment (t);;

let analysis_in_easy_case_in_length_three i1 i2 i3=
   let temp1=Tidel.safe_set [i1;i2;i3] in
   let temp2=Ordered.forget_order temp1 in
   let tempf=(fun j->List.nth temp2 (j-1)) in
   let a1=tempf 1 and a2=tempf 2 and a3=tempf 3 in
   if (a3-a2=a2-a1)
   then Light_triangle(construct_triple a1 (a2-a1))
   else failwith("Error E2 : Lonely tripleton");;
   
let analysis_in_length_three complete_rbc i1 i2 i3=
   let (_,e1)=Binary_constraint.unveil complete_rbc in
   let l_e1=Ordered.forget_order e1 in
   let temp1=reconstruct_triple  l_e1 i1 i2 in
   if temp1=None then analysis_in_easy_case_in_length_three i1 i2 i3 else
   let temp2=reconstruct_triple  l_e1 i1 i3 in
   if temp2=None then analysis_in_easy_case_in_length_three i1 i2 i3 else
   let temp3=reconstruct_triple  l_e1 i2 i3 in
   if temp3=None then analysis_in_easy_case_in_length_three i1 i2 i3 else
   Heavy_triangle
   (Option.unpack temp1,Option.unpack temp2,Option.unpack temp3);;
   
 let analysis_in_big_length (e1:list_of_elements_inside) n (z:int_set)=
   let effective_e1=Tidel.lemel(Tidel.safe_set e1)(z) in
   let tempf=(fun (j,wj)->
     Van_der_waerden_basics.mini_h(j)-Tidel.length(Tidel.kengeij effective_e1 wj)
   ) in
   let (ans,sols)=Min.minimize_it_with_care tempf (all_embeddings n z) in
   let (j0,wj0)=List.hd(sols) in
   let v0=Tidel.kengeij effective_e1 wj0 in
   Long(ans,wj0,j0,Van_der_waerden_basics.mini_h(j0),v0,Tidel.length v0);;  
 
  let analysis_in_length_four (e1:list_of_elements_inside) n i1 i2 i3 i4=
   let temp1=analysis_in_big_length e1 n (Tidel.safe_set [i1;i2;i3;i4]) in
   if bound_from_explanation(temp1)<=2 then temp1 else
   let rf=reconstruct_triple e1 in
   let temp2=rhomb_check rf (i1,i2,i3,i4) in
   if temp2<>None then Option.unpack temp2 else
   temp1;;
 
 let analysis_in_length_five (e1:list_of_elements_inside) n i1 i2 i3 i4 i5=
   let temp1=analysis_in_big_length e1 n (Tidel.safe_set [i1;i2;i3;i4;i5]) in
   if bound_from_explanation(temp1)<=2 then temp1 else
   let rf=reconstruct_triple e1 in
   let temp2=pentagon_check rf (i1,i2,i3,i4,i5) in
   if temp2<>None then Option.unpack temp2 else
   let temp3=saw_check rf (i1,i2,i3,i4,i5) in
   if temp3<>None then Option.unpack temp3 else
   let temp4=orange_check rf (i1,i2,i3,i4,i5) in
   if temp4<>None then Option.unpack temp4 else
   temp1;;
   
 let analysis_in_length_six (e1:list_of_elements_inside) n i1 i2 i3 i4 i5 i6=
   let temp1=analysis_in_big_length e1 n (Tidel.safe_set [i1;i2;i3;i4;i5;i6]) in
   if bound_from_explanation(temp1)<=2 then temp1 else
   let rf=reconstruct_triple e1 in
   let temp2=bike_check rf (i1,i2,i3,i4,i5,i6) in
   if temp2<>None then Option.unpack temp2 else
   temp1;;  
   
 type tsp_set=triple_starting_point Tidel.set;;  
   
 let find_explanation_for_cleaned_set n rbc (z:tsp_set)=
       (* we assume that no element of z is automatically determined *)
      let m=Tidel.length(z) and lz=Ordered.forget_order z in
      let enz=(fun j->List.nth lz (j-1)) in
      let (_,e1)=Binary_constraint.unveil rbc in
      let l_e1=Ordered.forget_order e1 in
      if m<1 then failwith("Error E3 : Some set is too small") else
      if m=1 then Isolated_point else
      if m=2 then analysis_in_length_two rbc (enz 1) (enz 2) else
      if m=3 then analysis_in_length_three rbc (enz 1) (enz 2) (enz 3) else
      if m=4 then analysis_in_length_four l_e1 n (enz 1) (enz 2) (enz 3) (enz 4) else
      if m=5 then analysis_in_length_five l_e1 n (enz 1) (enz 2) (enz 3) (enz 4) (enz 5) else
      if m=6 then analysis_in_length_six l_e1 n (enz 1) (enz 2) (enz 3) (enz 4) (enz 5) (enz 6) else
      analysis_in_big_length l_e1 n z;;
      
  let imperfect_measure_for_cleaned_set n rbc z=
      (* we assume that no element of z is automatically determined *)
      bound_from_explanation(find_explanation_for_cleaned_set n rbc z);;
      
   let imperfect_measure n incomplete_rbc (old_z:tsp_set)=
      let rbc=Van_der_waerden_basics.completed_binary_constraint n incomplete_rbc in
      let (n1,e1)=Binary_constraint.unveil rbc in
      let fixed_part=Tidel.length(Tidel.kengeij e1 old_z)
      and z=Tidel.lemel(old_z)(Tidel.teuzin n1 e1) in
      fixed_part+(imperfect_measure_for_cleaned_set n rbc z);; 
 
 
