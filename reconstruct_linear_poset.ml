(*

#use"reconstruct_linear_poset.ml";;

Computes the (canonical) maximal acyclic sub-poset of a given poset, returns
it as a list L where each element of L is a triple (a,anc_a,a_is_clean)
where anc_a is the list of all ancestors of a, ordered as in L, and a_is_clean
is a boolean indicating if a is the ancestor or a descendant of an "active"
element.

The input is a list of vertex*bool elements,  where the boolean indicates
if the vertex is "active". If you don't need that functionality, you can set
those booleans to any arbitrary value.


Also returns a (non-canonical,non-exhaustive) set of cycles.


*)


let iterator coat 
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt1)=
    (* 
    between is a "chained" list of pairs (x1,x2),(x2,x3), ...
    (stocked in reverse actually)
    that will possibly lead to a cycle
    *)
    if opt1<>None then ([],Tidel.empty_set,[],Tidel.empty_set,[],[],opt1) else
    if (between,not_yet_checked)=([],[]) 
    then ([],Tidel.empty_set,[],Tidel.empty_set,[],[],
          Some(cycles,Listennou.rev_map (fun (z,p)->(z,fst p)) checked)) 
    else
    let a=
    	  (if between=[] 
    	   then List.hd(not_yet_checked)
           else snd(List.hd(between))
          ) in
    let not_yet_checked2=List.filter (fun z->z<>a) not_yet_checked in
    let coat_a=coat(a) in
    let coatoms_of_a=Tidel.safe_set(coat_a) in
    let temp1=Tidel.lemel coatoms_of_a checked_union in
    if Tidel.length(temp1)=0
    then let temp3=coatoms_of_a::(Image.image (fun z->snd(List.assoc z checked)) 
                      (coat_a)) in
         let ordered_set_version=Tidel.big_teuzin(temp3) in
         let temp4=Option.filter_and_unpack (
           fun (b,_)->if Tidel.elfenn b ordered_set_version
             then Some(b)
             else None
         ) checked in
         let list_version=List.rev(temp4) in
         let data_for_a=(list_version,ordered_set_version) in
         ((a,data_for_a)::checked,Tidel.insert a checked_union,
         cycles,cycles_union,[],not_yet_checked2,None)
    else
    if Tidel.elfenn a temp1
    then ([],Tidel.empty_set,[a]::cycles,Tidel.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    if (not(Tidel.kengeij_goullo temp1 cycles_union))
    then (checked,checked_union,cycles,Tidel.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    (*see if we can close the cycle *)
    match Option.find_it(fun (x,y)->Tidel.elfenn x temp1) between with
     None->(checked,checked_union,cycles,cycles_union,
     		(a,Tidel.hd temp1)::between,not_yet_checked,None)
    |Some(p)->
        let (before,_,after)=Three_parts.select_center_element_and_reverse_left (fun x->x=p) between in
        let temp2=Image.image fst before in
        let new_cycle=(fst p)::(temp2@[a]) in
        let ordered_cycle=Tidel.diforchan new_cycle in
        let not_yet_checked3=List.filter (fun z->Tidel.nelfenn z ordered_cycle) not_yet_checked in
        (checked,checked_union,new_cycle::cycles,
        Tidel.teuzin ordered_cycle cycles_union,
        [],not_yet_checked3,None);;

let reconstruct_linear_poset coat l=
  let rec tempf=(fun
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt)->
    if opt<>None
    then Option.unpack opt
    else tempf(iterator coat 
    (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt))
    ) in
    tempf([],Tidel.empty_set,[],Tidel.empty_set,[],l,None);;
    
(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
reconstruct_linear_poset sugar (ennig 1 30);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

reconstruct_linear_poset brown (ennig 1 21);;  


*)
 
    
    
    
    