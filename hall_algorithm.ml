
type ('a,'b) t=
{ 
  choices_made:('a*'b) list; 
  raw_matter:('a*('b Tidel.set)) list
};;


let add_chooser_quickly eq x (i0,x0)=
  let tempf=(function (i,set_for_i)->
     if eq(i)(i0) 
     then None
     else Some(i,Tidel.outsert x0 set_for_i)
  ) in
  let new_l2=Option.filter_and_unpack tempf x.raw_matter in
  {choices_made=(i0,x0)::x.choices_made;raw_matter=new_l2};;
  
let add_several_choosers_quickly eq x li0x0=
  List.fold_left (add_chooser_quickly eq) x li0x0;; 

let urgent_problems x=
  List.filter(function (j,box)->Tidel.length(box)<2)(x.raw_matter);;

let rec automatic_update eq x=
 let temp1=urgent_problems(x) in
 if temp1=[] then x else
 if List.exists(function (j,box)->box=Tidel.empty_set)(temp1)
 then failwith("Problem in the Hall algorithm")
 else let temp2=List.rev_map(function (j,box)->(j,Tidel.hd box) )(temp1) in
      let inter=add_several_choosers_quickly eq x temp2 in
      automatic_update eq inter;;      

let constructor eq x=
automatic_update eq ({ choices_made=[];raw_matter=x});;


let add_chooser eq (i0,x0) x=
  automatic_update eq (add_chooser_quickly eq x (i0,x0));;

let iterator eq (l,x)=
  (*we assume that x is updated and that x.raw_matter is not empty *)
   let first_problem=List.hd(x.raw_matter) in
   let arbitrary_element=Tidel.hd(snd first_problem) in
   let arbitrary_choice=(fst first_problem,arbitrary_element) in
   let record=(first_problem,arbitrary_element) in
   (record::l,add_chooser eq arbitrary_choice x);;
       
let solve_explicitly eq x=    
  let rec tempf=(function (l,x)->
    if x.raw_matter=[] then (l,x.choices_made) else
     tempf(iterator eq (l,x))
  ) in
  tempf([],x);;

let solve eq x=snd(solve_explicitly eq x);;



