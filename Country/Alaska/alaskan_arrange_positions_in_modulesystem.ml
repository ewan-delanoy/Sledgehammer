
(* 

There are two main rules :

1) The ancestors of a file x always come before x.
2) Non-optional files always come before optional files.

We assume that the first file is not optional, if the file system is not empty.

#use"Country/Alaska/alaskan_arrange_positions_in_modulesystem.ml";;


*)


let reserved_item_for_beginning_of_optional_files=None;;

let index_necessarily_before_optional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.find_it(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->Some(List.length(l1),reserved_item_for_beginning_of_optional_files)
   |Some(j1,_)->Some(j1-1,reserved_item_for_beginning_of_optional_files);;

let index_necessarily_after_nonoptional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.find_it(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->None
   |Some(j1,_)->Some(j1,reserved_item_for_beginning_of_optional_files);;

let formalize_other_bounds mdata l=
  let l1=Ennig.index_everything (mdata) in
  let localize=(fun anv->
    match Option.find_it (fun (j,info)->(Modulesystem_data.name info)=anv) l1 with
    None->None
    |Some(j1,_)->Some(j1,Some(anv))
  ) in
  Image.image localize l;;

let lower_bound mdata x l_other_bounds_before=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp1=(fun l->
     if l=[] then None else
     let last_parent_name=List.hd(List.rev l) in
     let j=fst(Option.find_really (fun (j,info)->
     (Modulesystem_data.name info)=last_parent_name) l1) in
     Some(j,Some(last_parent_name))
  )(Modulesystem_data.direct_fathers x) in
  let temp2=(fun bowl->
     if bowl
     then index_necessarily_before_optional_file mdata
     else None
  )(Half_dressed_module.is_optional name_of_x ) in
 let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_before) in
 let temp4=Max.maximize_it_if_possible(fst)(temp3) in
 Option.propagate fst temp4;;
  
  
let upper_bound mdata x l_other_bounds_after=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp0=Option.find_it (fun (j,info)->List.mem name_of_x (Modulesystem_data.all_ancestors info)) l1 in
  let temp1=(function 
     None->None
     |Some(j1,data1)->Some(j1,Some(Modulesystem_data.name data1))
  )(temp0) in
  let temp2=(fun bowl->
     if bowl
     then None
     else index_necessarily_after_nonoptional_file mdata
  )(Half_dressed_module.is_optional name_of_x ) in
   let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_after) in
   let temp4=Min.minimize_it_if_possible(fst)(temp3) in
   Option.propagate fst temp4;;
  
 exception NewNonoptDependingOnOldOpt of Half_dressed_module.t*Half_dressed_module.t;;
 exception OldNonoptDependingOnNewOpt of Half_dressed_module.t*(Half_dressed_module.t option);;
 exception Conflict of Half_dressed_module.t*Half_dressed_module.t;;
  
  
 let treat_insertion_error dt (_,data_down) (_,data_up)=
    if data_down=reserved_item_for_beginning_of_optional_files
    then raise(OldNonoptDependingOnNewOpt(Modulesystem_data.name dt,data_up))
    else
    if data_up=reserved_item_for_beginning_of_optional_files
    then raise(NewNonoptDependingOnOldOpt(Option.unpack data_down,Modulesystem_data.name dt))
    else raise(Conflict(Option.unpack data_down,Option.unpack data_up));;
  
 let insertion_index mdata dt lower_bound upper_bound=
    if upper_bound=None
    then List.length(mdata)
    else let (j_up,data_up)=Option.unpack(upper_bound) in
         if lower_bound=None
         then (* here we use the fact that the first file is not optional, 
              if the file system is not empty *)
              j_up-1
         else let (j_down,data_down)=Option.unpack(lower_bound) in
              if (j_down>j_up)
              then treat_insertion_error dt (j_down,data_down) (j_up,data_up)
              else j_up-1;;

 let insert_data mdata x (l_other_bounds_before,l_other_bounds_after)=
   let lower_bound=lower_bound mdata x l_other_bounds_before
   and upper_bound=upper_bound mdata x l_other_bounds_after
   in
   let i=insertion_index mdata x lower_bound upper_bound
   and l1=Ennig.index_everything (mdata) in
   let (temp1,temp2)=List.partition (fun (j,t)->j<=i) l1 in
   let temp3=Image.image snd temp1
   and temp4=Image.image snd temp2 in
   temp3@(x::temp4);;
   

