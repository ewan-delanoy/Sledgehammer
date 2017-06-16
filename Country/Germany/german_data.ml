
(* 

#use"Country/Germany/german_data.ml";;

*)



let filedata_selector mdata ending x=List.filter 
   (Modulesystem_data.check_presence ending) mdata;;
 
  
let inside_files mdata=Image.image Mlx_ended_absolute_path.to_path 
(Alaskan_data.all_mlx_files mdata);;  
  
let local_directories mdata=
  let temp1=Alaskan_data.all_mlx_files mdata in
  let temp5=Image.image Mlx_ended_absolute_path.to_string temp1 in
  let temp2=Image.image (fun s->Father_and_son.father s '/') temp5 in
  let temp3=Ordered_string.diforchan temp2 in
  let temp4=Ordered.forget_order temp3 in
  temp4;;
   
 let see_if_file_is_registered mdata mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx
    and edg=Mlx_ended_absolute_path.ending mlx in  
    match Option.find_it (fun a->Modulesystem_data.name a=hm) mdata with
    None->false
    |Some(dt)->Modulesystem_data.check_presence edg dt;;
 
let check_presences mdata hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) mdata with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 let acolytes mdata hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) 
      mdata with
     None->[]
    |Some(dt)->Modulesystem_data.acolytes dt;;    
   
 
 let outdated_files mdata=
   let temp1=Image.image Modulesystem_data.outdated_acolytes mdata in
   List.flatten temp1;;
 

   
 let descendants mdata names=
    let temp1=List.filter(
      fun dt->List.exists (fun t->List.mem t names) 
        (Modulesystem_data.all_ancestors dt)
    ) mdata in
    temp1;;
    
let optionality_partition mdata=
  let (before,core,after)=Three_parts.select_center_element 
    Modulesystem_data.is_optional mdata in
  (before,Option.add_perhaps core after);;    

exception Deletability_issue of Mlx_ended_absolute_path.t;;

let is_deletable mdata mlxfile=
   let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
   if (descendants mdata [hm])<>[]
   then false
   else let edg=Mlx_ended_absolute_path.ending mlxfile in
        if List.mem edg [Ocaml_ending.ml;Ocaml_ending.mli]
        then true
        else 
        if List.mem edg [Ocaml_ending.mll;Ocaml_ending.mly]
        then let opt=Option.find_it (fun a->Modulesystem_data.name a=hm) mdata in
             (
               match opt with
               None->true
               |Some(dt)->not(Modulesystem_data.ml_present dt)
             )
        else raise(Deletability_issue(mlxfile));; 
 
let unregistered_mlx_files mdata=
   let temp1=Mlx_ended_absolute_path.complete_ls German_constant.root in
   List.filter (fun mlx->
     not(see_if_file_is_registered mdata mlx)
   ) temp1;;
 
let system_size mdata=List.length(mdata);;
 
exception  Non_registered_module of Half_dressed_module.t;;
 
let above mdata hm=
   match Option.find_it(fun dt->Modulesystem_data.name dt=hm) mdata with
    None->raise(Non_registered_module(hm))
   |Some(dt)->Modulesystem_data.all_ancestors dt;;
   
let below mdata hm=
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.all_ancestors dt)
   then Some(Modulesystem_data.name dt)
   else None) mdata;;   
 
let directly_below mdata hm=
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.direct_fathers dt)
   then Some(Modulesystem_data.name dt)
   else None) mdata;;    


let files_containing_string mdata some_string=
   let temp1=Alaskan_data.all_mlx_paths mdata in
   List.filter (fun ap->Substring.is_a_substring_of 
     some_string (Io.read_whole_file ap)) temp1;;
 

let default_targets mdata=
  let temp1=Image.image Ocaml_target.from_modulesystem_data mdata 
  and temp2=Image.image Modulesystem_data.name mdata in
  let temp3=List.flatten temp1 
  and mtn=German_constant.main_toplevel_name in
  temp3@[Ocaml_target.toplevel mtn temp2]
  ;;


let default_toplevel =Alaskan_data.default_toplevel 
   German_constant.main_toplevel_name;; 
 
let deletable_files mdata=
  let temp2=Image.image Modulesystem_data.name mdata in
  let is_interesting=(fun hm->List.mem 
     (Half_dressed_module.to_string hm) 
     Interesting_modules.list
  ) in
  let temp3=List.filter (
    fun hm->
    (not(is_interesting hm))
    &&
    (List.for_all(fun hm2->not(is_interesting hm2)) (below mdata  hm))
  ) temp2 in
  let (temp4,temp5)=List.partition (fun hm->
    let s_hm=Half_dressed_module.to_string hm in
    not(Substring.begins_with s_hm "Optional/")
   ) temp3 in
  (temp4,temp5);;    

let outdated_interesting_modules mdata=
	List.filter (
       fun s->match Option.find_it(
         fun md->Half_dressed_module.to_string(Modulesystem_data.name md)=s
       ) mdata with
       None->true
       |Some(md0)->
         let hm0=Modulesystem_data.name md0 in
        (below mdata hm0)<>[]
    ) Interesting_modules.list;;
  

let industrial_separator=Industrial_separator.german_data;;    


let archive mdata=
     Nonblank.make(String.concat industrial_separator (Image.image Modulesystem_data.archive mdata));;
    

let unarchive s=
   let v1=Str.split (Str.regexp_string industrial_separator) (Nonblank.decode(s)) in
   Image.image Modulesystem_data.unarchive v1;;
     

  
 
 
 
