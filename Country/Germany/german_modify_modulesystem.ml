
(* 


#use"Country/Germany/german_modify_modulesystem.ml";;


*)


 
 exception Non_registered_file of Mlx_ended_absolute_path.t;;  
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  

 let unregister_mlx_file_and_keep_old_data mdata mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Abandoned_children(mlxfile,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    match opt with
     None->raise(Non_registered_file(mlxfile))
    |Some(dt)->
      let edg=Mlx_ended_absolute_path.ending mlxfile in
      if (not(Modulesystem_data.check_presence edg dt))
      then raise(Non_registered_file(mlxfile))
      else 
      let new_dt=Modulesystem_data.make_absence edg dt in
      if (Modulesystem_data.registered_endings new_dt)=[]
      then (dt,before@after)
      else (dt,before@(new_dt::after));;
    
   
 let unregister_mlx_file mdata mlxfile=snd(unregister_mlx_file_and_keep_old_data mdata mlxfile);;
   
 
 let unregister_module mdata hm=
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else before@after;;
    
   
  
exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
 let register_mlx_file mdata mlx_file =
   let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
   and ending=Mlx_ended_absolute_path.ending mlx_file in 
   let nm=Half_dressed_module.naked_module hm in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.naked_module(Modulesystem_data.name dt)=nm) 
      mdata in
   if opt=None
   then  let old_info=Read_info_on_file_in_system.complete_info mdata mlx_file in
         let info1=Modulesystem_data.make_presence ending old_info in
         (*
         if a mll or mly file is being registered, the ml will automatically be created,
         so let us anticipate by already adding a ml presence
         *)
         let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
         then Modulesystem_data.make_ml_present info1 else info1) in
         	  before@[info] 
   else
   let old_dt=Option.unpack(opt) in
   let old_name=Modulesystem_data.name old_dt in
   if (old_name<>hm)
   then raise(Name_conflict(old_name,hm))
   else 
   let edgs=Modulesystem_data.registered_endings old_dt in
   if List.length(edgs)>1
   then  raise(Overcrowding(mlx_file,edgs))
   else  
   if List.mem ending edgs
   then raise(Already_registered_file(mlx_file))
   else
   if (not(List.mem Ocaml_ending.ml (ending::edgs)))
   then raise(Bad_pair(mlx_file,List.hd edgs))
   else 
   let dt1=Read_info_on_file_in_system.complete_info mdata mlx_file in
   let new_dt=Modulesystem_data.make_presence ending dt1 in
   if ending<>Ocaml_ending.ml
   then before@(new_dt::after) 
   else 
   let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
   if temp3=[]
   then before@(new_dt::after)
   else  
   let last_father=List.hd(List.rev(Modulesystem_data.direct_fathers new_dt)) in
   let (before1,opt1,after1)=Three_parts.select_center_element  (fun dt->
           (Modulesystem_data.name dt)=last_father) before in
   let lf1=Option.unpack opt1  in    
   let temp2=Image.image (Modulesystem_data.update_anclibdir new_dt mdata) (after1@after) in
   let final_list=before1@(lf1::new_dt::temp2) in
   final_list;;
  
  let try_to_register_mlx_file mdata mlx_file=
    try(Some(register_mlx_file mdata mlx_file)) with _->None;;  

   let try_to_register_mlx_files mdata mlx_files=
   let rec tempf=(fun
    (vdata,failures,yet_untreated)->
      match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match try_to_register_mlx_file vdata mlx with
        None->tempf(vdata,mlx::failures,others)
        |Some(nfs)->tempf(nfs,failures,others)
      )
   ) in
   tempf(mdata,[],mlx_files);;
  
   
  exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
  exception Nonregistered_module of Half_dressed_module.t;;  
  
   
let reposition mdata hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_data=before@after in
   German_arrange_positions_in_modulesystem.insert_data amputated_data info (l_before,l_after);;  

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

    
let recompute_module_info mdata hm=
   let (before,_,after)=Three_parts.select_center_element(
      fun md->Modulesystem_data.name md=hm
   ) mdata in
  let new_md=Read_info_on_file_in_system.recompute_complete_info_for_module
       mdata hm in 
  before@(new_md::after);;
   

     
   
   