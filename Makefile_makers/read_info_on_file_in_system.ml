
(* 


Recompute the characteristics of a module
stored in memory.

#use"Makefile_makers/read_info_on_file_in_system.ml";;


*)


let find_needed_data_for_file l fn=
   let temp1=Look_for_module_names.names_in_file fn in
   let selecter=(fun info->
     let hm=Modulesystem_data.name info in
     let name=Half_dressed_module.naked_module hm in
     if List.mem name temp1
     then Some(info)
     else None
   ) in
   Option.filter_and_unpack selecter l;;

let find_needed_data l mlx=
   let fn=Mlx_ended_absolute_path.to_path mlx in
   find_needed_data_for_file l fn;;

let find_needed_names l mlx=
   let temp1=find_needed_data l mlx in
   Image.image Modulesystem_data.name temp1;;

 let find_needed_libraries mlx genealogy=
   let fn=Mlx_ended_absolute_path.to_path mlx in
   let temp1=Look_for_module_names.names_in_file fn in
   List.filter
   (
     fun lib->
       if List.exists 
          (fun mdl->List.mem(Naked_module.of_string mdl)(temp1))
            (Ocaml_library.modules_telling_a_library_away lib)
       then true
       else List.exists 
            (fun info->List.mem lib (Modulesystem_data.needed_libraries info) ) 
            genealogy
   )
   Ocaml_library.all_libraries;;

 let find_needed_directories mlx genealogy=
   let temp1=Image.image 
     (fun t->Tidel.diforchan(Modulesystem_data.needed_directories t)) 
       genealogy in
   let s_mlx=Mlx_ended_absolute_path.to_string mlx in
   let temp2=(fun bowl->
       if bowl 
       then let new_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
            Tidel.singleton(new_subdir)::temp1
       else temp1
   )(String.contains s_mlx '/') in    
   let temp3=Tidel.big_teuzin temp2 in
   Ordered.forget_order temp3;;
 
 let check_presences l hm=
 match Option.find_it (fun a->Modulesystem_data.name a=hm) l with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 
 let complete_info l mlx=
   let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
   let genealogy=find_needed_data l mlx in
   let (mlp,mlip,mllp,mlyp)=check_presences l hm
   and (mlmt,mlimt,mllmt,mlymt)=Modulesystem_data.compute_modification_times hm in
   let dirfath=Image.image (Modulesystem_data.name) genealogy in
   let temp1=Image.image 
   			(fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
   			genealogy in
   let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
   let tempf=(fun t->
   					let nam_t=Modulesystem_data.name t in
   					if Tidel.elfenn nam_t temp2
   					then Some(nam_t)
   					else None) in
   let allanc=Option.filter_and_unpack tempf l in
   let libned=find_needed_libraries mlx genealogy
   and dirned=find_needed_directories mlx genealogy in
   Modulesystem_data.make
   (hm,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned);;
   
let recompute_complete_info_for_module l hm=
  let opt=Option.find_it(fun a->Modulesystem_data.name a=hm) l in
  let dt=Option.unpack opt in
  let edg=List.hd(Modulesystem_data.registered_endings dt) in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  complete_info l mlx;;
        
    
let quick_update l x=
  let hm=Modulesystem_data.name (x) in
  if (Half_dressed_module.to_string hm)=Debugger_name.debugger_name
  then None
  else
  let new_values=Modulesystem_data.compute_modification_times hm 
  and old_values=Modulesystem_data.modification_times x in
  if old_values=new_values
  then None
  else
  let (n_ml,n_mli,n_mll,n_mly)=new_values in
  let edg=List.hd(Modulesystem_data.registered_endings x) in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let fathers=find_needed_names l mlx in
  Some(
  {
    Modulesystem_data.name=x.Modulesystem_data.name;
    ml_present=x.Modulesystem_data.ml_present;
    mli_present=x.Modulesystem_data.mli_present;
    mll_present=x.Modulesystem_data.mll_present;
    mly_present=x.Modulesystem_data.mly_present;
    ml_modification_time=n_ml;
    mli_modification_time=n_mli;
    mll_modification_time=n_mll;
    mly_modification_time=n_mly;
    needed_libraries=x.Modulesystem_data.needed_libraries;
    direct_fathers=fathers;
    all_ancestors=x.Modulesystem_data.all_ancestors;
    needed_directories=x.Modulesystem_data.needed_directories;
   }   
   )   
  ;;
  
  