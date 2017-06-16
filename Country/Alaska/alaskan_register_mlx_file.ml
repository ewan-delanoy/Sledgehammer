
(* 

#use"Country/Alaska/alaskan_register_mlx_file.ml";;


*)

exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
let on_monitored_modules mdata mlx_file =
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

  
let on_targets (old_mdata,old_dirs,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_dir=Half_dressed_module.subdirectory hm in
 let new_mdata=on_monitored_modules old_mdata mlx in
 let new_dirs=
 (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
 and new_tgts=
 (if Half_dressed_module.is_optional hm
  then old_tgts
  else (*
       The only outdated targets are the main toplevel, 
       and targets corresponding to an identical module
       (for example when a mll or mly is added to
       an already registered ml) 
        *)
       List.filter (
        fun tgt->match Ocaml_target.toplevel_name tgt with
          None->(match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
                )
          |Some(name)->false
       ) old_tgts
  ) in
  (new_mdata,new_dirs,new_tgts);; 
 

