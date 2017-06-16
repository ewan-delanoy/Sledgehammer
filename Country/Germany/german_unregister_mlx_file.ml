
(* 


#use"Country/Germany/german_unregister_mlx_file.ml";;


*)

 exception Non_registered_file of Mlx_ended_absolute_path.t;;  
 exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
 

 let on_monitored_modules mdata mlxfile=
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
      then before@after
      else before@(new_dt::after);;
    
 let on_targets (old_mdata,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_mdata=on_monitored_modules old_mdata mlx in
  let new_dirs=German_directories.from_data new_mdata
  and new_tgts=List.filter (fun tgt->
   	match Ocaml_target.main_module tgt with
   	None->false |Some(hm2)->hm2<>hm
  ) old_tgts in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=
    snd(Alaskan_make_ocaml_target.make
      German_constant.root
     (new_mdata,new_tgts) default_top) in
  (new_mdata2,new_dirs,new_tgts2);;   
  
