
(* 


#use"Country/Alaska/alaskan_force_modification_time.ml";;


*)

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let update dir mdata mlx=
   let hm=Mlx_ended_absolute_path.half_dressed_core mlx
   and edg=Mlx_ended_absolute_path.ending mlx in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Non_existent_mtime(mlx))
   else 
   let dt=Option.unpack opt in
   let file=(Directory_name.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
   let old_val=Modulesystem_data.modification_time dt edg 
   and new_val=(Unix.stat file).Unix.st_mtime  in
   if old_val=new_val
   then mdata
   else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
        before@(new_dt::after);;

    
   
   