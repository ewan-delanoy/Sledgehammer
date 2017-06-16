
(* 


#use"Makefile_makers/update_ancs_libs_and_dirs_in_modulesystem.ml";;

Takes a modulesystem where everything is correct except possibly
the libs and dirs. Updates everything recursively starting
from the modules with no dependencies. 

*)


let moduledata_hshtbl=Hashtbl.create 500;;

exception Iterator_for_update_exn;; 

let iterator_for_update (graet,da_ober)=match da_ober with
  []->raise(Iterator_for_update_exn)
  |(md,atoms_for_md)::peurrest->
     let hm=Modulesystem_data.name md 
     and mlx=Modulesystem_data.principal_mlx md in 
     let new_ancestor_names=Image.image Modulesystem_data.name atoms_for_md in
     let genealogy=Image.image (Hashtbl.find moduledata_hshtbl) new_ancestor_names in
     let new_libs=Read_info_on_file_in_system.find_needed_libraries mlx genealogy
     and new_dirs=Read_info_on_file_in_system.find_needed_directories mlx genealogy in
     let new_md=
     {
    	Modulesystem_data.name=md.Modulesystem_data.name;
        ml_present=md.Modulesystem_data.ml_present;
        mli_present=md.Modulesystem_data.mli_present;
        mll_present=md.Modulesystem_data.mll_present;
        mly_present=md.Modulesystem_data.mly_present;
        ml_modification_time=md.Modulesystem_data.ml_modification_time;
        mli_modification_time=md.Modulesystem_data.mli_modification_time;
        mll_modification_time=md.Modulesystem_data.mll_modification_time;
        mly_modification_time=md.Modulesystem_data.mly_modification_time;
        needed_libraries=new_libs;
        direct_fathers=md.Modulesystem_data.direct_fathers;
        all_ancestors=new_ancestor_names;
        needed_directories=new_dirs;
     } in
     let _=Hashtbl.add moduledata_hshtbl hm new_md in
     (new_md::graet,peurrest);;
     
let rec computer_for_update (graet,da_ober)=
  if da_ober=[]
  then List.rev(graet)
  else computer_for_update(iterator_for_update (graet,da_ober));;   

let update l=computer_for_update ([],l);;


     
     