(*

#use"Country/Germany/german_update_copied_compiler.ml";;

*)

let prepare destdir=
  let l1=Alaskan_data.all_short_paths (German_wrapper.data()) in
  let l2=Image.image (
    fun ap->
      let s_ap=Absolute_path.to_string ap in
      Directory_name.cut_beginning German_constant.root s_ap
  ) (German_wrapper.outside_files()) in
  let main_diff=Prepare_dircopy_update.compute_diff (German_constant.root,l1@l2) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Directory_name.join destdir filename) in
  Replace_inside.replace_inside_file
  (Directory_name.connectable_to_subpath German_constant.root,Directory_name.connectable_to_subpath destdir)
  the_file;;


    
let ucc destdir=
  let _=Image.image Unix_command.uc (prepare destdir) in
  let _=Image.image (prepare_special_file destdir)
    ["my_pervasives.ml";"my_printers.ml";"my_loadings.ml"]
   in 
  Alaskan_create_target_system.from_main_directory destdir None [];;
       
       






   
   
  