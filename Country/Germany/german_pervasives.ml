(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=German_constant.root;;

let s_cdir=Directory_name.connectable_to_subpath cdir;;
let current_registered_directories ()=German_wrapper.directories();;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory.SD ""
  ) (current_registered_directories()) in
  (Subdirectory.SD "")::(temp1@
  [Subdirectory.SD "Remembered";Subdirectory.SD "Forgotten"]);;

let fl=German_vague_string.to_path;;
let hmx=German_vague_string.to_module;;

let fmr x=Alaskan_data.find_module_registration (German_wrapper.data()) (hmx x);;
let abo x=German_data.above (German_wrapper.data()) (hmx x);;
let bel x=German_data.below (German_wrapper.data()) (hmx x);;
let dbel x=German_data.directly_below (German_wrapper.data()) (hmx x);;


let ren_without_backup x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo_without_backup x y=German_wrapper.relocate_module (hmx x) y;;
let mmo_without_backup x=German_wrapper.make_module_optional (hmx x) ;;



let fg_without_backup x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else German_wrapper.forget_module (hmx x);;

let regi_without_backup x= 
  let mlx=Mlx_ended_absolute_path.of_path_and_root (fl x) cdir in
  German_wrapper.register_mlx_file mlx;;

let rego_without_backup x=
   German_wrapper.register_outside_file (Absolute_path.of_string x);;

let ureg_without_backup x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mll";".mly"] 
  then let mlx=Mlx_ended_absolute_path.of_path_and_root (fl x) cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (hmx x);;

let double_semicolon=";"^";";;

let cf t1 t2=
   let ap1=fl t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ap2=(Father_and_son.invasive_father s_ap1 '/')^"/"^t2^".ml" in
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let s1=Cull_string.cobeginning (String.length s_cdir) s_ap1
   and s2=Cull_string.cobeginning (String.length s_cdir) s_ap2 in
   let txt1="#use\""^s1^"\""^double_semicolon
   and txt2="#use\""^s2^"\""^double_semicolon in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) ap2  in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;   

let vo s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s1);;


let syz()=German_data.system_size (German_wrapper.data());;

let init=German_wrapper.initialize;;
let reco_ref=ref(0);;
let reco_without_backup ()=
  let _=(reco_ref:=(!reco_ref)+1) in
  German_wrapper.recompile();;


let pbk ()=Dircopy_diff.display(German_wrapper.diff());;
let bk=German_backup_target_system.backup_with_message (German_wrapper.diff());;

let rd ()=Alaskan_remove_debuggables.rd German_constant.root (German_wrapper.data());;
let sd=German_wrapper.start_debugging;;


let rv_without_backup x y=German_values_in_modules.rename_string_or_value (German_wrapper.data()) x y;;



let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem wal (German_wrapper.data()) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem modname (German_wrapper.data()) ;;
let muv x=German_values_in_modules.modules_using_value (German_wrapper.data()) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let vld=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(German_wrapper.data())) ;;

let rsh_without_backup=German_wrapper.refresh;;

let am ()=
    let temp1=German_wrapper.data() in
    let temp2=Image.image (fun md->
     Naked_module.to_string(
       Half_dressed_module.naked_module(Modulesystem_data.name md))
    ) temp1 in
    temp2;;
    

let oim ()=German_data.outdated_interesting_modules (German_wrapper.data());;
let df () =German_data.deletable_files (German_wrapper.data());;
let ucc ()=German_update_copied_compiler.ucc (Directory_name.of_string "/Users/ewandelanoy/Documents/OCaml/Cherokee");;

let reco ()=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup None);;
let reco_with_comment s=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup (Some s));;


let fg x=(fg_without_backup x;German_wrapper.backup None);;
let mmo x=(mmo_without_backup x;reco());;

let regi x=(regi_without_backup x;German_wrapper.backup None);;
let rndir p=(German_wrapper.rename_directory p;reco());;

let rego x=(rego_without_backup x;reco());;
let relo x y=(relo_without_backup x y;reco());;
let ren  x y=(ren_without_backup  x y;reco());;
let rsh ()=(rsh_without_backup ();German_wrapper.backup None);;
let rv x y=(rv_without_backup x y;reco());;
let ureg x=(ureg_without_backup x;reco());;



