
(* 

#use"Country/Germany/german_register_outside_file.ml";;


*)


let on_outside_directories (o_files,o_dirs) ap=
  let main_dir=German_constant.root in
  let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap) in
  let s_sdir=Father_and_son.father temp1 '/' in
  let sdir=Subdirectory.of_string s_sdir in
  let new_odirs=(if List.mem sdir o_dirs then o_dirs else o_dirs@[sdir]) 
  and new_ofiles=(if List.mem ap o_files then o_files else ap::o_files) in
  (new_ofiles,new_odirs);;
  
(*  

let unregister_outside_file (o_files,o_dirs) ap=
  let main_dir=German_constant.root in
  let tempf=(fun ap2->
  	let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap2) in
  	let s_sdir=Father_and_son.father temp1 '/' in
  	Subdirectory.of_string s_sdir 
  ) in
  let sdir=tempf ap in 
  let new_ofiles=List.filter (fun ap2->ap2<>ap) o_files in
  let new_odirs=(
       if List.exists(fun z->tempf(z)=sdir) new_ofiles
       then o_dirs
       else List.filter (fun sdir2->sdir2<>sdir) o_dirs
  )  in
  (new_ofiles,new_odirs);;     

*)