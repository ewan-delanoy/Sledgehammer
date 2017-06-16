(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important

*)

exception No_suitable_location of Directory_name.t*(Subdirectory.t list)*string;;

let find_file_location dir l_subdir old_x=
  let x=String.uncapitalize_ascii old_x in
  let s_dir=Directory_name.connectable_to_subpath(dir) in
  let original_endings=Ocaml_ending.all_string_endings in
  let endings=(
     if List.exists (fun edg->Substring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Cartesian.product(l_subdir) endings in
  let tempf=(fun (sd,edg)->
  	let s1=s_dir^(Subdirectory.connectable_to_subpath sd)^x^edg in
  	if Sys.file_exists s1
  	then Some(Absolute_path.of_string s1)
  	else None
  ) in
  let opt=Option.find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Option.unpack(opt);;