(*

#use"mlx_ended_absolute_path.ml";;

*)

type t=MLX of Ocaml_ending.t*string*Directory_name.t;;

exception Unknown_ending of string;;
exception Unpointed_filename of string;;

exception Inexistent_filename of string;;

let short_path (MLX(edg,s,_))=match edg with
   Ocaml_ending.Ml->  s^".ml"
  |Ocaml_ending.Mli-> s^".mli"
  |Ocaml_ending.Mll-> s^".mll"
  |Ocaml_ending.Mly-> s^".mly";;

let to_string=short_path;;

let of_string_and_root s dir= 
  if not(String.contains s '.') then raise(Unpointed_filename(s)) else
  let (core,ending)=Father_and_son.father_and_son s '.' in
  let s_dir=Directory_name.connectable_to_subpath dir in
  if (not(Sys.file_exists(s_dir^s)))
  then raise(Inexistent_filename(s_dir^s))
  else
  if ending="ml"  then MLX (Ocaml_ending.ml,core,dir) else
  if ending="mli" then MLX (Ocaml_ending.mli,core,dir) else
  if ending="mll" then MLX (Ocaml_ending.mll,core,dir) else
  if ending="mly" then MLX (Ocaml_ending.mly,core,dir) else
  raise(Unknown_ending(s));;

let try_from_string_and_root s dir=
  try (Some(of_string_and_root s dir)) with _->None;;


let root (MLX(_,_,dir))=dir;;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.connectable_to_subpath dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    of_string_and_root subpath dir;;    

let try_from_path_and_root ap dir=
    try (Some(of_path_and_root ap dir)) with _->None;;

let decompose (MLX(edg,s,dir))=
  (Half_dressed_module.of_string_and_root s dir,edg);;

let half_dressed_core mlx=fst(decompose mlx);;
let ending mlx=snd(decompose mlx);;

let to_path mlx=
  let (hm,edg)=decompose mlx in
  let dir=root mlx in
  let s_hm=Half_dressed_module.to_string hm 
  and s_dir=Directory_name.connectable_to_subpath dir in
  Absolute_path.of_string( s_dir^s_hm^(Ocaml_ending.to_string edg) );;

let join hs ending=
  let (s,dir)=Half_dressed_module.unveil hs in
  MLX(ending,s,dir);;

  
exception Failed_File_Renaming of t*string;;  
  
(*

notice that the ending is preserved below. If the user
unwittingly puts a wrong ending, this will have no effect.

*)  
  
let do_file_renaming mlx new_name=
  let core=Father_and_son.invasive_father (No_slashes.to_string new_name) '.' in
  let checked_name=No_slashes.of_string(core^(Ocaml_ending.to_string(ending mlx))) in
  let ap=to_path mlx in
  let new_ap=Rename_file.rename ap checked_name in
  of_path_and_root new_ap (root mlx);;   
  
let do_file_displacing mlx new_subdir=
  let s_new_subdir=Subdirectory.connectable_to_subpath new_subdir
  and dir=root mlx in
  let s_dir=Directory_name.connectable_to_subpath dir in
  let new_dir=Directory_name.of_string(s_dir^s_new_subdir) in
  let ap=to_path mlx in
  let new_ap=Relocate_file.relocate ap new_dir in
  of_path_and_root new_ap (root mlx);;  
  
let rename_endsubdirectory (subdir,newdirname) (MLX(edg,s,dir))=
  MLX(edg,Rename_endsubdirectory.re (subdir,newdirname) s,dir);;
  
let is_optional x=Half_dressed_module.is_optional(half_dressed_core x);;  
let is_archived x=Half_dressed_module.is_archived(half_dressed_core x);;  

let complete_ls dir=
  let temp1=Directory_name.connectable_to_subpath dir in
  let temp2=More_unix.quick_beheaded_complete_ls temp1 in
  let temp3=Option.filter_and_unpack(
     fun s->try_from_string_and_root s dir
  ) temp2 in
  List.filter (fun mlx->not(is_archived mlx)) temp3;;

let to_absolute_path mlx=
  let s=short_path mlx
  and dir=root mlx in
 let s_dir=Directory_name.connectable_to_subpath dir in
 Absolute_path.of_string(s_dir^s);;   


let ocaml_name w=
  let s=short_path w
  and dir=root w in
  "Mlx_file"^"name"^".of_string_and_index("^
  (Strung.enclose s)^
  ")("^(Directory_name.connectable_to_subpath dir)^")";;    

let industrial_separator=Industrial_separator.mlx_ended_absolute_path;;  
 


let prepare_archive (MLX(edg,s,dir))=
  let s_edg=Ocaml_ending.to_string(edg) in
  let shortened_s_edg=String.sub s_edg 1 (String.length(s_edg)-1) in
  [shortened_s_edg;s;Directory_name.connectable_to_subpath dir];;

  
let archive x=String.concat industrial_separator (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   let edg=List.hd l1 
   and s=List.nth l1 1 
   and dir=Directory_name.of_string(List.nth l1 2) in
   MLX(Ocaml_ending.of_string("."^edg),s,dir);;
