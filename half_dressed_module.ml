(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)


          
type t={
   bundle_main_dir : string;
   subdirectory    : string;
   naked_module     : string;
};;

let bundle_main_dir x=Directory_name.of_string(x.bundle_main_dir);;
let subdirectory x=Subdirectory.of_string(x.subdirectory);;
let naked_module  x=Naked_module.of_string(x.naked_module);;


exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Directory_name.without_trailing_slash dir in
	    if List.for_all (fun edg->not(Sys.file_exists(s_dir^"/"^s^edg)) ) Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
	    };;  
   
   
let to_string x=
   let sub=x.subdirectory in
   if sub=""
   then x.naked_module
   else sub^"/"^(x.naked_module);;

let unveil x=(to_string x,bundle_main_dir x);;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.without_trailing_slash dir in
    let n_dir=(String.length s_dir)+1 in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    let s=Father_and_son.invasive_father subpath '.' in
    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
    }  ;;    

let is_optional x=
  let s=to_string x in
  if String.length(s)<9 then false else
  String.sub s 0 9="Optional/";;

let is_forgotten x=
  let s=to_string x in
  if String.length(s)<10 then false else
  String.sub s 0 10="Forgotten/";;


let is_remembered x=
  let s=to_string x in
  if String.length(s)<11 then false else
  String.sub s 0 11="Remembered/";;

let is_archived hm=(is_optional hm)||(is_forgotten hm)||(is_remembered hm);;

let is_executable x=
  let s=to_string x in 
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;

let capitalized_module_name x=
  (String.capitalize_ascii x.naked_module);;
  
let rename_endsubdirectory (old_subdir,new_subdirname) x=
   {
	      bundle_main_dir = x.bundle_main_dir;
   		  subdirectory    = Subdirectory.without_trailing_slash(
   		                    Subdirectory.rename_endsubdirectory
   		                       (old_subdir,new_subdirname) 
   		                       (Subdirectory.of_string(x.subdirectory)));
          naked_module    = x.naked_module;
    }  ;;    
   

let industrial_separator=Industrial_separator.half_dressed_module;;  

let archive x=
   String.concat industrial_separator 
    [x.bundle_main_dir;x.subdirectory;x.naked_module];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   {
	      bundle_main_dir = List.nth l1 0;
   		  subdirectory    = List.nth l1 1;
          naked_module    = List.nth l1 2;
    };;  
   
            
          
   
