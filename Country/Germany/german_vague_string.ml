(*

#use"Country/Germany/german_vague_string.ml";;

*)


let to_path x=Find_suitable_ending.find_file_location 
   German_constant.root (German_wrapper.directories()) x;;

exception Absent_module of string;;

let to_module x=
  let s=Father_and_son.invasive_father x '.' in
  match (Option.find_and_stop(
      fun edg->try(Some(to_path(s^edg))) with _->None
  ) Ocaml_ending.all_string_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->Half_dressed_module.of_path_and_root ap German_constant.root;;
   

