
(* 


#use"relocate_file.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)

exception Failed of Absolute_path.t*Directory_name.t;;

let relocate ap new_dir=
  let old_path=Absolute_path.to_string ap
  and (_,fn)=Unjoin_path.unjoin_path ap in
  let new_path=(Directory_name.connectable_to_subpath new_dir)^(No_slashes.to_string fn) in
  let i=Unix_command.uc("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_dir))
  else Absolute_path.of_string new_path;;



   
   
   