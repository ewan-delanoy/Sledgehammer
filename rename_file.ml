
(* 


#use"rename_file.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)

exception Failed of Absolute_path.t*No_slashes.t;;

let rename ap new_name=
  let old_path=Absolute_path.to_string ap
  and (dir,_)=Unjoin_path.unjoin_path ap in
  let new_path=(Directory_name.connectable_to_subpath dir)^(No_slashes.to_string new_name) in
  let i=Unix_command.uc("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_name))
  else Absolute_path.of_string new_path;;



   
   
   