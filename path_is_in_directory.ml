(*

#use"path_is_in_directory.ml";;

*)

let path_is_in_directory ap dir=
  Substring.begins_with
   (Absolute_path.to_string ap)
   (Directory_name.connectable_to_subpath dir)
   ;;
