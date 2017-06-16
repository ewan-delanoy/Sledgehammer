(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)

type t=D of string;;

let unsafe_from_string s=D s;;

exception Non_directory of string;;

let of_string s=
  let temp1=Tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then D(Tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let without_trailing_slash (D s)=s;;

let connectable_to_subpath (D s)=s^"/";;

exception Nonexistent_file of string;;

module Private=struct
let check_filename t=
  if Sys.file_exists t
  then t
  else raise(Nonexistent_file(t));;
end;;

let join (D s) w=Private.check_filename(s^"/"^w);;

let force_join (D s) w=
   let t=s^"/"^w in
   if Sys.file_exists t
   then t
   else let _=Unix_command.uc("touch "^t) in
        t;;

exception Cut_error of t*string;;

let cut_beginning (D s) w=
   let ns=String.length(s)
   and nw=String.length(w) in
   if (ns+1)>nw then raise(Cut_error(D s,w)) else
   if (String.sub w 0 (ns+1))<>(s^"/") then raise(Cut_error(D s,w)) else
   String.sub w (ns+1) (nw-ns-1);;
   


let ocaml_name (D s)="Directory_name"^"."^"unsafe_from_string(\""^s^"\")";;