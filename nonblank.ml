
(* 

Useful to avoid empty strings between two successive separators in
an archive string. To see example of how it is used, look at the
archive/unarchive function in the new_modulesystem_data module for
example.

#use"swoon.ml";;

*)

let make s=if s="" then "#" else s;;
let decode s=if s="#" then "" else s;;
  
