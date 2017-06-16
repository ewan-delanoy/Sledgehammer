(*

#use"absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Tools_for_absolute_path.of_string s);;
let to_string (AP s)=s;;

let ocaml_name ap=
 let s=to_string ap in
"Absolute"^"_path"^"."^"of_string(\""^s^"\"";;

let test_equal_paths s1 s2=
((of_string s1)=(of_string s2));;

exception Error_during_file_creation;;
 
let create_file w=
    let cr=(fun w->
      let ld=Unix.openfile w [Unix.O_RDWR;Unix.O_CREAT;Unix.O_EXCL] 0o666 in
       Unix.close ld
    ) in
    if Sys.file_exists w then of_string w else
    if (not(String.contains w '/'))
    then (cr w;of_string w)
    else 
    let i=String.rindex w '/' in
    let filename=String.sub w (i+1) ((String.length w)-(i+1)) in
    let g1="jnoxgghg_"^filename in
    let _=Unix_command.uc ("rm -f "^g1) in
    let _=cr g1 in
    let _=Unix_command.uc ("mv "^g1^" "^w) in
    let _=Unix_command.uc ("rm -f "^g1) in
    of_string w;;
    
let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;

