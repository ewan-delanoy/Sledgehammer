(*

#use"shell_command.ml";;

*)



type dir_string=string;;
type command_string=string;;
type announce_string=string;;
type failure_string=string;;

type command=
  C of command_string*announce_string*failure_string
 |ChangeDir of dir_string*announce_string*failure_string;;


let of_strings s ann fai=C (s,ann,fai);;
let change_dir s_dir ann fai=ChangeDir(s_dir,ann,fai);;

let do_and_notice_failure s=
 let i=Unix_command.uc s in
 if i<>0
 then print_string("Failed during "^s);flush stdout;;

let unveil=function
 (C (s,ann,fai))->(s,ann,fai)
 |ChangeDir(dir_string,ann,fai)->("cd "^dir_string,ann,fai);;

exception Content_of_cd of string;;

let command_content=function 
 (C (s,ann,fai))->s
 |ChangeDir(dir_string,ann,fai)->raise(Content_of_cd(dir_string));;

let usual s=C(s,"","Failed during "^s);;

let semi_usual (s,t)=C(s,"","Failed during "^t);;

let usual_change_dir s=ChangeDir(s,"","Failed during cd "^s);;

let cd (d:dir_string)=(try (fun _->0)(Sys.chdir(d)) with 
   _->2);;
 

let execute_without_commenting=function
 (C (s,ann,fai))->Unix_command.uc s
 |ChangeDir(dir_string,ann,fai)->cd dir_string;;

let print_if_nonempty s=
  if s="" 
  then ()
  else print_string (s^"\n");flush stdout;;

let minimal_announce_and_do=function
 (C(s,ann,fai))->
  let _=print_if_nonempty ann in
  let bowl=((Unix_command.uc s)=0) in
  let _=(if not(bowl) then print_if_nonempty fai) in
  bowl
 |(ChangeDir(dir_string,ann,fai))->
  let _=print_if_nonempty ann in
  let bowl=((cd dir_string)=0) in
  let _=(if not(bowl) then print_if_nonempty fai) in
  bowl ;;

let maximal_announce_and_do=function
 (C(s,ann,fai))->
  let _=print_if_nonempty s in
  ((Unix_command.uc s)=0)
 |(ChangeDir(dir_string,ann,fai))->
  let _=print_if_nonempty ("cd "^dir_string) in
  ((cd dir_string)=0);;

let display_all_commands=ref(false);;

let announce_and_do x=
  if (!display_all_commands)
  then maximal_announce_and_do x
  else minimal_announce_and_do x;;
    
let take_care_of_root_directory root l=
  let s_root=Directory_name.connectable_to_subpath root 
  and s_cwd=Sys.getcwd() in
  if Absolute_path.test_equal_paths s_root s_cwd
  then l
  else (usual_change_dir s_root)::
     (l@[usual_change_dir s_cwd]);;     
    
let rec try_successively l=match l with
[]->true
|x::others->
    if announce_and_do x
    then try_successively others
    else false;;    
    
