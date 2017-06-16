(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)


let is_admissible s=
  (List.for_all (
     fun edg->not(Substring.ends_with s ("."^edg))
  ) ["depend";"ocamlinit";"cmi";"cmo";"DS_Store";"txt";"php";
     "ocaml_made";"ocaml_debuggable"])
  &&
   (List.for_all (
     fun beg->not(Substring.begins_with s beg)
  ) ["Remembered/";"Forgotten/"])
  &&
  (
    not(
    List.mem s
    ["debugger.ml";"my_loadings.ml";
     "ecaml";"makefile";"neptu"]
    )
  )
  ;;
  
let name_of_clone_directory="/Users/ewandelanoy/Downloads/Clone";;  
let github_clone_command=
"git clone https://github.com/ewan-delanoy/Ludlow-Street "^
name_of_clone_directory;;  

exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

let check ()=
  let i=(
    if Sys.file_exists(name_of_clone_directory)
    then Unix_command.uc("rm -rf "^name_of_clone_directory) 
    else 0
  ) in
  if i<>0
  then raise(Failure_in_clone_directory_creation)
  else 
  let _=Unix_command.uc("mkdir -p "^name_of_clone_directory) in
  let remotedir=Directory_name.of_string name_of_clone_directory in
  let j=Unix_command.uc github_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let diff=Prepare_dircopy_update.compute_greedy_diff
     German_constant.root remotedir in
  let rc1=List.filter is_admissible (Dircopy_diff.recently_deleted diff)
  and rc2=List.filter is_admissible (Dircopy_diff.recently_changed diff)
  and rc3=List.filter is_admissible (Dircopy_diff.recently_created diff) in
  (rc1,rc2,rc3);;
