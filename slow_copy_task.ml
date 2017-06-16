(*

#use"slow_copy_task.ml";;

*)

type t={
   port    : int;
   webhost : string;
   local_root: string;
   remote_root: string;
   mutable filenames   :string list;  
   keeper_name: string;
};;


let industrial_separator1=Industrial_separator.slow_copy_task1;;
let industrial_separator2=Industrial_separator.slow_copy_task2;;    

  
  
let archive x=
   String.concat industrial_separator1
   [
     string_of_int(x.port);
     x.webhost;
     x.local_root;
     x.remote_root;
     Nonblank.make(String.concat industrial_separator2 x.filenames);
     x.keeper_name;
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
{
   port        =int_of_string(List.nth l1 0);
   webhost     =List.nth l1 1;
   local_root  =List.nth l1 2;
   remote_root =List.nth l1 3;
   filenames   =Str.split (Str.regexp_string industrial_separator2) (List.nth l1 4);  
   keeper_name =List.nth l1 5;
};;


let watcher_file kn=
   if not(String.contains kn '/')
   then "ok_for_"^kn
   else let (a,b)=Father_and_son.father_and_son kn '/' in
        a^"/ok_for_"^b;;


let initialize_from_file ap=unarchive(Io.read_whole_file ap);;

let initialize_from_data
  (p,w,l,r,dir,kn)=
  let _=Unix_command.uc("touch "^kn) in
  let _=Unix_command.uc("touch "^(watcher_file kn)) in
  let temp1=More_unix.complete_ls_with_nondirectories_only dir in
  let d=String.length(l) in
  let temp2=Image.image (fun ap->
  Cull_string.cobeginning d (Absolute_path.to_string ap)) temp1 in
  
  {
   	port        =p;
   	webhost     =w;
   	local_root  =l;
   	remote_root =r;
   	filenames   =temp2;  
   	keeper_name =kn;
   };;

let save x=
   let ap=Absolute_path.of_string(x.keeper_name) in
   Io.erase_file_and_fill_it_with_string ap (archive x);;
   
let ask_permission_to_execute x=
   Sys.file_exists(watcher_file(x.keeper_name));;   

let execute_one_step x=
   if x.filenames=[] then 0 else
   let t=Single_copy_task.create_from_webhost_local_remote_file
   (x.port,x.webhost,x.local_root,x.remote_root,List.hd(x.filenames)) in
   let i=Single_copy_task.execute t in
   let _=(
   if i=0
   then x.filenames<-List.tl(x.filenames);
        save x
   ) in
   i;;   
   
let execute_all_steps x=
   if x.filenames=[] then () else
   let sn=string_of_int(List.length(x.filenames)) in
   let counter=ref(1) 
   and watcher=ref(ask_permission_to_execute x) in
   while (!watcher)
   do
   let i=execute_one_step x in
   let perm=ask_permission_to_execute x  in
   let message_end=(if i=0 then "succeeded" else "failed") in
   let message1="Step "^(string_of_int(!counter))^" of "^sn^" "^message_end in
   let message=(if perm then message1 else "Computation stopped.") in
   let _=(if i=0 then counter:=(!counter)+1) in
   print_string message;
   flush stdout;
   watcher:=(x.filenames<>[])&&perm;
   done;;  
   
(*

let example=initialize_from_data
  (7822,
   "tribunem@tribunemicael.net",
   "/Users/ewandelanoy/Documents/",
   "~/private_html/",
   Directory_name.of_string "/Users/ewandelanoy/Documents/Amailh",
   "Remembered/task_keeper1");;


*)
     

   
