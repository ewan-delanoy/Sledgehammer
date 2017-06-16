(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)

(*
let uc=Sys.command;;
*)


exception Command_failed of string;;

let accu=ref([]:string list);;

let hardcore_uc s=
   let i=Sys.command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(accu:=s::(!accu)) in i;;

let hardcore_mode=ref(true);;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else Sys.command s;;


