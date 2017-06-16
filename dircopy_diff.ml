(*

#use"dircopy_diff.ml";;

*)

type t={
   recently_deleted : string list;
   recently_changed : string list;
   recently_created : string list;
};;

let recently_deleted x=x.recently_deleted;;
let recently_created x=x.recently_created;;
let recently_changed x=x.recently_changed;;

let veil a b c={
   recently_deleted =Recently_deleted.to_string_list a;
   recently_changed =Recently_changed.to_string_list b;
   recently_created =Recently_created.to_string_list c;
};;

let display x=
   let tempf=(fun msg l->
   "\n"::msg::(Image.image(fun w->"\t\t"^w) l)
   ) in
   let temp1=tempf "Deleted : " (x.recently_deleted)
   and temp2=tempf "Created : " (x.recently_created)
   and temp3=tempf "Changed : " (x.recently_changed) in
   let temp4=String.concat "\n" (temp1@temp2@temp3) in
   (print_string temp4;
    flush stdout);;

module Private=struct

let summarize_short_path s=
   String.capitalize_ascii(Father_and_son.son (Father_and_son.invasive_father s '.') '/');;
 
let summarize_short_path_list l=
    let temp1=Image.image summarize_short_path l in
    Ordered.forget_order(Ordered_string.diforchan temp1);;

end;;

let explain x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Option.filter_and_unpack tempf
   [
     "Deleted",Private.summarize_short_path_list(x.recently_deleted);
     "Created",Private.summarize_short_path_list(x.recently_created);
     "Modified",Private.summarize_short_path_list(x.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   
let is_empty x=
  (x.recently_deleted,x.recently_created,x.recently_changed)=
   ([],[],[]);;   
   
   
   
   
   
   