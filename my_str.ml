(*

An adptation of OCaml's Str module :

String indices are now from 1 to n instead of 0 to (n-1).
Operations on regexps are encoded as functions in the module.

#use"my_str.ml";;

*)

type backtrack_length=int;;

type regexp=M of string*(Str.regexp)*backtrack_length;;

let unveil (M(s,_,b))=s;;
let veil s=M(s,Str.regexp s,0);;

let set_backtrack (b:backtrack_length) (M(s,rgxp,_))=M(s,rgxp,b);;

let regexp_string s=let quote=Str.quote s in veil quote;;

let plus (M (s,_,_))=let new_s="\\("^s^"\\)+" in veil new_s;;
let star (M (s,_,_))=let new_s="\\("^s^"\\)*" in veil new_s;;

let concat (M(s1,_,_)) (M(s2,_,_))=veil (s1^s2);;
   
let big_concat l=
  if l=[]
  then veil ""
  else let temp1=List.map (fun w->"\\("^(unveil w)^"\\)") l in
       let new_s=String.concat "" temp1 in
       veil new_s;;   

let big_or l=
  let temp1=List.map (fun (M(s,_,_))->"\\("^s^"\\)") l in
  let new_s=String.concat "\\|" temp1 in
  veil new_s;;

let ore a b=big_or [a;b];;

let string_match (M(_,rgxp,b)) s i0=
   let bowl=(Str.string_match rgxp s (i0-1)) in
   if bowl
   then Some(i0+(String.length(Str.matched_string s))-b-1)
   else None;;
   
(* Functions from the option or ennig s *)  

let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let unpack=function
None->failwith("Void is not unpackable")
|Some(x)->x;;   
   
let rec find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;
   
(* End of functions from the option or ennig modules *)     
      
   
type left_regexp=regexp;;
type center_regexp=regexp;;   
type right_regexp=regexp;;   
   
type centered_regexp=left_regexp*center_regexp*right_regexp;;   
 
let create_centered_regexp a b c=((a,b,c):centered_regexp);;   
   
let centered_regexp_match ((a,b,c):centered_regexp) s i0=
  let opt1=string_match a s i0 in
  if opt1=None then None else
  let i1=unpack(opt1)+1 in
  let opt2=string_match b s i1 in
  if opt2=None then None else   
  let i2=unpack(opt2)+1 in 
  let opt3=string_match c s i2 in
  if opt3=None then None else   
  Some(i1,i2-1);;
   
 
let centered_regexp_list_match l s i0=
  let temp1=index_everything l in
  find_and_stop(fun (i,rgxp)->
    match centered_regexp_match rgxp s i0 with
     Some(j,k)->Some(i,(j,k))
    |None->None
  ) temp1;;   
   
 let find_all_occurrences l s i0=
  let n=String.length s in
  let rec tempf=(fun (graet,j)->
    if j>n then List.rev(graet) else
    let opt=centered_regexp_list_match l s j in
    if opt=None then tempf(graet,j+1) else
    let (k,(l,m))=unpack(opt) in
    tempf((k,(l,m))::graet,m+2)
  ) in
  tempf([],i0);;

   
   
 (*  
 
 centered_regexp_match (veil "12.",veil"4.6",veil"78") "123456789abcdef" 1;;
 
 let s1="123456789abcdef";;
 let a1=veil "12." and b1=veil"4.6" and c1=veil"78";;
 
 let w1=string_match a1 s1 1;;
 
 
 
 string_match (regexp_string "456") "123456789abcdef" 4;; 

let test_centered_regexp  
 
let search_forward (M(_,rgxp,b)) s i0=
   let j1=(Str.search_forward rgxp s (i0-1))+1 in
   let j2=j1+(String.length(Str.matched_string s))-b-1 in
   (j1,j2);;
  *) 