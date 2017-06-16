(*


#use"my_global_replace.ml";;

The my_global_replace is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.


*)


exception Ambiguity of string;;

let my_global_replace (a,b) s=
  let n=String.length(s) and na=String.length(a) in
  let indices=Substring.occurrences_of_in a s in
  if indices=[] then s else
  let m=List.length(indices)+1 in
  let pattern_appears_left=((List.nth indices 0)=1)
  and pattern_appears_right=((List.nth indices (m-2))=n+1-na) in
  if m=1 then s else
  let fst_coord=(fun j->if j=1 then 1 else (List.nth indices (j-2))+na)
  and snd_coord=(fun j->if j=m then n else (List.nth indices (j-1))-1) in
  let coords=(fun j->
    if (j=1)&&pattern_appears_left then None else
    if (j=m)&&pattern_appears_right then None else
    Some(fst_coord j,snd_coord j)
  )
  in
  let unchanged_intervals=Option.filter_and_unpack coords (Ennig.ennig 1 m) in
  if List.exists (fun (x,y)->x>y) unchanged_intervals
  then raise(Ambiguity(a)) 
  else 
  let unchanged_substrings=Image.image (fun (x,y)->Cull_string.interval s x y) unchanged_intervals in
  let draft=String.concat b unchanged_substrings in
  let left_padding=(if pattern_appears_left then b else "")
  and right_padding=(if pattern_appears_right then b else "") in
  left_padding^draft^right_padding;;
  
(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
*)  
  
