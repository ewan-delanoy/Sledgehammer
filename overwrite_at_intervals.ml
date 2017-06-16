(*

#use"overwrite_at_intervals.ml";;

*)



let inside_string replacings s=
  let n=String.length s
  and r=List.length replacings in
  let x_coord=(fun j->
    if j=1 then 1 else
    snd(fst(List.nth replacings ((j-3)/2)))+1
  ) and y_coord=(fun j->
   if j=2*r+1 then n else
    fst(fst(List.nth replacings ((j-1)/2)))-1
  ) in
  let xy_substring=(fun j->
    Cull_string.interval s (x_coord j) (y_coord j)
  ) in
  let all_parts=Ennig.doyle (
    fun j->
      if (j mod 2)=1
      then xy_substring j
      else Overwriter.to_string(snd(List.nth replacings ((j-2)/2)))
  ) 1 (2*r+1) in
  String.concat "" all_parts;;

(*

inside_string
 [(7,12),"garfield";(23,24),"jack";(30,30),"gas"]
 "12345678901234567890123456789012345678901234567890";;
 

*)

let inside_file replacings fn=
  let old_t=Io.read_whole_file fn in
  let new_t=inside_string replacings old_t in
  Io.erase_file_and_fill_it_with_string fn new_t;;  
  





