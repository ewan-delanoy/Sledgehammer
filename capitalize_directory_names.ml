(*

#use"capitalize_directory_names.ml";;


*)

let cdn s=
  let n=String.length(s) in
  let temp1=List.filter(
     fun j->(String.get s j)='/'
  )(Ennig.ennig 0 (n-1)) in
  if temp1=[] then s else
  let temp4=List.rev(List.tl(List.rev temp1)) in
  let temp2=0::(Image.image (fun j->j+1) temp4) in
  let tempf=(fun j->
    let t=String.make 1 (String.get s j) in
    if List.mem j temp2
    then String.capitalize_ascii t
    else t
  ) in
  let temp3=Ennig.doyle tempf 0 (n-1) in
  String.concat "" temp3;;

(*

cdn "do/You/feel/like/sixty/feet";;
cdn "/do/You/feel/like/sixty/feet";;
cdn "do/You/feel/like/sixty/feet/";;
cdn "/do/You/feel/like/sixty/feet/";;

cdn "peggy";;
cdn "peggy/lee";;

*)