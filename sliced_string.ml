(*

#use"sliced_string.ml";;

A string is sliced when \n's are introduced at suitable places
in it, so that the display will have shorter lines and will 
therefore be more readable.

The type is abstract (see the mli) because displaying a very long of strings is
not very useful in the toplevel.

*)

type t=Sl of string list;;

let to_string_list (Sl l)=l;;
let of_string_list l=Sl l;;

let concat_two (Sl l1) (Sl l2)=Sl(l1@l2);;

let concat=function
[]->Sl[]
|a::peurrest->List.fold_left concat_two a peurrest;;
 
let print (Sl l)=String.concat "\n" l;;


 let itemize (shower:'a->string) l=
  let n=List.length(l) in
  let d=String.length(string_of_int(n)) in
  let temp1=Image.image(shower)(l) in
  let temp2=Ennig.index_everything(temp1) in
  let tempf=(function (j,v)->
     let s1=string_of_int(j) in
     let dd=d-String.length(s1) in
     let s2=(function ()->if dd>0 then String.make(dd)(' ') else "")() in
     s2^s1^"/  "^v
  ) in
  let temp3=Image.image tempf temp2 in
  of_string_list temp3;;
 
let max_line_length_ref=ref(70);;

type inner_separator=string;;
  
 let make_aggregates (sep:inner_separator)=function
 []->Sl[]
 |a::b->
  let rec tempf=(function
   (accu,breman,pouez,da_ober)->match da_ober with
    []->List.rev((List.rev breman)::accu)
	|y::peurrest->
	     let py=String.length y in
	     let pouez_nevez=pouez+py in
	     if (pouez_nevez>(!max_line_length_ref))
		 then tempf((List.rev breman)::accu,[y],py,peurrest)
		 else tempf(accu,y::breman,pouez_nevez,peurrest)
   ) in
   let temp1=tempf([],[a],String.length a,b) in
   Sl(Image.image (String.concat sep) temp1);;
