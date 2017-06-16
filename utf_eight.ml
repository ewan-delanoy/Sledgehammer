(*

#use"utf_eight.ml";;

*)



let array_to_predict_char_length = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];;
  
exception Malformed_exn of int*(int list);;   
exception Bad_starter of int*int;;

let pusher_for_decoding (stream,j) =
  let b0=List.nth stream j in
  let l=Array.get array_to_predict_char_length b0 in
  if l=1 
  then (b0,j+l)
  else
  let b1=List.nth stream (j+1) in
  if l=2
  then (
        if b1 lsr 6 != 0b10 
        then raise(Malformed_exn(1,[b0;b1])) 
        else (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F),j+l)
       )
  else
  let b2=List.nth stream (j+2) in    
  if b2 lsr 6 != 0b10 then raise(Malformed_exn(2,[b0;b1;b2]))  else 
  if l=3
  then (
        let c = ((b0 land 0x0F) lsl 12) lor
                ((b1 land 0x3F) lsl 6) lor
                 (b2 land 0x3F) in
        begin match b0 with
      	| 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then raise(Malformed_exn(3,[b0;b1;b2])) else (c,j+l)
      	| 0xED -> if b1 < 0x80 || 0x9F < b1 then raise(Malformed_exn(4,[b0;b1;b2])) else (c,j+l)
      	| _ -> if b1 lsr 6 != 0b10 then raise(Malformed_exn(5,[b0;b1;b2])) else (c,j+l)
      	end
       )
  else
  let b3=List.nth stream (j+3) in  
  if b3 lsr 6 != 0b10 then raise(Malformed_exn(6,[b0;b1;b2;b3]))  else  
  if l=4
  then 
       (
        let c = ((b0 land 0x07) lsl 18) lor
                ((b1 land 0x3F) lsl 12) lor
          		((b2 land 0x3F) lsl 6) lor
                 (b3 land 0x3F)        in
        begin match b0 with
      	| 0xF0 -> if b1 < 0x90 || 0xBF < b1 then raise(Malformed_exn(6,[b0;b1;b2;b3])) else (c,j+l)
      	| 0xF4 -> if b1 < 0x80 || 0x8F < b1 then raise(Malformed_exn(7,[b0;b1;b2;b3])) else (c,j+l)
      	| _ -> if b1 lsr 6 != 0b10 then raise(Malformed_exn(8,[b0;b1;b2;b3])) else (c,j+l)
      	end
       )
  else raise(Bad_starter(b0,l));;

let decode l=
   let n=List.length l in
   if n=0 then [] else
   let rec tempf=(fun (accu,j)->
     if j>=n
     then List.rev(accu)
     else let (c,new_j)=pusher_for_decoding (l,j) in
          tempf(c::accu,new_j)
   ) in
   tempf([],0);;

let encode u =
    if u <= 0x007F 
    then [u] 
    else 
    if u <= 0x07FF 
    then[(0xC0 lor (u lsr 6)); (0x80 lor (u land 0x3F))]
    else 
    if u <= 0xFFFF 
    then  [ (0xE0 lor (u lsr 12));
            (0x80 lor ((u lsr 6) land 0x3F));
            (0x80 lor (u land 0x3F))]
    else
          [ (0xF0 lor (u lsr 18));
            (0x80 lor ((u lsr 12) land 0x3F));
            (0x80 lor ((u lsr 6) land 0x3F));
            (0x80 lor (u land 0x3F)) ];;

let encode_as_string u=
  let temp1=encode u in
  let d=List.length(temp1) in
  let b=Bytes.create d in
  let _=(for i=0 to (d-1) do Bytes.set b i (Char.chr (List.nth temp1 i)) done) in
  Bytes.to_string b;;
  
            
            
let unicode_point s=
   let n=String.length(s) in
   let temp1=Ennig.doyle (fun j->Char.code(String.get s j)) 0 (n-1) in
   Printf.sprintf "%X" (fst(pusher_for_decoding (temp1,0)));;             
            
let decompose s=
   let n=String.length(s) in
   let temp1=Ennig.doyle (fun j->Char.code(String.get s j)) 0 (n-1) in
   let temp2=decode temp1 in
   Image.image encode_as_string temp2;;           
            
            
            
            











