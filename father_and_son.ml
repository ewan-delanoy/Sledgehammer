(*

#use"father_and_son.ml";;

The son is invasive by default.

*)

let father_and_son s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then ("",s)
   else (String.sub s 0 i,String.sub s (i+1) ((String.length s)-i-1) );;

let father s c=fst(father_and_son s c);;
let son s c=snd(father_and_son s c);;

let invasive_father s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then s
   else String.sub s 0 i;;
    
  