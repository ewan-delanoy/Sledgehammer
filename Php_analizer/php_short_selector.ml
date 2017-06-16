(*

#use"Php_analizer/php_short_selector.ml";;

*)

type t =                                                                    
         Atomic of Php_atomic_selector.t
        |Block of Php_blocker.t;;
      
let new_constants=
   [
     "()",Block(Php_blocker.Parenthesis);
     "{}",Block(Php_blocker.Brace);
     "[]",Block(Php_blocker.Bracket);
   ];;

let is_constant=function
   Atomic(atom_sel)->Php_atomic_selector.is_constant atom_sel
  |Block(_)->false;;

let all_constants=
   let temp1=
   (
     Image.image (fun (s,ato)->
        (s,Atomic(ato))
     ) Php_atomic_selector.all_constants
   )
   @
   new_constants in
   let temp2=Image.image (fun (s,ato)->(-(String.length s),(s,ato)) ) temp1 in
   let temp3=Tidel2.diforchan temp2 in
   Tidel2.image snd temp3;;

let all_string_constants=Image.image fst all_constants;;

let list_from_string s=
  let temp1=Strung.longest_match_parsing all_string_constants s in
  Image.image (fun a->List.assoc a all_constants) temp1;;

exception Unregistered of t;; 
 
let to_string x=try (fst(Option.find_really (fun p->snd(p)=x) all_constants)) 
      with 
      _->raise(Unregistered(x));;

exception Unknown of string;;

let optional_of_string s0=match 
   Option.find_it (fun (s,sel)->s=s0) all_constants with
   None->None
   |Some(_,sel)->Some(sel);;
   
let of_string s=match optional_of_string s with
   None->raise(Unknown(s))
  |Some(s)->s;;
  

let recognize_atom atom_sel=
   let f=(function x->
      if x=Positioned_php_token_list.empty then None else
      let (a,peurrest)=Positioned_php_token_list.ht x in
        if Php_atomic_selector.test atom_sel (Positioned_php_token.fst a) 
        then let (u,v)=Positioned_php_token.snd a in
             Some(Php_char_range.make u v,peurrest)
        else None
   ) in
   (f : Php_recognizer.t);;   

let recognize sel=
   let f=(function
      l->(
        match sel with
         Atomic(atom_sel)->recognize_atom atom_sel l
        |Block(blckr)->Php_recognize_block.recognize_block blckr l
      )
   ) in
   (f : Php_recognizer.t);; 
   


(*

let gg x=
   let y=of_string x in
   let z=to_string y in
   let u=of_string z in
   (y,y=u);;

gg "if new instanceof kwd variable";;
gg "  if new instanceof kwd variable";;
gg "deny  if new instanceof kwd variable";;


*)


