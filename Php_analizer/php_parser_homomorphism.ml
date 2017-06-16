(*

#use"Php_analizer/php_parser_homomorphism.ml";;

Aka combinators.

*)


let star prsr=
  let rec tempf=(fun (graet,cr,z)->
      match Php_parser.parse prsr z with
       None->Some(List.rev graet,cr,z)
      |Some(res,cr2,z2)->tempf(res::graet,cr2,z2)
  ) in
  let tempg=(fun l->
     match tempf([],Php_char_range.dummy,l) with
     None->None
     |Some(res,cr3,z3)->
        if res=[] then Some(res,cr3,z3) else
        let first_lxng=fst(Positioned_php_token.snd(Positioned_php_token_list.hd(l))) 
        and last_lxng=Php_char_range.snd cr3 in
        Some(res,Php_char_range.make first_lxng last_lxng,z3)
  ) in
  (tempg: 'a list Php_parser.t);;

