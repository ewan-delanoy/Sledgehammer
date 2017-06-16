(*

#use"Php_analizer/termite_reverse_parse.ml";;

Useful to debug the Termite.parse function.

*)


let rec iterator_for_reverse_parsing x=
  let y=Termite.pusher_for_parsing x in
  if snd(y)<>None
  then fst x
  else iterator_for_reverse_parsing y;;

let rp (Termite.Trmt(trmt)) l=iterator_for_reverse_parsing (([],trmt,[],l),None) ;;

     
 
