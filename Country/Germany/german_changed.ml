
(* 


#use"Country/Germany/german_changed.ml";;



*)

let update r_changed changed=
  let r_chan=Ordered_string.safe_set r_changed
  and   chan=Ordered_string.safe_set (Recently_changed.to_string_list changed) in
  let whole=Ordered_string.teuzin r_chan chan in
  Recently_changed.of_string_list(Ordered.forget_order whole);;

