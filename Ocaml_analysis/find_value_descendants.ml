(*

#use"Ocaml_analysis/find_value_descendants.ml";;

*)

let fvd all_ocaml_items s=
   let j1=String.index(s)('.')+1 in
   let module_name=Cull_string.beginning (j1-1) s in
   let (native_ones,foreign_ones)=
   List.partition (
      fun itm->
        let s1=Ocaml_gsyntax_item.name itm in
        let j=String.index(s1)('.')+1 in
        (Cull_string.beginning (j-1) s1)=module_name
   ) all_ocaml_items in
   let native_descendants=
   Option.filter_and_unpack(
     fun itm->
        let current_namespace=Longest_shared_module.lsm 
           s (Ocaml_gsyntax_item.name itm) in
        let l=(if current_namespace="" 
               then 0 
               else 1+(String.length current_namespace)) in   
        let shortened_name=Cull_string.cobeginning l s in
        if Isolated_occurrences.of_in shortened_name (Ocaml_gsyntax_item.content itm)<>[]
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) native_ones 
   and foreign_descendants=
   Option.filter_and_unpack(
     fun itm->
        if Isolated_occurrences.of_in s (Ocaml_gsyntax_item.content itm)<>[]
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) foreign_ones in
   (native_descendants,foreign_descendants);;
   
