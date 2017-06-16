(*

#use"gather_preprinter_result.ml";;


*)


let max_line_length=ref 100;;
let max_num_lines=ref 12;;

let gather_preprinter_result l=
  let rec helper=( fun 
   (graet,line_count,s_between,da_ober)->
     match da_ober with
     []->(if s_between=""
          then graet
          else graet^"\n"^s_between)
     |s_a::peurrest->
        if (String.length(s_between)+String.length(s_a)>(!max_line_length))&&(s_between<>"")
        then (
              if line_count>=(!max_num_lines)
              then graet^"..."
              else helper(graet^"\n"^s_between,line_count+1,s_a,peurrest)
             )
        else helper(graet,line_count,s_between^s_a,peurrest)     
  ) in
  helper("",0,"",l);;

let printer_of_preprinter (prpr: 'a Preprinter.t)=
  (fun
  (dummy:Format.formatter) x->
   Format.open_box 0;
   Format.print_string(gather_preprinter_result (prpr x));
   Format.close_box()
   );;