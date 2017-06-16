
   let modularize initial_name=
      let s_initial_name=Absolute_path.to_string initial_name in
      let i1=String.rindex ("/"^s_initial_name) '/' in
      let son=(fun _->
         if i1=0 then s_initial_name else
         (Cull_string.cobeginning i1 s_initial_name)
         ) () in
      let i2=String.index (son^".") '.' in
      let son_without_points=(fun _->
         if i2=(String.length son) then son else
         (Cull_string.beginning i2 son)
         ) () in    
     let content=Io.read_whole_file(initial_name) in
     let first_letter=(String.sub son 0 1) in
     let usual_case=
        List.mem(first_letter)
        [
          "a";"b";"c";"d";"e";
          "f";"g";"h";"i";"j";
          "k";"l";"m";"n";"o";
          "p";"q";"r";"s";"t";
          "u";"v";"w";"x";"y";
          "z"
        ] 
     in   
     let new_name=(fun _->
        if usual_case
        then (String.capitalize_ascii(first_letter))^(Cull_string.cobeginning 1 son_without_points)
        else "M"^son_without_points
     )() in
     let new_content=
       "\n\nmodule "^new_name^"=struct\n\n"^content^"\n\nend;"^";\n\n" in
     let new_filename="Optional/modularized_"^son in 
     (new_filename,new_content);;
     
     
    
 
