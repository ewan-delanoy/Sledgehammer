(*

#use"pretty_print_intervals_and_felines.ml"

A feline interval is an arithmetic progression.

*)


 let interval l=match l with
 []->"{}"
 |a::peurrest->
     if peurrest=[]
     then ("{"^(string_of_int a)^"}")
     else  let b=List.hd(List.rev peurrest) in
          if (l=Ennig.ennig(a)(b))&&(b-a>1) 
          then ("{"^string_of_int(a)^".."^string_of_int(b)^"}")
          else
          let temp1=Image.image(string_of_int)(l) in
          "{"^String.concat(",")(temp1)^"}";;

 let feline_interval (i,r,l)=
   if r=1 then interval([i;i+l-1]) else
   (interval [i;i+(l-1)*r])^"(by "^(string_of_int r)^")";;
   

 







   
