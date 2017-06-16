type t=Var of string;;

let of_string x=Var(x);;
let unveil (Var x)=x;;

let cmp=((fun (Var v1) (Var v2)->
  let n1=String.length(v1)
  and n2=String.length(v2) in
 if n1=n2 
 then Tidel.cmp v1 v2 
 else Tidel.cmp n1 n2):t Total_ordering.t);;
 
 let eq (Var s1) (Var s2)=(s1=s2);;
 
 let dummy=Var"";;
 
 let specific_name i=Var("yyy"^string_of_int(i));;
 
      
  
 (* let basic i=Var("a"^(string_of_int i));; *)
 
 let basic i=Var("a_{"^(string_of_int i)^"}");; 
 
 let latex_alpha (Var w)=
   let m=String.length(w)-1 in
   "\\alpha"^(String.sub w 1 m);;
 
 let dual_of_variable (Var(w))=
  let m=String.length(w)-1 in
  if ((String.get w m)=='*')
  then (Var(String.sub w 0 m))
  else (Var (w^"*"));;
  
 let is_a_toggler (Var w)=
  let n=String.length(w) in
  if n<5 
  then false 
  else ((String.sub w 0 4)="ccc<")&&((String.get w (n-1))='>');;
  
 let toggle (Var w)=
  if is_a_toggler(Var w)
  then Var(String.sub w 4 ((String.length w)-5))
  else Var("ccc<"^w^">");;  
   
  
 let toggle (Var w)=
  let n=String.length(w) in
  if n<5 then Var("ccc<"^w^">") else
  if ((String.sub w 0 4)="ccc<")&&((String.get w (n-1))='>')
  then Var(String.sub w 4 (n-5))
  else Var("ccc<"^w^">");;
  
 let ocaml_name v=
 (*cut the names in two to avoid problems with 
     the dependency computer *)
 if v=dummy then "Variabl"^"e.dummy" else 
 match v with 
 (Var x)->
 "Variabl"^"e.of_string(\""^(String.escaped x)^"\")";;
 
 
