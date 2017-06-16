
type 'a t=Cond of ('a Tidel.set)*int*string;;

type int_ref=int ref;;
type int_set=int Tidel.set;;
type int_t=int t;;

(* Cond(A,m,c) represents the set of all X''s such that |A \cap X| \leq m *)
  


let unveil (Cond(a,m,anv))=(a,m,anv);;

let check (Cond(a,m,anv)) x=(Tidel.length(Tidel.kengeij a x)<=m);;

let big_check lc x=List.for_all (fun cond->check cond x) lc;;


let redundancy (Cond(a1,m1,anv1)) (Cond(a2,m2,anv2))=
Tidel.length(Tidel.lemel a2 a1)<=m2-m1;;

let compare (Cond(a1,m1,anv1)) (Cond(a2,m2,anv2))=
 if a1=a2
 then m1<m2
 else Ordered_bare_set.lt a1 a2;;
 
let total_ordering=
  let temp1=(fun x y->if x=y then Total_ordering.Equal else
                      if compare(x)(y) then Total_ordering.Lower else
                      Total_ordering.Greater) in
  (temp1:('a t) Total_ordering.t);;
 
let unsafe_constructor (x,m,anv)=Cond(x,m,anv);; 

let incisive_value (Cond(x,m,anv))=
   if (Tidel.length x)=1 then Some(Tidel.hd(x)) else None;; 

let make x m anv=
 if (m<0)
 then failwith("A cardinality is always nonnegative")
 else if Tidel.length(x)<=m
      then failwith("This is not a determination")
      else Cond(x,m,anv);;

let name_counter =(ref (1):int_ref);;

let make_with_default_name x m=
   let name=("w_"^(string_of_int (!name_counter)) ) in
   let _=(name_counter:=(!name_counter)+1) in
   make x m name;;

let make_with_telltale_name (x:int_set) m=((make x m 
   ("a_"^(Pretty_print_intervals_and_felines.interval (Tidel.forget_order x)))):int_t);;

let delete_redundancies l=Listennou.delete_redundancies redundancy l;; 



let complete_name (Cond(x,m,anv))=anv;; 


let print c=
 match c with (Cond(x,m,anv))->
 complete_name(c)^"\226\137\188"^(string_of_int m);;

 let print_out (dummy:Format.formatter) (c)=
   Format.open_box 0;
   Format.print_string(print c);
   Format.close_box();;






              
                            
