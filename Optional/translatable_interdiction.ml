
type int_set = int Tidel.set;;

type t=Cond of (int Tidel.set)*int*string;;



(* Cond(A,m,c) represents the set of all X''s such that |A \cap X| \leq m *)
  


let unveil (Cond(a,m,anv))=((a:int_set),m,anv);;

let find_forbidden_subset ((Ordered.S lz):int_set) (Cond(Ordered.S lforb,m,anv))=
     if lz=[] then None else
     let z=Ordered.S lz in
     let az=List.hd(lz) and bz=List.hd(List.rev lz) in
     let intv=List.rev(Ennig.ennig (az-1) (bz-1)) in
     let tempf=(
     	fun t->
     	   let translated_forb=Image.image (fun k->k+t) lforb in
     	   if Tidel.length(Tidel.kengeij (Ordered.S translated_forb) z)>m
     	   then (Some(Ordered.S translated_forb):int_set option)
     	   else None
     ) in
     Option.find_and_stop tempf intv;;
  
  let find_forbidden_subset_from_list z lt=
    Option.find_and_stop (find_forbidden_subset z) lt;;
  
 let check t z=(find_forbidden_subset z t=None);;
  
 let big_check lt z=(find_forbidden_subset_from_list z lt=None);;

let watched_product lt (l1:int_set list) (l2:int_set list)=
       let temp1=Cartesian.product l1 l2 in
       let temp2=Option.filter_and_unpack(fun (x,y)->
           let z=Tidel.teuzin x y in
           if big_check lt z
           then Some z
           else None
       ) temp1 in
       let temp3=Ordered.forget_order(Ordered_bare_set.safe_set2 temp2) in
       (temp3:int_set list);;
       
let explicit_watched_product lt (l1:int_set list) (l2:int_set list)=
       let tempf=(fun (x,y)->
           let z=Tidel.teuzin x y in
           if big_check lt z
           then Some z
           else None
       )  in
       let temp1=Explicit.map_on_cartesian_product tempf l1 l2 in
       let temp2=Ordered.forget_order(Ordered_bare_set.safe_set2 temp1) in
       (temp2:int_set list);;       

let explicit_watched_big_product lt ll=
  List.fold_left (explicit_watched_product lt) [Tidel.empty_set] ll;;

let fill_iterator lt ((graet:int_set),da_ober)=match da_ober with
   []->(graet,[])
  |w::peurrest->
    let trial=Tidel.insert w graet in
    if big_check lt trial
    then ((trial:int_set),peurrest)
    else ((graet:int_set),peurrest);;
    
let fill lt c=
    let (ans,_)=Listennou.morzholan (fill_iterator lt) c in
    ans;;
  

let remainder lt n=
   let tempf=(fun
     (Cond(Ordered.S lx,m,anv))->
        let temp1=List.rev lx in
        let b=List.hd temp1 and temp2=List.tl temp1 in
        let temp3=List.rev_map (fun i->i+n-b) temp2 in
        (((Ordered.S temp3):int_set),m-1)
   ) in
   Image.image tempf lt;;
   
let remainder_check l z=List.for_all (fun (a,m)->
     Tidel.length(Tidel.kengeij a z)<=m
) l;;   


let name_for_set (x:int_set)=
   let lx=Ordered.forget_order x in
   let a=List.hd(lx)
   and b=List.hd(List.rev lx) in
   if lx=Ennig.ennig a b
   then let d=b-a+1 in
        ("["^(string_of_int d)^"]",Ordered.S(Ennig.ennig 1 d))
   else 
   let better_lx=Image.image (fun i->i-a+1) lx in
   let temp1=Image.image string_of_int better_lx in
   let temp2="{"^(String.concat ";" temp1)^"}" in
   (temp2,((Ordered.S better_lx):int_set));;




let make (x:int_set) m=
 if (m<0)
 then failwith("A cardinality is always nonnegative")
 else if Tidel.length(x)<=m
      then failwith("This is not a determination")
      else let (sx,better_x)=name_for_set x in
          Cond(x,m,sx^"\226\137\188"^(string_of_int m));;


let print (Cond(x,m,anv))=anv;;

 let print_out (dummy:Format.formatter) (c)=
   Format.open_box 0;
   Format.print_string(print c);
   Format.close_box();;






              
                            
