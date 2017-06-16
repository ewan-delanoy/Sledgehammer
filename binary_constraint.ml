(*******************************************************************************)

(* 
 Here we study constraints on set of integers.
 The basic building block is [i in X ] or [i not in X].
 In the object Possible(n,e) (as below) represents the set of X s such that X is disjoint
 from n and contains e.

*)

(*******************************************************************************)

type elements_outside=int Tidel.set;;
type elements_inside=int Tidel.set;;
type int_set=int Tidel.set;;


type list_of_elements_outside=int list;;
type list_of_elements_inside=int list;;

type t= 
   Impossible
   |Possible of (elements_outside*elements_inside) ;;
   
 let unveil=((function
   Impossible->(Tidel.singleton 0,Tidel.singleton 0)
   |Possible(n,e)->(n,e)):>t -> (elements_outside*elements_inside) );;
  
 let inclusion cx cy=
   if cx=Impossible then true else
   if cy=Impossible then false else
   let (n1,e1)=unveil(cx) and (n2,e2)=unveil(cy) in
   (Tidel.ental n2 n1)&&(Tidel.ental e2 e1);;
   
 let containment cy cx=inclusion cx cy;;  
 

 let usual_constructor ((x:elements_outside),(y:elements_inside))=
    if Tidel.kengeij_goullo(x)(y)
    then Possible(x,y)
    else Impossible;;
    
 let handy_constructor ((x:list_of_elements_outside),(y:list_of_elements_inside))=
   usual_constructor(Tidel.safe_set x,Tidel.safe_set y);;
 
 let no_constraint=Possible(Tidel.empty_set,Tidel.empty_set);;
 

 let intersection cx cy=
   if (cx=Impossible)||(cy=Impossible) then Impossible else
   let (n1,e1)=unveil(cx) and (n2,e2)=unveil(cy) in
   let new_n=Tidel.teuzin n1 n2 
   and new_e=Tidel.teuzin e1 e2 in
   usual_constructor(new_n,new_e);;
   
 let push_element_inside i x=match x with
   Impossible->Impossible
   |Possible(n,e)->
       if Tidel.elfenn i n
       then Impossible
       else Possible(n,Tidel.insert i e);;
       
  let push_element_outside i x=match x with
   Impossible->Impossible
   |Possible(n,e)->
       if Tidel.elfenn i e
       then Impossible
       else Possible(Tidel.insert i n,e);;    
       
  let big_intersection l=List.fold_left 
     intersection no_constraint l;;
     
 let check (z:int_set)=function
    Impossible->false
    |Possible(n,e)->(Tidel.kengeij_goullo n z)&&(Tidel.ental e z);;
    
 let support=((function
    Impossible->Tidel.empty_set
    |Possible(n,e)->Tidel.teuzin n e):t -> int_set);;
    
 let forget x=function
     Impossible->Impossible
    |Possible(n,e)->Possible(Tidel.outsert x n,Tidel.outsert x e);;
       
 let translate x t=match x with
    Impossible->Impossible
    |Possible(n,e)->
       let tr=(fun z->Tidel.unsafe_set(Tidel.image (fun i->i+t) z)) in
      Possible(tr n,tr e);;
       
  let print=function
    Impossible->"\226\152\185"
    |Possible(n,e)->
       let temp1=Tidel.image string_of_int n
       and temp2=Tidel.image string_of_int e 
       in
       let s1=String.concat (",") temp1
       and s2=String.concat (",") temp2
       in
       "\226\142\168"^(s1^"|"^s2)^"\226\142\172";;
       
       
    
       
       
  let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;
        
       
       
       
       
