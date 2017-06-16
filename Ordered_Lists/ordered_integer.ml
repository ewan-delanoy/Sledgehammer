(* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type set=int Ordered.old_set;;
let cmp=(Total_ordering.standard:>(int Total_ordering.t));;let lt x y=x<y;;
let unsafe_set=(Ordered.unsafe_set:>(int list-> set));;
let forget_order=(Ordered.forget_order:>(set->int list));;

let kreskus_strizh x=Ordered.kreskus_strizh cmp x;;
let kreskus x=Ordered.kreskus cmp x;;

let elfenn=((fun a ox->Ordered.elfenn cmp a ox):>(int->set->bool));;
let teuzin=((fun ox oy->Ordered.teuzin cmp ox oy):>( set->set->set));;
let diforchan=((fun x->Ordered.diforchan cmp x):>(int list->set));;
let lemel=((fun ox oy->Ordered.lemel cmp ox oy):>(set->set->set));;
let ental=((fun ox oy->Ordered.ental cmp ox oy):>(set->set->bool));;
let kengeij=((fun ox oy->Ordered.kengeij cmp ox oy):>set->set->set);;
let kengeij_goullo=((fun ox oy->Ordered.kengeij_goullo cmp ox oy):>set->set->bool);;
let min=((fun x->Ordered.min cmp x):>int list->int);;
let max=((fun x->Ordered.max cmp x):>int list->int);;

let hd ox=List.hd(forget_order ox);;
let image f ox=Image.image f (forget_order ox);;
let rev_map f ox=Image.image f (forget_order ox);;
let empty_set=unsafe_set [];;
let singleton x=unsafe_set [x];;
let filter f x=unsafe_set(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set u,unsafe_set v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(elfenn a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else diforchan(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=teuzin(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left teuzin empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Ordered.expand_boolean_algebra cmp x):>(set list->(set list)));; 
 
 
