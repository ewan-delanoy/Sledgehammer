(* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type 'a set2=('a Tidel.set) Ordered.old_set;;
let lt (u:'a Tidel.set) (v:'a Tidel.set)=
let lu=Tidel.length(u) and lv=Tidel.length(v) in
if lu=lv then u<v else lu<lv;;
let cmp=((fun x y->
		if lt(x)(y) then Total_ordering.Lower else
		if lt(y)(x) then Total_ordering.Greater else
		Total_ordering.Equal): ('a Tidel.set) Total_ordering.t);;


let unsafe_set2=(Ordered.unsafe_set:>(('a Tidel.set) list-> 'a set2));;
let forget_order=(Ordered.forget_order:>('a set2->('a Tidel.set) list));;

let kreskus_strizh x=Ordered.kreskus_strizh cmp x;;
let kreskus x=Ordered.kreskus cmp x;;

let elfenn=((fun a ox->Ordered.elfenn cmp a ox):>(('a Tidel.set)->'a set2->bool));;
let teuzin=((fun ox oy->Ordered.teuzin cmp ox oy):>( 'a set2->'a set2->'a set2));;
let diforchan=((fun x->Ordered.diforchan cmp x):>(('a Tidel.set) list->'a set2));;
let lemel=((fun ox oy->Ordered.lemel cmp ox oy):>('a set2->'a set2->'a set2));;
let ental=((fun ox oy->Ordered.ental cmp ox oy):>('a set2->'a set2->bool));;
let kengeij=((fun ox oy->Ordered.kengeij cmp ox oy):>'a set2->'a set2->'a set2);;
let kengeij_goullo=((fun ox oy->Ordered.kengeij_goullo cmp ox oy):>'a set2->'a set2->bool);;
let min=((fun x->Ordered.min cmp x):>('a Tidel.set) list->('a Tidel.set));;
let max=((fun x->Ordered.max cmp x):>('a Tidel.set) list->('a Tidel.set));;

let hd ox=List.hd(forget_order ox);;
let image f ox=Image.image f (forget_order ox);;
let rev_map f ox=Image.image f (forget_order ox);;
let empty_set2=unsafe_set2 [];;
let singleton x=unsafe_set2 [x];;
let filter f x=unsafe_set2(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set2 u,unsafe_set2 v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(elfenn a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set2 x=if kreskus_strizh(x) then unsafe_set2(x) else diforchan(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=teuzin(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left teuzin empty_set2 x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Ordered.expand_boolean_algebra cmp x):>('a set2 list->('a set2 list)));; 
 
 
