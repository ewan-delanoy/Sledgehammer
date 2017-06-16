type 'a t= ('a->'a->bool);;


let select_minimal_elements (kenver:'a t) ll=
(*we suppose that ll is pre-ordered with respect to kenver *)
let rec select_minimal_elements0=(function graet->(function
[]->List.rev(graet)
|a::da_ober->select_minimal_elements0(a::graet)(List.filter(function b->not(kenver a b) )(da_ober)))) in
select_minimal_elements0 [] ll;;

let select_maximal_elements (kenver:'a t) ll=
(*we suppose that ll is pre-ordered with respect to kenver *)
let rec select_maximal_elements0=(function graet->(function
[]->List.rev(graet)
|a::da_ober->select_maximal_elements0(a::graet)(List.filter(function b->not(kenver b a) )(da_ober)))) in
select_maximal_elements0 [] (List.rev ll);;

let select_minimal_elements_carefully (kenver:'a t) ll=
(*we do not suppose that ll is pre-ordered with respect to kenver *)
let rec select_minimal_elements0=(fun graet l->match l with
[]->List.rev(graet)
|a::da_ober->
 if List.exists(function c->kenver c a)(da_ober)
 then select_minimal_elements0(graet)(da_ober)
 else let temp=List.filter(function b->not(kenver a b) )(da_ober) in
      select_minimal_elements0(a::graet)(temp)) in
 select_minimal_elements0([])(ll);;

let select_maximal_elements_carefully (kenver:'a t) ll=
(*we do not suppose that ll is pre-ordered with respect to kenver *)
let rec select_maximal_elements0=(fun graet l->match l with
[]->List.rev(graet)
|a::da_ober->
 if List.exists(function c->kenver a c)(da_ober)
 then select_maximal_elements0(graet)(da_ober)
 else let temp=List.filter(function b->not(kenver b a) )(da_ober) in
      select_maximal_elements0(a::graet)(temp)) in
 select_maximal_elements0([])(ll);;
