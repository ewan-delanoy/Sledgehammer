(*

#use"Php_analizer/php_check_recognizer_rules.ml";;

If everything is OK, all the components of they_should_be_empty are []

*)

let hi=List.length;;
let ofo=Ordered.forget_order;;

let part1=Image.image 
  Php_token.projected_version Php_token.fixture_of_nonconstants;;

let z1=Image.image (fun cst->Php_token.Constant(cst))
    Php_constant_token.all ;;
let part2=Image.image  Php_token.projected_version z1;;

let part3=Image.image fst Php_atomic_selector.special_list;;

let part4=Image.image fst Php_short_selector.new_constants;;

let part5=
   ofo(Tidel.diforchan(
   (Image.image fst Php_constructible_recognizer.all_pairs)
   @
   (Image.image snd Php_constructible_recognizer.all_pairs)
   ));;
   

let labelled_parts=[
   "nonconst",part1;
   "const",part2;
   "atomic",part3;
   "short",part4;
   "recognizer",part5;
];;

let parts=Image.image snd labelled_parts;;

let u1=Image.image (fun x->(x,ofo(Tidel.diforchan(x))) ) parts;;
let check1=List.filter (fun (x,y)->hi(x)<>hi(y)) u1;;
let u2=Image.image snd u1;;
let labelled_u2=Image.image (fun ((x,y),z)->(x,z) ) (List.combine labelled_parts u2);;
let whole=ofo(Tidel.diforchan(List.flatten u2));;
let u3=Image.image (
  fun w->
   (w,Option.filter_and_unpack (
     fun (j,l)->
       if List.mem w l
       then Some(j)
       else None
   ) labelled_u2)
) whole;;
let check2=List.filter (fun (w,l)->List.length(l)>1) u3;;

let they_should_be_empty=(check1,check2);;



