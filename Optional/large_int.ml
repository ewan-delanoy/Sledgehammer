(*******************************************************************************)

(*
   
   This is just some syntactic sugar to have shorter names
   for the Big_int module functions, and also to have a more 
   visible type.
   
   #use "large_int.ml";;

*)

(*******************************************************************************)

type t=L of Big_int.big_int;;

let zero=L(Big_int.zero_big_int);;
let one=L(Big_int.unit_big_int);;

let minus (L x)=L(Big_int.minus_big_int x);;
let abs (L x)=L(Big_int.abs_big_int x);;
let succ (L x)=L(Big_int.succ_big_int x);;
let pred (L x)=L(Big_int.pred_big_int x);;
let add (L x) (L y)=L(Big_int.add_big_int x y);;
let sub (L x) (L y)=L(Big_int.sub_big_int x y);;
let mult (L x) (L y)=L(Big_int.mult_big_int x y);;
let square u=mult u u;;
let cube u=mult u (square u);;
let sqrt (L x)=L(Big_int.sqrt_big_int x);;
let div (L x) (L y)=L(Big_int.div_big_int x y);;
let modd (L x) (L y)=L(Big_int.mod_big_int x y);;
let gcd (L x) (L y)=L(Big_int.gcd_big_int x y);;
let power (L x) k=L(Big_int.power_big_int_positive_int x k);;

let sign (L x)=Big_int.sign_big_int x;;
let opposite x=sub zero x;;
let compare (L x) (L y)=Big_int.compare_big_int x y;;

let cmp=((fun x y->let d=compare x y in
   if d<0 then Total_ordering.Lower else
   if d>0 then Total_ordering.Greater else
   Total_ordering.Equal):> t Total_ordering.t);;

let eq (L x) (L y)=Big_int.eq_big_int x y;;
let leq (L x) (L y)=Big_int.le_big_int x y;;
let geq (L x) (L y)=Big_int.ge_big_int x y;;
let lt (L x) (L y)=Big_int.lt_big_int x y;;
let gt (L x) (L y)=Big_int.gt_big_int x y;;

let min x y=if lt y x then y else x;;
let max x y=if lt x y then y else x;;

let abs x=max x (opposite x);;

let to_string (L x)=Big_int.string_of_big_int x;;
let of_string s=L(Big_int.big_int_of_string s);;

let to_int (L x)=Big_int.int_of_big_int x;;
let of_int s=L(Big_int.big_int_of_int s);;

 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(to_string x);
   Format.close_box();;
 
