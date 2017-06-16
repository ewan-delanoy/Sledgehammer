(*All the implementations are naive and not optimized. Completely useless for large matrices. *)

type t={numOfLines:int; numOfCols:int; core:Rational.t array};;

let unveil x=(x.numOfLines,x.numOfCols,x.core);;

let number_of_lines x=x.numOfLines;;
let number_of_columns x=x.numOfCols;;

let get x i j=let p=(i-1)*x.numOfCols+(j-1) in Array.get x.core p;;

let set x i j v=let p=(i-1)*x.numOfCols+(j-1) in Array.set x.core p v;;

let make nl nc=
  let arr=Array.make(nl*nc)(Rational.zero) in
  {numOfLines=nl;numOfCols=nc;core=arr};;

let constructor nl nc f=
  let arr=Array.make(nl*nc)(Rational.zero) in
  let _=(for k=0 to (nl*nc)-1
         do 
            let i=(k/nc)+1 in
            let j=k-(nc*(i-1))+1 in
            Array.set arr k (f i j)
         done) in
   {numOfLines=nl;numOfCols=nc;core=arr};;
   
 let simple lambda=constructor 1 1 (fun i j->lambda);;  
 
 let of_column l=
   let n=List.length(l) in
   constructor n 1 (fun i j->List.nth l (i-1));;
 
 let of_line l=
   let n=List.length(l) in
   constructor 1 n (fun i j->List.nth l (j-1));;
   
 let with_columns lc=
   let p=List.length(List.hd lc)
   and q=List.length(lc) in
   constructor p q (fun i j->let z=List.nth lc (j-1) in List.nth z (i-1));;
 
  let with_lines ll=
   let p=List.length(ll)
   and q=List.length(List.hd ll) in
   constructor p q (fun i j->let z=List.nth ll (i-1) in List.nth z (j-1));;
   
 let with_int_columns old_lc=
   let lc=Image.image (Image.image Rational.of_int) old_lc in
   with_columns lc;;
   
  let with_int_lines old_lc=
   let lc=Image.image (Image.image Rational.of_int) old_lc in
   with_lines lc;;  
   
  let diagonal l=
     let n=List.length(l) in
     constructor n n (fun i j->
     if i=j 
     then List.nth(l)(i-1) 
     else Rational.zero);; 
   
   
 let dot lambda x=
    let tempf=(fun i j->Rational.mult lambda (get x i j)) in
    constructor x.numOfLines x.numOfCols tempf;;
   
let add mat1 mat2=
  let nl=mat1.numOfLines and nc=mat1.numOfCols in
  if (nl<>mat2.numOfLines)||(nc<>mat2.numOfCols)
  then failwith("Matrices cannot be added : their dimensions are not identical.")
  else  let tempf=(fun i j->Rational.add (get mat1 i j) (get mat2 i j) ) in
        constructor nl nc tempf;;

let sub mat1 mat2=
  let nl=mat1.numOfLines and nc=mat1.numOfCols in
  if (nl<>mat2.numOfLines)||(nc<>mat2.numOfCols)
  then failwith("Matrices cannot be added : their dimensions are not identical.")
  else  let tempf=(fun i j->Rational.sub (get mat1 i j) (get mat2 i j) ) in
        constructor nl nc tempf;;
        
 let mult mat1 mat2=
  let p=mat1.numOfLines and q1=mat1.numOfCols 
  and q2=mat2.numOfLines and r=mat2.numOfCols  in
  if (q1<>q2)
  then failwith("Matrices cannot be multiplied : their dimensions are not compatible.")
  else  let tempf=(fun i k->
           let ttemp1=Ennig.doyle(fun j->Rational.mult(get mat1 i j)(get mat2 j k) )(1)(q1) in
            Rational.big_sum ttemp1 ) in
        constructor p r tempf;;
               

let transpose mat=
  constructor mat.numOfCols mat.numOfLines (fun i j->get mat j i);;
  
let cofactor mat i0 j0=
  let eraser=(fun i x->if x<i then x else x+1) in
  constructor (mat.numOfCols-1) (mat.numOfLines-1) 
      (fun i j->get mat (eraser i0 i) (eraser j0 j));;
      
let rec unsafe_determinant mat=
  (*here we assume that mat is a square matrix *)
  let n=mat.numOfCols in
  if n<2 then get mat 1 1 else
  let tempf=(fun i->
    let d=Rational.mult(get mat 1 i)(unsafe_determinant(cofactor mat 1 i)) in
    if (i mod 2=0) then Rational.opposite d else d
  ) in
  Rational.big_sum(Ennig.doyle tempf 1 n);;

let determinant mat=
  if mat.numOfCols<>mat.numOfLines
  then failwith("Determinant undefined for a non-square matrix.")
  else unsafe_determinant mat;;

let near_inverse mat=
  (*we assume that mat is square and has dimension >1. *)
 let tempf=(fun i j->
    let d=unsafe_determinant(cofactor mat j i) in
    if ((i+j) mod 2=0) then d else Rational.opposite d
 ) in
 constructor mat.numOfLines mat.numOfCols tempf;;

let inverse mat=
  let n=mat.numOfLines in
  if mat.numOfCols<>n
  then failwith("Inverse undefined for a non-square matrix.")
  else if n=1 then simple(Rational.inverse(get mat 1 1)) else
       let temp1=near_inverse(mat) in
       let d=Rational.big_sum(Ennig.doyle(fun i->Rational.mult(get mat 1 i)(get temp1 i 1))(1)(n)) in
       if d=Rational.zero
       then failwith("Non-invertible matrix")
       else dot (Rational.inverse d) temp1;;

let div mat1 mat2=mult(mat1)(inverse mat2);;


let submatrix mat ii jj=
  let tempf=(fun i j->
     let other_i=List.nth ii (i-1)
     and other_j=List.nth jj (j-1) in
     get mat other_i other_j
  ) in
  constructor (List.length ii) (List.length jj) tempf;;



let nonzero_coefficient mat=
  let p=number_of_lines mat
  and q=number_of_columns mat in
  let rec tempf=(fun (i,j)->
     if i<1 then None else
     if Rational.is_nonzero (get mat i j)
     then Some(i,j)
     else
     if (i,j)=(1,1) then None else
     if j=1 then tempf(i-1,q) else
     tempf(i,j-1)
     ) in
  tempf(p,q);;   


let is_zero mat=((nonzero_coefficient mat)=None);;

 let eq mat1 mat2=
     is_zero(sub mat1 mat2);;

let rec write_as_linear_combination mat0 l_mat=
   (* We assume that the matrices all have the same size *)
   match l_mat with
   []->if is_zero mat0
       then Some[]
       else None
   |v1::peurrest->
      let pack=nonzero_coefficient v1 in
      if pack=None
      then Option.propagate 
          	(fun l->(Rational.zero)::l) 
      		(write_as_linear_combination mat0 peurrest)
      else 
      let (i0,j0)=Option.unpack pack in
      let pivot=get v1 i0 j0 in
      let projector=(
        fun v->
          let c=get v i0 j0 in
          let tempm1=dot (Rational.div c pivot) v1 in
          sub v tempm1
      ) in
      let mat1=projector mat0
      and temp1=Image.image projector peurrest in
      match write_as_linear_combination mat1 temp1 with
      None->None
      |Some(l)->
         let temp2=List.combine l peurrest in
         let temp3=Image.image
         	(fun (lamb,v)->
           		let c=get v i0 j0 in
          		Rational.mult(Rational.div c pivot) lamb
         	) temp2 in
         let s=Rational.big_sum temp3 in
         let temp4=Rational.div (get mat0 i0 j0) pivot in
         let lamb1=Rational.sub temp4 s in
         Some(lamb1::l);;
   
   
let subkernel_in_image l linear_form =
   let eval_form=(fun v->let tempm=mult linear_form v in get tempm 1 1) in
   let temp1=Ennig.index_everything l in
   let temp2=List.rev_map (fun (j,v)->(j,v,eval_form v)) temp1 in
   let pack=Option.find_it(fun (j,v,r)->Rational.is_nonzero r) temp2 in
   if pack=None then l else
   let (j0,v0,pivot)=Option.unpack pack in
   let projector=(
        fun v->
          let c=eval_form v in
          let tempm1=dot (Rational.div c pivot) v0 in
          sub v tempm1
      ) in
   let tempf=(
     fun (j,v)->
       if j>j0 then Some(v) else 
       if j=j0 then None else
       Some(projector v)
     ) in
    Option.filter_and_unpack tempf temp1;; 
   
 let subkernel_deep_in_image l linear_forms =
    List.fold_left subkernel_in_image l linear_forms;;
  

 let lines_inside mat=
     let p=number_of_lines mat
     and q=number_of_columns mat in 
     let lq=Ennig.ennig 1 q in
     Ennig.doyle (fun i->submatrix mat [i] lq) 1 p;;
  
 let columns_inside mat=
     let p=number_of_lines mat
     and q=number_of_columns mat in 
     let lp=Ennig.ennig 1 p in
     Ennig.doyle (fun j->submatrix mat lp [j]) 1 q;;  
  
let canonical_base_for_back_space mat=
    let q=number_of_columns mat in 
    let tempf1=(fun j->
       with_int_columns([Ennig.doyle (fun i->if i=j then 1 else 0) 1 q])
    ) in
    let temp1=Ennig.doyle tempf1 1 q in
    temp1;;

let gauss_operation mat=
   let g=Rational.gauss_multiplier (Array.to_list(mat.core)) in
   dot g mat;;

let kernel mat=
   let temp1=
   subkernel_deep_in_image 
     (canonical_base_for_back_space mat)
       (List.rev(lines_inside mat)) in
    Image.image gauss_operation temp1;;

let find_solution_to_matrix_equation aa bb=
  (* finds a solution x to aa*x=bb, if it exits *)
  let columns_of_aa=columns_inside aa
  and columns_of_bb=columns_inside bb in
  let temp2=Image.image (fun y->
     write_as_linear_combination y columns_of_aa
  ) columns_of_bb in
  if List.exists(fun x->x=None) temp2
  then None
  else
  let temp3=Image.image Option.unpack temp2 in
  Some(with_columns temp3);;
  
  

let print x=
  let nl=x.numOfLines and nc=x.numOfCols in
  let tempf1=Memoized.make(fun (i,j)->Rational.print(get x i j) ) in
  let tempf2=Memoized.make(fun j->
     Max.list(Ennig.doyle(fun i->String.length(tempf1 (i,j) ))(1)(nl)) ) in
  let tempf3=(fun i j->
      let s=tempf1(i,j) and m=tempf2(j) in
      let list_of_spaces=Ennig.doyle(fun y->" ")(1)(m-(String.length s)) in
      let completer=String.concat("")(list_of_spaces) in
      completer^s
  )   in
  let tempf4=(fun i->" ["^(String.concat("|")( Ennig.doyle(tempf3 i) 1 nc))^"]") in
  "\n\n"^(String.concat("\n")( Ennig.doyle tempf4 1 nl))^"\n";;
  
 let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;

  
