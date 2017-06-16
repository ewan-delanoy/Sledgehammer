(*******************************************************************************)

(* 
  
   
 Checks if a list of   Binary_constraint.t elements covers all possible cases.
 

*)

(*******************************************************************************)

  
let rec find_forgotten_case l=
  if l=[] then Some(Binary_constraint.no_constraint) else
  let temp1=Image.image Binary_constraint.support l in
  let temp2=Tidel.big_teuzin temp1 in
  if Tidel.length temp2=0 then None else
  let x1=Tidel.hd(temp2) in
  let (bbl1,bl_between)=List.partition (fun b->
    let (n,_)=Binary_constraint.unveil b in
    Tidel.elfenn x1 n
  ) l in
  let bl1=Image.image (Binary_constraint.forget x1) bbl1 in
  let (bbl2,bl3)=List.partition (fun b->
    let (_,e)=Binary_constraint.unveil b in
    Tidel.elfenn x1 e
  ) bl_between in
  let bl2=Image.image (Binary_constraint.forget x1) bbl2 in
  let opt1=find_forgotten_case(bl1@bl3) in
  if opt1<>None
  then Some(Binary_constraint.push_element_outside x1 (Option.unpack opt1))
  else 
  let opt2=find_forgotten_case(bl2@bl3) in
  if opt2<>None
  then Some(Binary_constraint.push_element_inside x1 (Option.unpack opt2))
  else 
  None;;
  
 let check l=((find_forgotten_case l)=None);; 
