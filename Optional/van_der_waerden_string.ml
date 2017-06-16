




type fundamental_parameter=int;;

type vdw_string_size=int;;

type t=S of fundamental_parameter*string;;

let unveil (S(r,s))=(r,s);;

let content (S(r,s))=s;;

let length (S(r,s))=String.length s;;

let pairs_to_be_checked (r:fundamental_parameter) (n: vdw_string_size) i=
   let mr=min r in
   let temp1=Ennig.doyle(fun t->(i-2*t,i-t))(1)(mr((i-1)/2))
   and temp2=Ennig.doyle(fun t->(i-t,i+t))(1)(mr(min(i-1)(n-i)))
   and temp3=Ennig.doyle(fun t->(i+t,i+2*t))(1)(mr((n-i)/2))
   in
   temp1@temp2@temp3;;
   
   

let check_pair s (i,j)=
  (*we assume that s has been correctly constructed, so (si,sj)<>(1,1) *)
  let s_copy=Bytes.of_string(s) in
  if (String.get s (i-1)='1')&&((String.get s (j-1))<>'0')
  then let _=Bytes.set s_copy (j-1) '0' in
       s_copy
  else 
  if (String.get s (j-1)='1')&&((String.get s (i-1))<>'0')
  then let _=Bytes.set s_copy (i-1) '0' in
       s_copy
  else s_copy;;
  

  
let check_pairs s l=
 if l=[] then Bytes.of_string s else
 List.fold_left   check_pair s l;;


let left_watchers (r:fundamental_parameter) s m=
  let n=String.length(s) in
  let temp1=Ennig.doyle(fun t->(m+t,m+2*t))(max(1)(1-m))(min(r)((n-m)/2)) in
  Image.image (fun (i,j)->(String.get s (i-1),String.get s (j-1)) ) temp1;; 

let general_left_extensiblity_test(r:fundamental_parameter) s m=
   List.for_all (fun (si,sj)->
      (si,sj)<>('1','1')
   ) (left_watchers r s m);;


let right_watchers (r:fundamental_parameter) s m=
  let n=String.length(s) in
  let temp1=Ennig.doyle(fun t->(m-2*t,m-t))(max(1)(m-n))(min(r)((m-1)/2)) in
  Image.image (fun (i,j)->(String.get s (i-1),String.get s (j-1)) ) temp1;; 

let general_right_extensiblity_test(r:fundamental_parameter) s m=
   List.for_all (fun (si,sj)->
      (si,sj)<>('1','1')
   ) (right_watchers r s m);;

let is_extensible (r:fundamental_parameter) s=
  general_right_extensiblity_test r s (String.length(s)+1);;

let measure_extensibility (r:fundamental_parameter) s=
  let rec tempf=(fun (k,da_ober)->match da_ober with
      []->k
      |(a,b)::peurrest->
         if (a,b)=('1','1') then 1 else
         if ((a,b)=('1','F'))||
            ((a,b)=('F','1')) then tempf(2,peurrest) else
         tempf(k,peurrest)   
  ) in
  tempf(3,right_watchers r s (String.length(s)+1));;
 
let all_extensions (S(r,s))=
   match measure_extensibility r s with
    1->[S(r,s^"0")]
   |2->[S(r,s^"0");S(r,s^"F")]
   |3->[S(r,s^"0");S(r,s^"1");S(r,s^"F")]
   |arall->failwith("Im a backdoor man");;
  
let create (r:fundamental_parameter) (n: vdw_string_size)=S(r,String.make n 'F');;

let set_to_white vdw_s j=
  let (S(r,s))=vdw_s in
  let n=String.length(s) in
  if (j<1)||(j>n) then vdw_s else
  let old_c=String.get s (j-1) in
  if old_c='0' 
  then let _=(print_string ("\nForbidden move : s["^(string_of_int j)^"]<-1 for s="^s^"\n");
        flush stdout) in
        vdw_s
  else 
  if old_c='1' 
  then let _=(print_string ("\nRedundant move : s["^(string_of_int j)^"]<-1 for s="^s^"\n");
        flush stdout) in
        vdw_s
  else 
  let temp1=pairs_to_be_checked r n j in
  let new_s=check_pairs s temp1 in
  let _=Bytes.set new_s (j-1) '1' in
  S(r,new_s);;
  
let set_to_black vdw_s j=  
  let (S(r,s))=vdw_s in
  let n=String.length(s) in
  if (j<1)||(j>n) then vdw_s else
  let old_c=String.get s (j-1) in
  if old_c='1' 
  then let _=(print_string ("\nForbidden move : s["^(string_of_int j)^"]<-0 for s="^s^"\n");
        flush stdout) in
        vdw_s
  else 
  if old_c='0' 
  then let _=(print_string ("\nRedundant move : s["^(string_of_int j)^"]<-0 for s="^s^"\n");
        flush stdout) in
        vdw_s
  else 
  let new_s=Bytes.of_string s in
  let _=Bytes.set new_s (j-1) '0' in
  S(r,new_s);;  
  
let make_move vdw_s (j,c)=
  if c=0 then set_to_black vdw_s j else
  if c=1 then set_to_white vdw_s j else
  vdw_s;;
  
  
let first_free_cell (S(r,s))= 
   let n=String.length(s) in
   Option.find_it 
   (fun j->let c=String.get(s)(j-1) in (c<>'0')&&(c<>'1')) 
   (Ennig.ennig 1 n);;

let rec fill_greedily vdw_s=
  match first_free_cell vdw_s with
  None->vdw_s
  |Some(j0)->
     let new_vdw_s=set_to_white vdw_s j0 in
     fill_greedily new_vdw_s;;

let uncurried_sphere =
  Memoized.recursive(fun old_f ((r:fundamental_parameter),n)->
   if n<1 then [S(r,"")] else
   let temp1=old_f(r,n-1) in
   let temp2=Image.image all_extensions temp1 in
   List.flatten temp2
  );;
     
let sphere (r:fundamental_parameter) n=uncurried_sphere(r,n);;
  
let uncurried_ball =
  Memoized.make(fun ((r:fundamental_parameter),n)->
   let temp1=Ennig.doyle (sphere r) 0 n in
   List.flatten temp1
  );;

let ball (r:fundamental_parameter) n=uncurried_ball(r,n);;


 let hard_measure (S(r,s))= 
   let n=String.length(s) in
   List.length(List.filter (fun j->String.get s (j-1)='1') (Ennig.ennig 1 n));;

let duracell_decomposition vdw_s=
 let (S(r,s))=vdw_s in
  match first_free_cell vdw_s with
  None->(s,S(r,""))
  |Some(j0)->(Cull_string.beginning (j0-1) s,S(r,Cull_string.cobeginning (j0-1) s) );;

let checks_in_generic_construction (r:fundamental_parameter) s ones accu=
  let n=String.length s in
  let tempf1=(fun (i,j)->
     let t=j-i in
     let x=i-t in
     if (t>r)||(x<1) then () else
     let c=String.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^sx^"],s["^si^"],s["^sj^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  )
  and tempf2=(fun (i,j)->
     let t=j-i in
     let x=j+t in
     if (t>r)||(x>n) then () else
     let c=String.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^si^"],s["^sj^"],s["^sx^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  )
   and tempf3=(fun (i,j)->
     let d=j-i in
     if (d mod 2)>0 then () else
     let t=d/2 in
     let x=i+t in
     if (t>r) then () else
     let c=String.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^si^"],s["^sx^"],s["^sj^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  ) in
  (List.iter tempf1 ones;List.iter tempf2 ones;List.iter tempf3 ones);;
  
let construct (r:fundamental_parameter) s=
  let n=String.length(s) in
  let temp0=List.filter(fun j->String.get s (j-1)='1')(Ennig.ennig 1 n)
  and accu=ref([]) in
  let ones=Uple.list_of_pairs temp0 in
  let _=checks_in_generic_construction r s ones accu in
  let corrections=Ordered.forget_order(Tidel2.diforchan(!accu)) in
  if corrections=[] then S(r,s) else 
  let _=List.iter(fun (x,(i,j))->Bytes.set s (x-1) '0') corrections in
  let temp1=Image.image(fun (x,(i,j))->
    (string_of_int x,string_of_int i,string_of_int j)
  )(corrections) in
  let temp2=Image.image (fun (sx,si,sj)->
    "s["^sx^"]<-0, because of s["^si^"] and s["^sj^"]."
  ) temp1 in
  let temp3="\n\n\n Some corrections : \n\n"^(String.concat "\n" temp2)^"\n\n\n" in
  let _=(print_string temp3;flush stdout) in
  S(r,s);;
  
let construct_discreetly0 (r:fundamental_parameter) s=
  let n=String.length(s) in
  let temp0=List.filter(fun j->String.get s (j-1)='1')(Ennig.ennig 1 n)
  and accu=ref([]) in
  let ones=Uple.list_of_pairs temp0 in
  let _=checks_in_generic_construction r s ones accu in
  let corrections=Ordered.forget_order(Tidel2.diforchan(!accu)) in
  if corrections=[] then Some(s) else 
  let s_copy=Bytes.of_string s in
  let _=List.iter(fun (x,(i,j))->Bytes.set s_copy (x-1) '0') corrections in
  Some s_copy;;
  
let construct_discreetly (r:fundamental_parameter) s=
  try construct_discreetly0 r s with _->
  None;;
   
   
   
   
let concat a (S(r,b))=
  match construct_discreetly r (a^b) with
  None->None
  |Some(w)->if Substring.begins_with w a
                  then Some(S(r,w))
                  else None;;
 
let add_blanks_on_the_right (S(r,s)) b=
  let n=String.length(s) in
  let temp1=List.filter (fun j->not(general_right_extensiblity_test r s (n+j))) (Ennig.ennig 1 b) in
  let rightmost_part=String.make b 'F' in
  let _=List.iter (fun j->Bytes.set rightmost_part (j-1) '0') temp1 in
  S(r,s^rightmost_part);;



let add_blanks_repeatedly_on_the_right (S(r,s)) b=
  let n=String.length(s) in
  let temp1=List.filter (fun j->not(general_right_extensiblity_test r s (n+j))) (Ennig.ennig 1 b) in
  let rightmost_part=String.make b 'F' in
  let _=List.iter (fun j->Bytes.set rightmost_part (j-1) '0') temp1 in
  Ennig.doyle (fun j->S(r,s^(Cull_string.beginning j rightmost_part))) 1 b;;


let add_blanks_on_the_left (S(r,s)) b=
  let temp1=List.filter (fun j->not(general_left_extensiblity_test r s (1-j))) (Ennig.ennig 1 b) in
  let leftmost_part=String.make b 'F' in
  let _=List.iter (fun j->Strung.set leftmost_part (b-j+1) '0') temp1 in
  S(r,leftmost_part^s);;



let add_blanks_repeatedly_on_the_left (S(r,s)) b=
  let temp1=List.filter (fun j->not(general_left_extensiblity_test r s (1-j))) (Ennig.ennig 1 b) in
  let leftmost_part=String.make b 'F' in
  let _=List.iter (fun j->Strung.set leftmost_part (b-j+1) '0') temp1 in
  Ennig.doyle (fun j->S(r,(Cull_string.ending j leftmost_part)^s )) 1 b;;


let resize_on_the_left (S(r,s))=
  let n=String.length(s) and intended_n=2*r in
  let d=intended_n-n in
  if (d<0) then S(r,Cull_string.ending intended_n s) else
  if (d=0) then S(r,s) else
  add_blanks_on_the_left (S(r,s)) d;;

let is_weakly_admissible (r:fundamental_parameter) s=
   let n=String.length(s) in
   let temp1=List.filter (fun i->Strung.get s i='1') (Ennig.ennig 1 n) in
   let temp2=Uple.list_of_triples temp1 in
   List.for_all 
   (fun (x,y,z)->
      let d=y-x in (z-y<>d)||(d>r)
   )
   temp2;;
   
let is_strongly_admissible (r:fundamental_parameter) s=
   match construct_discreetly r s with
   None->false
   |Some(t)->t=s;;
   
let strongly_admissible_extensions (r:fundamental_parameter) l=
   let temp1=Cartesian.product l ["0";"1";"F"] in
   let temp2=Image.image (fun (x,y)->x^y) temp1 in
   let temp3=List.filter (is_strongly_admissible r) temp2 in
   temp3;;
   

let pair_is_weakly_concatable (S(r1,s1)) (S(r2,s2))=
  if r1<>r2
  then failwith("No concat possible")
  else is_weakly_admissible r1 (s1^s2);;


let solve_naively=Memoized.recursive(fun old_f vdw_s->
   match first_free_cell vdw_s with
   None->(hard_measure vdw_s,[vdw_s])
   |Some(j0)->
      let vdw_s1=set_to_black vdw_s j0
      and vdw_s2=set_to_white vdw_s j0 in
      let (m1,l1)=old_f(vdw_s1)
      and (m2,l2)=old_f(vdw_s2) in
      if m1<m2 then (m2,l2) else
      if m2<m1 then (m1,l1) else
      (m1,l1@l2)
);;


let all_realizations=Memoized.recursive(fun old_f vdw_s->
   match first_free_cell vdw_s with
   None->[vdw_s]
   |Some(j0)->
      let vdw_s1=set_to_black vdw_s j0
      and vdw_s2=set_to_white vdw_s j0 in
      let l1=old_f(vdw_s1)
      and l2=old_f(vdw_s2) in
      l1@l2
);;


let graded_realization=Memoized.make(fun (vdw_s,j)->
   let p=fst(solve_naively vdw_s)
   and temp1=all_realizations vdw_s in
   List.filter (fun t->hard_measure t=p-j) temp1
);;

let direct_decompositions=Memoized.make(fun (S(r,s))->
   let h=(fun t->fst(solve_naively(S(r,t))) ) 
   and n=String.length(s) in
   let c0=h(s) in
   let tester=(fun j->
     let beg=Cull_string.beginning j s
     and cobeg=Cull_string.cobeginning j s in
     h(beg)+h(cobeg)=c0
   ) in
   List.filter tester (Ennig.ennig 1 (n-1))
);;


let shadow_on_the_left (r:fundamental_parameter) p s=
   if String.length(s)<p then failwith("string too short") else
   let s1=Option.unpack(construct_discreetly(r)(s)) in
   let temp1=S(r,s1) in
   let temp2=snd(solve_naively(temp1)) in
   let temp3=Image.image (fun vdw_s->Cull_string.beginning p (content vdw_s)) temp2 in
   let temp4=Ordered_string.diforchan temp3 in
   temp4;;
   
type string_set_set=string Ordered_bare_set.set2;;   
   
let dances_for_spectator_on_the_right (r:fundamental_parameter) s l=
   let srs=S(r,s) and p=String.length(s) in
   let tempf=(fun w->
     if pair_is_weakly_concatable srs (S(r,w))
     then Some(shadow_on_the_left r p (s^w))
     else None
   ) in
   let temp1=Option.filter_and_unpack tempf l in
   let temp2=Ordered_bare_set.diforchan temp1 in
   (temp2:string_set_set);;


 let print_out (dummy:Format.formatter) (S(n,x))=
   Format.open_box 0;
   Format.print_string(x);
   Format.close_box();;

  
