
let one_more_time f (an_holl,da_ober)=
 let l_da_ober=Tidel.forget_order(da_ober) in
 let temp1=List.flatten(List.rev_map(f)(l_da_ober)) in
 let temp2=Tidel.diforchan(temp1) in
 let re_nevez=Tidel.lemel(temp2)(an_holl) in
 let hollad_nevez=Tidel.teuzin(an_holl)(re_nevez) in
 (hollad_nevez,re_nevez);; 
  
let rec morzholan f (an_holl,da_ober)=
  if da_ober=Tidel.empty_set
  then an_holl
  else morzholan f (one_more_time f (an_holl,da_ober));;
  
let father f l=let tl=Tidel.diforchan(l) in morzholan f (tl,tl);;

let one_more_time2 f (an_holl,graet,da_ober)=
 let l_graet=Tidel.forget_order(graet) 
 and l_da_ober=Tidel.forget_order(da_ober) in
 let zz1=Cartesian.product(l_graet)(l_da_ober)
 and zz2=Cartesian.product(l_da_ober)(l_graet) 
 and zz3=Cartesian.product(l_da_ober)(l_da_ober) in
 let zz=List.flatten [zz1;zz2;zz3] in
 let temp1=List.flatten(List.rev_map (function (x,y)->[f x y]) zz ) in
 let temp2=Tidel.diforchan(temp1) in
 let re_nevez=Tidel.lemel(temp2)(an_holl) in
 let hollad_nevez=Tidel.teuzin(an_holl)(re_nevez) in
 (hollad_nevez,an_holl,re_nevez);; 
  
let rec morzholan2 f (an_holl,graet,da_ober)=
  if da_ober=Tidel.empty_set
  then an_holl
  else morzholan2 f (one_more_time2 f (an_holl,graet,da_ober));; 
  
let binary_operation f l=let tl=Tidel.diforchan(l) in morzholan2 f (tl,Tidel.empty_set,tl);;  

let explore_tree f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
 let explore_tree_explicitly f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  let _=(print_string("("^string_of_int(ia)^","^string_of_int(j+1)^")\n");
                  flush stdout) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
type 'a hierarchize_data=
   Normal of  'a Tidel.set list * 'a Tidel.set * ('a * 'a Tidel.set) list
  |Failed of  'a Tidel.set list * 'a Tidel.set * ('a * 'a Tidel.set) list
  |Succeeded of 'a list list;;
   

let pusher_for_hierarchization (graet,hollad,da_ober)=
  if da_ober=[]
  then Succeeded(List.rev_map Ordered.forget_order graet)
  else 
  let temp1=Image.image (fun (x,y)->(x,Tidel.lemel y hollad)) da_ober in
  let (temp2,temp3)=List.partition (fun (x,z)->Tidel.length z=0) temp1 in
  if temp2=[]
  then Failed(graet,hollad,da_ober)
  else
  let temp4=Image.image fst temp2 in
  let o_temp4=Tidel.diforchan(temp4) in
  let temp5=Image.image (fun (x,z)->(x,Tidel.lemel z o_temp4)) temp3 in
  (Normal(o_temp4::graet,Tidel.teuzin hollad o_temp4,temp5));;
  
type 'a list_of_ancestors_map=('a -> 'a list);;  
  
let try_hierarchizing (f: 'a list_of_ancestors_map) l=
  let temp1=Image.image (fun t->(t,Tidel.diforchan(f t))) l in
  let rec tempf=(fun x->
    let y=pusher_for_hierarchization x in
    match y  with
    Normal(a,b,c)->tempf(a,b,c)
    |_->y) in
  tempf ([],Tidel.empty_set,temp1);;
  
let hierarchize f l=
  match try_hierarchizing f l with
   Succeeded(l)->l
  |_->failwith("Direct hierarchizing fails, there is a cycle. Try hierarchize instead");;
  
  

