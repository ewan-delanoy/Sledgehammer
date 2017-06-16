let iter0 (f:'a->unit) l addenda=
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^addenda^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout)
               done;;


let iter1 (f:'a->'a) initial_value a_priori_size addenda=
  let accu=ref(initial_value) in
  let s0=" of "^string_of_int(a_priori_size)^" "^addenda^"\n" in
  let _=(for j=1 to a_priori_size
               do
               ( 
                 accu:=f(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout;
               )
               done) in
  !accu;;

let iter2 (f:'a->'a) initial_value tester (shower:'a->string)  addenda=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
                 print_string((shower (!accu))^addenda);
                 flush stdout;
               )
               done) in
  !accu;;

let unverbose_iter (f:'a->'a) initial_value tester=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
               )
               done) in
  !accu;;

let iter f l=iter0 f l "";;

  
let iter_on_ennig f a b=
  let n=b-a+1 in
  let s0=" of "^string_of_int(n)^"\n" in
  for j=1 to n
               do
               (f(a-1+j);print_string(string_of_int(j)^s0);flush stdout)
               done;;

let e_rev l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(" (rev part)") in
   !accu;;  

 let unchronometered_filter f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(" (filter part)") in
   e_rev(!accu);;    
 
 let filter f l=Chronometer.it (unchronometered_filter f) l;; 
   
 let unchronometered_image f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(" (rev_image part)") in
   e_rev(!accu);;  
   
 let image f l=Chronometer.it (unchronometered_image f) l;;  
 
 let unchronometered_image_computed_backwards f l=
   let temp1=e_rev(l) in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(" (image part)") in
   (!accu);;     
 
  let image_computed_backwards f l=Chronometer.it 
   	(unchronometered_image_computed_backwards f) l;;  
 
 let unchronometered_map_on_cartesian_product f l1 l2=
   let iterator=(
      fun (graet,x0,y_da_ober,x_da_ober,y_klok)->
        if y_da_ober<>[]
        then let y0=List.hd(y_da_ober) and peurrest_y=List.tl(y_da_ober) in
             let opt=f(x0,y0) in
             if opt=None
             then (graet,x0,peurrest_y,x_da_ober,y_klok) 
             else ((Option.unpack opt)::graet,x0,peurrest_y,x_da_ober,y_klok)
        else
        if x_da_ober=[]
        then (graet,x0,y_da_ober,x_da_ober,y_klok)
        else
        let x1=List.hd(x_da_ober) and peurrest_x=List.tl(x_da_ober) in
        (graet,x1,y_klok,peurrest_x,y_klok)
   )
   and n=(List.length l1)*((List.length l2)+1)-1
   and initial_value=([],List.hd l1,l2,List.tl l1,l2) in
   let (graet,_,_,_,_)=iter1 iterator initial_value n " (cartesian part)" in
   e_rev graet;;
   
 let map_on_cartesian_product f l1 l2=Chronometer.it (unchronometered_map_on_cartesian_product f l1) l2;;
 
 let unchronometered_find_it f l=
    let g=(fun (graet,da_ober)->
       match da_ober with
       []->(graet,[])
       |a::peurrest->
         if f(a)
         then (Some a,[])
         else (None,peurrest)
    ) in
    fst(iter1 g (None,l) (List.length l) " (finder part)");;
 
 let find_it f l=Chronometer.it (unchronometered_find_it f) l;;  
 
 let unchronometered_explore_tree f l=
    let g=(fun (graet,p,q,da_ober)->
       match da_ober with
       []->(graet,0,0,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,p-1,q+1,peurrest)
         else (a::graet,p-1+List.length(temp1),q+1,temp1@peurrest)
    ) and 
    tester=(fun (graet,p,q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (graet,p,q,da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;

let pusher_in_leaf_finding f (graet,p,q,da_ober)=
  let a=List.hd(da_ober) and peurrest=List.tl(da_ober) in
  let temp1=f a in
  let new_walker=(fun ()->
    if temp1=[]
    then (a::graet,p+1,q+List.length(temp1)-1,peurrest) 
    else (graet,p+1,q+List.length(temp1)-1,temp1@peurrest) 
  )()
  in
  let (_,new_p,new_q,_)=new_walker in   
  let s=(string_of_int new_q)^" to be explored ; "^
       (string_of_int new_p)^" found\n" in  
  let _=(print_string s;flush stdout) in
      new_walker;;
 
let rec unchronometered_find_leaves 
  (f:('a -> 'a list)) 
  (g: ('a list * int * int * 'a list -> unit)) 
  (watcher,accu_ref)=
   let walker=(!accu_ref) in
   let (_,_,q,_)=walker in
   if (q>0)&&(Sys.file_exists watcher)
   then let new_walker=pusher_in_leaf_finding f walker in
        let _=(accu_ref:=new_walker) in
        unchronometered_find_leaves f g (watcher,accu_ref)
   else let _=g walker in walker;;
  
 
 let unchronometered_unverbose_explore_tree f l=
    let g=(fun (graet,da_ober)->
       match da_ober with
       []->(graet,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,peurrest)
         else (a::graet,temp1@peurrest)
    ) and 
    tester=(fun (graet,da_ober)->da_ober<>[]) 
    in
    let initial_value=([],l) in
    let (ans,_)=unverbose_iter g initial_value tester  in
    ans;;
 
 let explore_tree f l=Chronometer.it (unchronometered_explore_tree f) l;;  
 
 let find_leaves f g x=Chronometer.it (unchronometered_find_leaves f g) x;;  
 
 let unchronometered_explore_tree_with_stop checker f l=
    let g=(fun (p,q,da_ober,optional)->
       match da_ober with
       []->(0,0,[],None)
       |a::peurrest->
         let temp1=f a in
         let temp2=Option.find_it checker temp1 in
         if temp2<>None
         then (p+1,0,[],temp2)
         else 
         if temp1=[]
         then (p-1,q+1,peurrest,None)
         else (p-1+List.length(temp1),q+1,temp1@peurrest,None)
    ) and 
    tester=(fun (p,q,da_ober,optional)->(da_ober<>[])&&(optional<>None)) 
    and
    shower=(
     fun (p,q,da_ober,optional)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=(List.length l,0,l,None) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (_,_,_,ans)=iter2 g initial_value tester shower  "" in
    ans;;
    
 let unchronometered_unverbose_explore_tree_with_stop checker f l=
    let g=(fun (da_ober,optional)->
       match da_ober with
       []->([],None)
       |a::peurrest->
         let temp1=f a in
         let temp2=Option.find_it checker temp1 in
         if temp2<>None
         then ([],temp2)
         else 
         if temp1=[]
         then (peurrest,None)
         else (temp1@peurrest,None)
    ) and 
    tester=(fun (da_ober,optional)->(da_ober<>[])&&(optional<>None)) 
    in
    let initial_value=(l,None) in
    let (_,ans)=unverbose_iter g initial_value tester  in
    ans;;
       
    
 
 let explore_tree_and_stop f l=Chronometer.it 
 (unchronometered_explore_tree_with_stop f) l;;  
 
 type verbose_bool=bool;;
 type stopped_bool=bool;;
 
 let explore_tree_with_options checker translator f l 
  (verbose:verbose_bool)
   (stopped:stopped_bool)=
   if stopped
   then let opt=
        (fun bowl->
          if bowl
          then unchronometered_explore_tree_with_stop 
                   checker f l
          else unchronometered_unverbose_explore_tree_with_stop 
                   checker f l
        )(verbose) in
        if opt=None then "" else
        "\n\n\n"^(Option.unpack opt)^"\n\n\n"
   else let temp1=
        (fun bowl->
          if bowl
          then unchronometered_explore_tree f l
          else unchronometered_unverbose_explore_tree f l
        )(verbose) in
        let temp2=Image.image translator temp1 in 
        "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";;     
 
 
 let hard_big_concat ll=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(ll)(" (rev part before concatenating)") in
   let temp1=(!accu) in
   let last_one=List.hd(temp1) and others=List.tl(temp1) in
   let accu2=ref(last_one) in 
   let g=(fun x->accu2:=x@(!accu2)) in
   let _=iter0(g)(others)(" (concatenating)") in
   !accu2;;
   
 let partition f l=
  let hanterenn1=ref([])
  and hanterenn2=ref([]) in
  let h=(function x->
    if f(x)
    then hanterenn1:=x::(!hanterenn1)
    else hanterenn2:=x::(!hanterenn2)
  ) in
  let _=iter(h)(l) in
 (!hanterenn1,!hanterenn2);;



type 'a tester=('a -> bool);;
type ('a,'b) displayer=('a->'b);;
type 'a hammer=('a->'a);;

let morzholan 
  (tester:'a tester) 
  (displayer:('a,'b) displayer) 
  (hammer:'a hammer)
  (x0:'a)=
    let rec tempf=(
     fun (j,x)->
        if tester(x)
        then displayer(x)
        else
          let xx=hammer(x) in
          let _=(print_string("Iteration number "^string_of_int(j)^" done \n");flush stdout) in 
          tempf(j+1,xx)
    ) in
    tempf(1,x0);;
    
  

