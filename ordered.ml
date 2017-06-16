(*
    hardcore version :
    
type 'a old_set=S of 'a list;;
let unsafe_set x=S(x);;
let forget_order (S x)=x;;


*)
(*
    soft version :
    
type 'a old_set='a list;;
let unsafe_set=((fun x->x):>('a list->'a old_set));;
let forget_order=((fun x->x):>('a old_set->'a list));;


*)

type 'a old_set=S of 'a list;;

let unsafe_set x=S(x);;
let forget_order (S x)=x;;

let eq (S x) (S y)=(x=y);;
let neq (S x) (S y)=(x<>y);;

let kreskus (kenver:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)<>Total_ordering.Greater)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  
let kreskus_strizh (kenver:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)=Total_ordering.Lower)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;

let rec elfenn (kenver:'a Total_ordering.t) x ol=
   let rec elfenn0=(function
    []->false
    |a::peurrest->match kenver(x)(a) with
       Total_ordering.Lower->false
       |Total_ordering.Equal->true
       |Total_ordering.Greater->elfenn0 peurrest
   )  in
   elfenn0 (forget_order ol);;
		
            
let teuzin (kenver:'a Total_ordering.t) ox oy=
let rec teuzin0=
(function (u,v,accu)->
if u=[] then (List.rev_append(accu)(v)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->teuzin0(yu,v,xu::accu)
   |Total_ordering.Equal->teuzin0(yu,yv,xu::accu)
   |Total_ordering.Greater->teuzin0(u,yv,xv::accu)

) in
unsafe_set(teuzin0(forget_order ox,forget_order oy,[]));;

let rec diforchan (kenver:'a Total_ordering.t) x=
  if List.length(x)<2
  then unsafe_set(x)
  else let temp1=Listennou.didrochan(x) in
       let y1=diforchan(kenver)(fst temp1)
       and y2=diforchan(kenver)(snd temp1) in
       teuzin kenver y1 y2;;
  
  
  let lemel (kenver:'a Total_ordering.t) ox oy=
let rec lemel0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->lemel0(yu,v,xu::accu)
   |Total_ordering.Equal->lemel0(yu,yv,accu)
   |Total_ordering.Greater->lemel0(u,yv,accu)

) in
unsafe_set(lemel0(forget_order ox,forget_order oy,[]));;

let kengeij (kenver:'a Total_ordering.t) ox oy=
let rec kengeij0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev(accu)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->kengeij0(yu,v,accu)
   |Total_ordering.Equal->kengeij0(yu,yv,xu::accu)
   |Total_ordering.Greater->kengeij0(u,yv,accu)

) in
unsafe_set(kengeij0(forget_order ox,forget_order oy,[]));;

let kengeij_goullo (kenver:'a Total_ordering.t) ox oy=
let rec kengeij_goullo0=
(function (u,v)->
if (u=[])||(v=[]) then true else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->kengeij_goullo0(yu,v)
   |Total_ordering.Equal->false
   |Total_ordering.Greater->kengeij_goullo0(u,yv)
) in
kengeij_goullo0(forget_order ox,forget_order oy);;


let ental (kenver:'a Total_ordering.t) ox oy=
let rec ental0=
(function (u,v)->
if u=[] then true else
if v=[] then false else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->false
   |Total_ordering.Equal->ental0(yu,yv)
   |Total_ordering.Greater->ental0(u,yv)
) in
ental0(forget_order ox,forget_order oy);;

let min=((fun kenver x->match x with
  []->failwith("The empty set has no min")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Total_ordering.Lower
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let max=((fun kenver x->match x with
  []->failwith("The empty set has no max")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Total_ordering.Greater
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let cooperation_for_two kenver x y=
  List.filter (fun ox->
    forget_order ox<>[]
  ) [kengeij kenver x y;lemel kenver x y;lemel kenver y x];;
  
let expand_boolean_algebra kenver l=
  if List.length(l)<2 then l else
  let rec tempf=(fun 
    (graet,y0,etre,da_ober)->
      if etre=[] 
      then if da_ober=[]
           then y0::graet
           else let z0=List.hd(da_ober) and peurrest=List.tl(da_ober) in
                tempf([],z0,y0::graet,peurrest)
      else 
      let x0=List.hd(etre) and peurrest_etre=List.tl(etre) in
      let t1=kengeij kenver x0 y0 and t2=lemel kenver x0 y0 in
      let y1=lemel kenver y0 t1 in
      let temp1=List.filter (fun ox->forget_order ox<>[]) [t1;t2] in
      tempf(List.rev_append temp1 graet,y1,peurrest_etre,da_ober)
  )  in
  let x1=List.hd(l) and r1=List.tl(l) in
  tempf([],x1,[],r1);;

let length ox=List.length(forget_order ox);;
let image f ox=Image.image(f)(forget_order ox);;
let insert kenver x oy=teuzin kenver (unsafe_set [x])(oy);;
let exists f ox=List.exists f (forget_order ox);;
let safe_set kenver ox=if kreskus(kenver)(ox) 
                       then unsafe_set ox 
                       else diforchan kenver ox;;
let rev_map f ox=List.rev_map(f)(forget_order ox);;
let filter f ox=unsafe_set(List.filter(f)(forget_order ox));;
let for_all f ox=List.for_all(f)(forget_order ox);;
let singleton x=unsafe_set [x];;
let empty_set=unsafe_set [];;
let big_teuzin kenver x=List.fold_left (teuzin kenver) empty_set x;;
let big_kengeij kenver=function
   []->failwith("empty intersection undefined")
  |a::b->List.fold_left(kengeij kenver)(a)(b);;
let nelfenn kenver a ox=not(elfenn kenver a ox);;
let nental kenver a ox=not(ental kenver a ox);;
let eq ox oy=(forget_order ox)=(forget_order oy);;
  

