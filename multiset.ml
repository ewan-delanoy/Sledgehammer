
type 'a t=M of ('a*int) list;; 

let to_list (M l)=Image.image(fst)(l);;

let filter_odd_multiplicities (M l)=
   let temp1=List.filter(function (x,j)->(j mod 2)=1)(l) in
   Image.image(fst)(temp1);;
            
let length (M l)=Basic.big_sum(Image.image snd l);;            
            
let teuzin (M x) (M y)=
let rec teuzin0=
(function (u,v,accu)->
if u=[] then List.rev_append(accu)(v) else
if v=[] then List.rev_append(accu)(u) else
let pu=List.hd(u) and lu=List.tl(u) 
and pv=List.hd(v) and lv=List.tl(v) in
let xu=fst(pu) and yu=snd(pu)
and xv=fst(pv) and yv=snd(pv) in
if xu<xv
then teuzin0(lu,v,pu::accu)
else if xv<xu
     then teuzin0(u,lv,pv::accu)
	 else (*now we have xu=xv *)
      teuzin0(lu,lv,(xu,yu+yv)::accu)
) in
M(teuzin0(x,y,[]));;

let rec ddiforchan x=
if List.length(x)<2
then M(x)
else let t1=Listennou.didrochan(x) in
     teuzin(ddiforchan(fst(t1)))(ddiforchan(snd(t1)));;

let diforchan x=
ddiforchan(Image.image(function u->(u,1))(x));;

let big_teuzin x=List.fold_left(teuzin)(M[])(x);;

let leq (M x) (M y)=
let rec leq0=(
function (u,v)->
if u=[] then true else
if v=[] then false else
let pu=List.hd(u) and lu=List.tl(u) 
and pv=List.hd(v) and lv=List.tl(v) in
let xu=fst(pu) and yu=snd(pu)
and xv=fst(pv) and yv=snd(pv) in
if xu<xv
then false
else if xv<xu
     then leq0(u,lv)
     else if yu>yv
          then false
          else leq0(lu,lv)
)
in leq0(x,y);;

let algebraic_leq (M x) (M y)=
let rec leq0=(
function (u,v)->
if u=[] then true else
if v=[] then false else
let pu=List.hd(u) and lu=List.tl(u) 
and pv=List.hd(v) and lv=List.tl(v) in
let xu=fst(pu) and yu=snd(pu)
and xv=fst(pv) and yv=snd(pv) in
if xu>xv
then false
else if yu<yv
     then leq0(lu,(xv,yv-yu)::lv)
     else if yu=yv
          then leq0(lu,lv) 
          else leq0((xu,yu-yv)::lu,lv)
)
in leq0(List.rev x,List.rev y);;

