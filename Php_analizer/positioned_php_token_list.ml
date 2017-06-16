(*

#use"Php_analizer/positioned_php_token_list.ml";;

*)

type t={
   contained : Positioned_php_token.t list;
};;

let empty={contained=[]};;
let is_empty x=(x.contained=[]);;
let hd x=List.hd(x.contained);;
let tl x={contained=List.tl(x.contained)};;
let concat x y={contained=(x.contained)@(y.contained)};;

let cons a x={contained=a::(x.contained)};;
let singleton x={contained=[x]};;
let rev x={contained=List.rev(x.contained)};;
let length x=List.length(x.contained);;
let big_head d x={contained=Listennou.big_head d (x.contained)};;

let filter f x={contained=List.filter f (x.contained)}

exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;
    
exception File_exn;;    
    
let file x=match x.contained with
    []->raise(File_exn)
    |a::_->Positioned_php_token.file a;;    
    
let print x=
  let temp1=Image.image(fun ptok->
    let tok=Positioned_php_token.fst ptok in
    Php_token.projected_version tok
   ) x.contained in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (print x);;

    