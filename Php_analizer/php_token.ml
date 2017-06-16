(*

#use"Php_analizer/php_token.ml";;

*)

type t=
     Constant of Php_constant_token.t
    |Variable of string 
    |Ident of string
    |Comment of string
    |Single_quoted of string
    |Double_quoted of string
    |Heredoc of string
    |Nowdoc of string
    |Namespacer of bool*(string list)*string
    |External_echo of string
    |Int of string
    |Float of string
    |Char of char
    |End_of_text;;

let content=function
     
      (Constant ctok)->Php_constant_token.to_string ctok
     |(Variable s)->s 
     |(Ident s)->s
     |(Comment s)->s
     |(Single_quoted s)->"'"^s^"'"
     |(Double_quoted s)->"\""^s^"\""
     |(Heredoc s)->s
     |(Nowdoc s)->s
     |(Namespacer (b,l,s))->s
     |(External_echo s)->s
     |(Int s)->s
     |(Float s)->s
     |(Char c)->String.make 1 c
     |(End_of_text)->"EOF";;

let short_content x=
   let s=content x in
   if String.length(s)>50
   then "..."
   else s;;

let is_a_comment=function
   (Comment s)->true
  |_->false;;

let fixture_of_nonconstants=
    [
       Variable""; 
       Ident"";
       Comment"";
       Single_quoted"";
       Double_quoted"";
       Heredoc"";
       Nowdoc"";
       Namespacer (false,[],"");
       External_echo"";
       Int "0";
       Float "0.";
       Char '0';
    ];;

(*     
let fixture=
    [
       Constant(Php_constant_token.Kwd (Php_keyword.T_ABSTRACT));
       Constant(Php_constant_token.Punct (Php_punctuator.T_LPARENTHESIS));
       Constant(Php_constant_token.Op(Php_operator.T_NEW));
    ] @ fixture_of_nonconstants;;
*)


let put_lexeme_in_category=Memoized.make(fun s->
  match Php_operator.of_prudent_string s with
   Some(op)->Constant(Php_constant_token.Op(op))
  |None->
  (
   match Php_punctuator.of_prudent_string s with
   Some(punkt)->Constant(Php_constant_token.Punct (punkt))
  |None->
   (
    match Php_keyword.of_prudent_string s with
     Some(kwd)->Constant(Php_constant_token.Kwd (kwd))
    |None->Ident(s)
   ) 
  ));;
  
let of_string=put_lexeme_in_category;;  
  
let all_constant_strings=
   ( Php_operator.all_strings)
  @( Php_punctuator.all_strings)
  @( Php_keyword.all_strings);;  
  
let nonalphanumeric_lexemes=
  let temp4=List.filter (fun s->
     not(Charset.string_is_alphanumeric s)
    ) all_constant_strings in
  let temp5=Ordered_string.diforchan temp4 in
  let temp6=Ordered.forget_order temp5 in
  List.rev_map (fun s->(s,put_lexeme_in_category s) ) temp6;;
   

let instructions_for_nonalphanumeric_lexemes=
  let temp1=Image.image (
     fun (x,_)->Strung.enclose (Str.global_replace (Str.regexp_string "\n") "\\n" x) 
  ) nonalphanumeric_lexemes in
  let temp2=String.concat "\n  | " temp1 in
  "\n  | "^temp2^" as op {add_to_list lexbuf (read_word op);usual lexbuf}";;
  
   
let give_instructions_for_nonalphanumeric_lexemes ()=
   let s=instructions_for_nonalphanumeric_lexemes 
   and beg_m="(* instructions for nonalphanumeric chars begin here *)"
   and end_m="(* instructions for nonalphanumeric chars end here *)" in
   Replace_inside.overwrite_between_markers_inside_file 
    (Overwriter.of_string s)
    (beg_m,end_m)
    (Absolute_path.of_string "Php_analizer/php_lexer.mll");;

let token_category =function
      Constant(ctok)   ->Php_constant_token.token_category ctok
     |Variable(_)      ->Token_category.Variable
     |Ident(_)         ->Token_category.Identifier
     |Comment (_)      ->Token_category.Comment
     |Single_quoted(_) ->Token_category.Single_quoted_string
     |Double_quoted(_) ->Token_category.Double_quoted_string
     |Heredoc(_)       ->Token_category.Heredoc_string
     |Nowdoc(_)        ->Token_category.Nowdoc_string
     |Namespacer(_,_,_)->Token_category.Namespacer
     |External_echo(_) ->Token_category.External_item
     |Int(_)           ->Token_category.Integer
     |Float(_)         ->Token_category.Floating_number
     |Char(_)          ->Token_category.Character
     |End_of_text      ->Token_category.End_of_text;;
     
     


let projected_version=function
      (Constant ctok)->Php_constant_token.to_string ctok
     |x->Token_category.to_string (token_category x);;

let precedence=function 
  Constant ctok->(match ctok with
                    (Php_constant_token.Op op)->Some(Php_operator.precedence(op))
                   |_->None
                 )
  |_->None;;

let op s=Constant(Php_constant_token.Op(Php_operator.of_string s));;
let punct s=Constant(Php_constant_token.Punct(Php_punctuator.of_string s));;
let kwd s=Constant(Php_constant_token.Kwd (Php_keyword.of_string s));;

let test ctok tok=(tok=Constant(ctok));;



