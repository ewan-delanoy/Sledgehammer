{

let pos_fname x=x.Lexing.pos_fname;;
let pos_lnum x=x.Lexing.pos_lnum;;
let pos_bol x=x.Lexing.pos_bol;;
let pos_cnum x=x.Lexing.pos_cnum;;

let pos_make a b c d={
  Lexing.pos_fname=a;
  Lexing.pos_lnum=b;
  Lexing.pos_bol=c;
  Lexing.pos_cnum=d;
};;

let dummy_lexing = Lexing.dummy_pos;;
let dummy_pair = (dummy_lexing,dummy_lexing);;

let lexing_from_file file=
  let s=Io.read_whole_file file in
  let lexbuf=Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = (Absolute_path.to_string file);
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
   lexbuf;;

let translated_lexing lxg j={
  Lexing.pos_fname=pos_fname lxg;
  Lexing.pos_lnum=pos_lnum lxg;
  Lexing.pos_bol=pos_bol lxg;
  Lexing.pos_cnum=(pos_cnum lxg)+j;
};;


let comment s=Php_token.Comment(s);;
let ustring s=Php_token.Single_quoted(s);;
let dstring s=Php_token.Double_quoted(s);;
let variable s=Php_token.Variable(s);;
let integer i=Php_token.Int(i);;
let floating f=Php_token.Float(f);;
let character c=Php_token.Char(c);;
let end_of_text=Php_token.End_of_text;;
let external_echo s=Php_token.External_echo s;;

let read_word=Php_token.put_lexeme_in_category;;
    
type doctype=Nowdoc_type |Heredoc_type |Naked_doc_type;;    
    
let list_accu=ref Positioned_php_token_list.empty;;
let string_accu=ref"";;
let doc_ident_accu=ref"";;
let match_counter=ref 0;;
let current_doctype=ref Naked_doc_type;;
let faraway_beginning=ref Lexing.dummy_pos;;



let adhoc_doc_string doc_type s=match doc_type with
  Nowdoc_type->Php_token.Nowdoc(s)
  |_->Php_token.Heredoc(s);;
  
let namespace_list_accu=ref([]:string list);;
let namespace_absolute=ref(false);;
let namespace_string_accu=ref"";;  
  
let mk=Positioned_php_token.make;;
let uv=Positioned_php_token.unveil;;

let push lbuf (a,start_a,end_a) l=
   if Positioned_php_token_list.is_empty l 
   then Positioned_php_token_list.singleton(mk a (start_a,end_a)) 
   else
   let (h,peurrest)=Positioned_php_token_list.ht(l) in
   let (b,(start_b,end_b))=uv(h) in
   match (a,b) with
   (Php_token.Comment(ca),Php_token.Comment(cb))->
    let ba=ca^cb in
    Positioned_php_token_list.cons (mk (comment ba) (start_b,end_a)) peurrest
    |_->Positioned_php_token_list.cons (mk a (start_a,end_a)) l ;;
   
let preceding_lexeme=ref(None);;
   
let add_to_list lbuf a=
    let start_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1
    and end_a=Lexing.lexeme_end_p lbuf in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));; 

let add_long_one_to_list lbuf a=
    let start_a=translated_lexing (!faraway_beginning) 1
    and end_a=translated_lexing (Lexing.lexeme_end_p lbuf) 0 in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));;

let add_long_doc_to_list lbuf a=
    let start_a=translated_lexing (!faraway_beginning) 1
    and end_a=translated_lexing (Lexing.lexeme_end_p lbuf) (-1) in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));;


let add_composite_to_list lbuf (b,c)=
    let start_b=translated_lexing(Lexing.lexeme_start_p lbuf) 1
    and end_c=Lexing.lexeme_end_p lbuf in
    let end_b=translated_lexing start_b (String.length b-1)
    and start_c=translated_lexing end_c (1-(String.length c)) in
    (
     list_accu:=push lbuf (read_word b,start_b,end_b) (!list_accu);
     list_accu:=push lbuf (read_word c,start_c,end_c) (!list_accu);
     preceding_lexeme:=Some(read_word c);
     );;     

let semicolon=Php_token.of_string ";";;

let insert_semicolon lbuf=
    let start_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1 in
    let end_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1 in
    list_accu:=push lbuf (semicolon,start_a,end_a) (!list_accu);;

let insert_semicolon_if_needed lbuf=
    if (!preceding_lexeme)<>(Some semicolon)
    then insert_semicolon lbuf;;
    
let add_to_string c=(string_accu:=(!string_accu)^(String.make 1 c));;
let add_to_doc_ident c=(doc_ident_accu:=(!doc_ident_accu)^(String.make 1 c))  ;;
let initialize_namespace lbuf=(
   faraway_beginning:=(Lexing.lexeme_end_p lbuf);
);;
  

let ingest_namespace_element word=
     let w=String.sub word 0 ((String.length word)-1) in
     (
     namespace_list_accu:=w::(!namespace_list_accu);
     namespace_string_accu:=(!namespace_string_accu)^word
     );;
let finish_namespace w lbuf=
      let full_list=List.rev(w::(!namespace_list_accu))
      and full_string=(!namespace_string_accu)^w in
      let tok=Php_token.Namespacer (!namespace_absolute,full_list,full_string) in
      let start_t=(!faraway_beginning)
      and end_t=translated_lexing(Lexing.lexeme_end_p lbuf) 0 in
      (
        namespace_list_accu:=[];
        namespace_string_accu:="";
        list_accu:=Positioned_php_token_list.cons (mk tok (start_t,end_t)) (!list_accu);
      );;     
      
let initialize_doc lbuf=(
     doc_ident_accu:="\n"^(!doc_ident_accu);
     match_counter:=String.length(!doc_ident_accu)
     );;
let finish_comment lbuf =(add_long_one_to_list lbuf (comment(!string_accu)); string_accu:=""; );;
let finish_quote lbuf =(add_long_one_to_list lbuf (ustring(!string_accu)); string_accu:=""; );;
let finish_dquote lbuf =(add_long_one_to_list lbuf (dstring(!string_accu)); string_accu:=""; );;
let finish_nonphp lbuf =
 if (!string_accu)<>""
 then (
      
      add_long_one_to_list lbuf (external_echo(!string_accu));
      string_accu:="";
      );;


let finish_doc lbuf=
      let new_length=(String.length(!string_accu))-(String.length(!doc_ident_accu)) in
      let _=(string_accu:=String.sub (!string_accu) 0 new_length) in
      let tok=adhoc_doc_string (!current_doctype) (!string_accu) in
      string_accu:="";
      add_long_doc_to_list lbuf tok;
      insert_semicolon lbuf;;
      



exception Illegal_first_character_in_doc of char;;
exception Illegal_character_in_doc of char;;
exception Illegal_character_in_naked_doc of char;;
exception Illegal_character_in_namespace of char;;
exception Missing_newline_in_doc_beginning;;

exception Unfinished_doc of string*string*int;;
exception Badly_terminated_doc_ident;;

exception Badly_terminated_doc_text;;

let debug_list=ref [];;

    
}
    
let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*
let id = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*


rule outside_php = parse
  | "<?php "  {finish_nonphp lexbuf; inside_php lexbuf;}
  | "<?php\n"  {finish_nonphp lexbuf; inside_php lexbuf;}
  | _ as c {add_to_string c; outside_php lexbuf}
  |eof {finish_nonphp lexbuf;Positioned_php_token_list.rev(!list_accu)} 
  and inside_php=parse
  | " ?>"  {
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            (* insert_semicolon_if_needed lexbuf; *)
            outside_php lexbuf;}
  | "\n?>" {
            
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            (* insert_semicolon_if_needed lexbuf; *)
            Lexing.new_line lexbuf;
            outside_php lexbuf;
            }
  | "/*" { starred_comment lexbuf;}
  | "//" { slashed_comment lexbuf;}
  | '\'' { 
            faraway_beginning:=Lexing.lexeme_start_p lexbuf; 
            single_quoted_string lexbuf;}
  | '"'  { 
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            double_quoted_string lexbuf;}
  | "<<<" {
             faraway_beginning:=Lexing.lexeme_start_p lexbuf;
             doc_string lexbuf;
          }
  | ['\t' ' '] {inside_php lexbuf;}
  | ['\n' '\r'] {Lexing.new_line lexbuf;inside_php lexbuf;}
  | digit+ as inum
  	{ 
  	  let tok=integer inum in
  	  add_to_list lexbuf  tok; inside_php lexbuf
	}
  | digit+ '.' digit* as fnum
  	{ 
	  let tok=floating fnum in
  	  add_to_list lexbuf  tok; inside_php lexbuf
	}
  | '$' id as varname {  add_to_list lexbuf  (variable varname); inside_php lexbuf}	
  (* instructions for nonalphanumeric chars begin here *)
  | "(string)"
  | "(object)"
  | "(float)"
  | "(array)"
  | "<?php "
  | "<?php\n"
  | "(bool)"
  | "(int)"
  | ">>="
  | "==="
  | "<<="
  | "**="
  | "!=="
  | "||"
  | "|="
  | "^="
  | "?>"
  | ">>"
  | ">="
  | "=>"
  | "=="
  | "<>"
  | "<="
  | "<<"
  | "/="
  | ".="
  | "->"
  | "-="
  | "--"
  | "+="
  | "++"
  | "*="
  | "**"
  | "&="
  | "&&"
  | "%="
  | "!="
  | "::"
  | "~"
  | "}"
  | "|"
  | "{"
  | "^"
  | "]"
  | "["
  | "@"
  | "?"
  | ">"
  | "="
  | "<"
  | ";"
  | ":"
  | "/"
  | "."
  | "-"
  | ","
  | "+"
  | "*"
  | ")"
  | "("
  | "&"
  | "%"
  | "$"
  | "!" as op {
                add_to_list lexbuf (read_word op);
                inside_php lexbuf
              }
  (* instructions for nonalphanumeric chars end here *)
  | id "\\" as word {
                      initialize_namespace lexbuf;
                      ingest_namespace_element word;
                      namespace_mode lexbuf
                    }
  | '\\'  {
            namespace_absolute:=true;
            namespace_string_accu:="\\";
            initialize_namespace lexbuf;
            namespace_mode lexbuf
           }
  | "elseif"  { add_composite_to_list lexbuf  ("else","if"); inside_php lexbuf }        
  | id as word { add_to_list lexbuf  (read_word word); inside_php lexbuf }         
  | _ as c
  	{  add_to_list lexbuf  (character c); inside_php lexbuf}
  | eof
  	{ Positioned_php_token_list.rev(!list_accu) }
and starred_comment=parse
  |"*/" {finish_comment lexbuf; inside_php lexbuf}
  | _ as c {add_to_string c; starred_comment lexbuf}
  |eof {Positioned_php_token_list.rev(!list_accu)}
and slashed_comment=parse
  |['\n' '\r'] {finish_comment lexbuf; inside_php lexbuf}
  |" ?>" {finish_comment lexbuf; outside_php lexbuf}
  |"\n?>" {finish_comment lexbuf; outside_php lexbuf}
  | _ as c {add_to_string c; slashed_comment lexbuf}
  |eof {Positioned_php_token_list.rev(!list_accu)} 
and single_quoted_string=parse
  | "\\\'" {add_to_string '\''; single_quoted_string lexbuf}
  | "\\\\" {add_to_string '\\'; single_quoted_string lexbuf}
  | '\'' {finish_quote lexbuf; inside_php lexbuf}
  | _ as c {add_to_string c; single_quoted_string lexbuf}
  |eof {Positioned_php_token_list.rev(!list_accu)}    
and double_quoted_string=parse
  | "\\\"" {add_to_string '\''; double_quoted_string lexbuf}
  | "\\\\" {add_to_string '\\'; double_quoted_string lexbuf}
  | '\"' {finish_dquote lexbuf; inside_php lexbuf}
  | _ as c {add_to_string c; double_quoted_string lexbuf}
  |eof {Positioned_php_token_list.rev(
        Positioned_php_token_list.cons
        (mk end_of_text (Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf))
         (!list_accu))}     
and doc_string=parse
  | '\'' { current_doctype:=Nowdoc_type;step_one_in_doc lexbuf;}
  | '"'  { current_doctype:=Heredoc_type;step_one_in_doc lexbuf;}
  | alphanum as s {current_doctype:=Naked_doc_type;
                   doc_ident_accu:=s;
                   step_two_in_doc lexbuf;}
  | _ as c {raise(Illegal_first_character_in_doc(c))}
and step_one_in_doc=parse
  | alphanum as doc_ident {doc_ident_accu:=doc_ident;step_two_in_doc lexbuf}  
  | _ as c {raise(Illegal_character_in_doc(c))}  
and step_two_in_doc=parse
   "\"\n" {if((!current_doctype)<>Heredoc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf}
  | "'\n" {if((!current_doctype)<>Nowdoc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf}          
  | "\n" {if((!current_doctype)<>Naked_doc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf}    
  | _  {raise(Missing_newline_in_doc_beginning)}        
and step_three_in_doc=parse
   eof {raise(Unfinished_doc(!string_accu,!doc_ident_accu,!match_counter))}
   | _ as c {
      let s=(!doc_ident_accu)  in
      let n=String.length s in
      let j=(!match_counter) in
      (if ( c=(String.get s (n-j)) ) 
      then match_counter:=j-1
      else if c='\n'
           then match_counter:=n-1
           else match_counter:=n);
      add_to_string c;
      debug_list:=(c,!match_counter)::(!debug_list);
      if (( !match_counter)<1)
      then (
            step_four_in_doc lexbuf
           ) 
      else step_three_in_doc lexbuf
    }
and step_four_in_doc=parse
   ";" {
         step_five_in_doc lexbuf      
        }    
  |"\n" {
         finish_doc lexbuf;
         inside_php lexbuf         
        }    
  |_ as c 
        {
           let m=String.length(!doc_ident_accu) in
           string_accu:=((!string_accu)^(!doc_ident_accu));
           add_to_string c;
           match_counter:=String.length(!doc_ident_accu);
           (if c='\n' 
           then match_counter:=m-1
           else match_counter:=m);
           step_three_in_doc lexbuf;
        }  
  and step_five_in_doc=parse
     "\n" {
             finish_doc lexbuf;
             inside_php lexbuf   
          }
    |_ 
         {
           raise(Badly_terminated_doc_text);
         }      
  and namespace_mode=parse 
    id "\\" as word {
                      ingest_namespace_element word;
                      namespace_mode lexbuf
                    }
  | id as w  {
               finish_namespace w lexbuf;
               inside_php lexbuf
             }
  |_ as c {raise(Illegal_character_in_namespace(c))}               
  
{

let parse_string s =
          let _=(list_accu:=Positioned_php_token_list.empty;string_accu:="") in
          outside_php (Lexing.from_string s);;
  
let parse_file fn=
         let _=(list_accu:=Positioned_php_token_list.empty;string_accu:="") in
          outside_php (lexing_from_file fn);;
}
  
  
   