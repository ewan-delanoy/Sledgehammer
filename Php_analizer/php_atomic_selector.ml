(*

#use"Php_analizer/php_atomic_selector.ml";;

Selector that only deals with one character.

*)

type t =                                                                    
        CT of Php_constant_token.t
      | TC of Token_category.t
      | Prec of Strict_or_loose.t * Php_operator.t
      | LCT of Php_constant_token.t list
      | NCT of Php_constant_token.t list
      | CTTC of (Php_constant_token.t list)*(Token_category.t list);;
      
let is_constant =function
   CT(_)->true
   |_->false;;

let escape_list=[];;

let vbar_escape s=
   try List.assoc s escape_list with
   _->s;;      

let everyone_selector=NCT[];;

let special_list =
  [
    "assign",LCT(
              Image.image (fun x->Php_constant_token.Op(x))
               [Php_operator.T_EQUALS; 
                Php_operator.T_VLINE_EQUALS; 
                Php_operator.T_PLUS_EQUALS; 
                Php_operator.T_MINUS_EQUALS;
                Php_operator.T_STAR_EQUALS]
            );     
    "coerce",LCT(
              Image.image (fun x->Php_constant_token.Op(x))
               [
                Php_operator.T_COERCE_TO_INT; 
                Php_operator.T_COERCE_TO_BOOL;
                Php_operator.T_COERCE_TO_STRING;
                Php_operator.T_COERCE_TO_ARRAY;
                Php_operator.T_COERCE_TO_OBJECT; 
                Php_operator.T_COERCE_TO_BOOL;
               ]
            );          
    "id_or_var",CTTC([],
               [
                Token_category.Variable; Token_category.Identifier; 
               ]
              ); 
    "include_like",LCT(
               Image.image (fun x->Php_constant_token.Kwd(x))
               [Php_keyword.T_INCLUDE; 
                Php_keyword.T_INCLUDE_ONCE;                                
                Php_keyword.T_REQUIRE; 
                Php_keyword.T_REQUIRE_ONCE]   
               );                     
    "int_or_string_or_var",CTTC([],
               [
                Token_category.Variable;Token_category.Identifier;
                Token_category.Integer;
                Token_category.Single_quoted_string;Token_category.Double_quoted_string;
               ]
              );             
    "no_breach",NCT[Php_constant_token.Kwd Php_keyword.T_FOREACH;
                    Php_constant_token.Kwd Php_keyword.T_ENDFOREACH];
    "no_colon",NCT[Php_constant_token.Op Php_operator.T_COLON];
    "no_ivies",NCT(
               Image.image (fun x->Php_constant_token.Kwd(x))
               [Php_keyword.T_IF; Php_keyword.T_ELSE; Php_keyword.T_ELSEIF; Php_keyword.T_ENDIF]
               );
    "no_left_brace",   NCT[Php_constant_token.Punct Php_punctuator.T_LBRACE];
    "no_semicolon",NCT[Php_constant_token.Punct Php_punctuator.T_SEMICOLON];
    "no_ternary",NCT(
              Image.image (fun x->Php_constant_token.Op(x))
               [
                Php_operator.T_QUESTION; 
                Php_operator.T_COLON;
               ]
            );   
    "stringy",CTTC(
               [
                Php_constant_token.Op Php_operator.T_DOT; 
                Php_constant_token.Op Php_operator.T_LBRACKET;
                Php_constant_token.Op Php_operator.T_RBRACKET;
                Php_constant_token.Op Php_operator.T_QUESTION;
                Php_constant_token.Op Php_operator.T_COLON;
                Php_constant_token.Op Php_operator.T_EQUALS_MORE;
                Php_constant_token.Punct Php_punctuator.T_COLON_COLON;
                Php_constant_token.Punct Php_punctuator.T_LPARENTHESIS;
                Php_constant_token.Punct Php_punctuator.T_RPARENTHESIS;
                Php_constant_token.Punct Php_punctuator.T_COMMA;
                Php_constant_token.Punct Php_punctuator.T_ARROW;
               ],
               [
                Token_category.Variable; Token_category.Identifier; 
                Token_category.Comment; Token_category.Single_quoted_string;
                Token_category.Double_quoted_string; Token_category.Heredoc_string; 
                Token_category.Nowdoc_string
               ]
              );  
  ];; 
 
exception Unregistered of t;; 
 
let to_string = function
   CT(ctok)->vbar_escape(Php_constant_token.to_string ctok)
  |TC(tc)->Token_category.to_string(tc)
  |Prec(sol,op)->(Strict_or_loose.to_string sol)^(Php_operator.to_string op)
  |x->try (fst(Option.find_really (fun p->snd(p)=x) special_list)) 
      with 
      _->raise(Unregistered(x));;

let all_constants=
       let temp1=Cartesian.product Strict_or_loose.all Php_operator.all_operators in
       (
         Image.image (fun (s,ct)->(vbar_escape s,CT ct) ) Php_constant_token.all_pairs
       )
       @
       (
         Image.image (fun tc->(Token_category.to_string tc,TC tc) ) Token_category.all_tokens
       )
       @
       (
         Image.image (fun (sol,op)->let sel=Prec(sol,op) in (to_string sel,sel) ) temp1
       ) 
       @special_list;;

let all_string_constants=Image.image fst all_constants;;

exception Unknown of string;;

let optional_of_string s0=match 
   Option.find_it (fun (s,sel)->s=s0) all_constants with
   None->None
   |Some(_,sel)->Some(sel);;
   
let of_string s=match optional_of_string s with
   None->raise(Unknown(s))
  |Some(s)->s;;
  
let test sel tok = match sel with
   CT(ctok)->Php_token.test ctok tok
  |TC(tc)->Php_token.token_category(tok)=tc
  |NCT(lctok)->List.for_all (fun ctok->not(Php_token.test ctok tok)) lctok
  |LCT(lctok)->List.exists (fun ctok->Php_token.test ctok tok) lctok
  |CTTC(lctok,ltc)->(List.exists (fun ctok->Php_token.test ctok tok) lctok)
                    ||
                    (List.mem (Php_token.token_category tok) ltc)
  |Prec(sol,op)->
     if (List.mem (Php_token.token_category tok) Token_category.harmless_tokens)
           ||
           (List.mem tok [Php_token.punct"(";Php_token.punct")"])
       then true   
       else let p=Php_token.precedence(tok) in
            if p=None
            then false 
            else Strict_or_loose.test sol (Option.unpack p) (Php_operator.precedence op)
  ;;   
   
(*

let gg x=
   let y=of_string x in
   let z=to_string y in
   let u=of_string z in
   (y,y=u);;

gg "if new instanceof kwd variable";;
gg "  if new instanceof kwd variable";;
gg "deny  if new instanceof kwd variable";;


*)


