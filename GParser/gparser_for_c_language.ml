(*

#use"GParser/hparser_for_c_language.ml";;

*)

let prsr_for_comment=Gparser.Enclosure ("/*","*/");;


let prsr_for_space=Gparser.Constant " ";;
let prsr_for_tab=Gparser.Constant "\t";;

let prsr_for_space_or_tab=Gparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Gparser.Constant "\n";;
let prsr_for_newline=Gparser.Constant "\012";;
let prsr_for_individual_white=Gparser.Disjunction [prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline];;

let prsr_for_inline_white_maybe=Gparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Gparser.Star prsr_for_individual_white;;
let prsr_for_white=Gparser.One_or_more prsr_for_individual_white;;

let prsr1=Gparser.Enclosure ("\"","\"");;
let prsr2=Gparser.Enclosure ("<",">");;
let prsr3=Gparser.Disjunction [prsr1;prsr2];;


let prsr_for_inclusion=Gparser.Chain
      [
        Gparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser.Constant "include";
        prsr_for_inline_white_maybe;
        prsr3
      ];;


let prsr_for_typeword_not_starting_with_u=Gparser.Chain
   [
     Gparser.Sample_neg "u";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;

let prsr_for_typeword_starting_with_u_but_not_with_un=Gparser.Chain
   [
     Gparser.Constant "u";
     Gparser.Sample_neg "n";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;

let prsr_for_typeword=Gparser.Disjunction
   [
     prsr_for_typeword_not_starting_with_u;
     prsr_for_typeword_starting_with_u_but_not_with_un;
     
   ];;



let prsr_for_braced=Gparser.Chain
   [
      Gparser.Constant "{";
      Gparser.House_with_doors ("{","}",["/*","*/";"//","\n";"\"","\"";"'","'"]);
   ];;

let prsr_for_typename1=Gparser.Chain
   [
      Gparser.Constant "struct";
      prsr_for_inline_white_maybe;
      prsr_for_typeword;
      prsr_for_white;
      Gparser.Optional(prsr_for_braced);
   ];;
   
let prsr_for_typename2=Gparser.Chain
   [
      Gparser.Constant "unsigned";
      prsr_for_inline_white_maybe;
      Gparser.Constant "int";
   ];;    


let prsr_for_typename3=Gparser.Chain
   [
      Gparser.Constant "long";
      prsr_for_inline_white_maybe;
      Gparser.Constant "int";
   ];;    

let prsr_for_typename4=Gparser.Chain
   [
      Gparser.Constant "unsigned";
      prsr_for_inline_white_maybe;
      Gparser.Constant "long";
   ];;    

let prsr_for_typename=Gparser.Disjunction
   [
     prsr_for_typename1;
     prsr_for_typename2;
     prsr_for_typename3;
     prsr_for_typename4;
     prsr_for_typeword;
     Gparser.Constant "FILE";
   ];;

let prsr_for_possibly_starred_typename=Gparser.Chain
   [
     prsr_for_typename;
     prsr_for_inline_white_maybe;
     Gparser.Optional(Gparser.Constant "*");
   ];;

let prsr_for_variableword=Gparser.Chain
	[
	  Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
	  Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_0123456789"
	];;

let prsr_for_identword=Gparser.Chain
	[
	  Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	  Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	];;

let prsr_for_uppercase_word=Gparser.Sample_plus "_ABCDEFGHIJKLMNOPQRSTUVWXYZ";; 

let prsr_for_negative_int=Gparser.Chain
	[
	  Gparser.Constant "-";
	  Gparser.Sample_star "0123456789"
	];;


let prsr_for_rvalue=Gparser.Disjunction
   [
     prsr_for_identword;
     prsr_for_negative_int;
     Gparser.Sample_plus "0123456789";
     prsr_for_braced;
   ];;

let prsr_for_vardecl=Gparser.Chain
   [
      
      prsr_for_possibly_starred_typename;
      prsr_for_inline_white_maybe;
      prsr_for_variableword;
      prsr_for_inline_white_maybe;
      Gparser.Constant "=";
      prsr_for_inline_white_maybe;
      prsr_for_rvalue;
      prsr_for_inline_white_maybe;
      Gparser.Constant ";";
   ];;

let prsr_for_def_directive=Gparser.Chain
      [
        Gparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser.Constant "define";
        prsr_for_space_or_tab;
        Gparser.Race ("\\\n","\n");
      ];;

let prsr_for_lonely_def_directive=Gparser.Chain
      [
        Gparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser.Constant "define";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Gparser.Footless_constant "\n";
      ];;

let prsr_for_undef_directive=Gparser.Chain
      [
        Gparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser.Constant "undef";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Gparser.Footless_constant "\n";
      ];;

let prsr_for_typename_inliner1=Gparser.Chain
     [
       Gparser.Constant "__inline";
       prsr_for_white;
       Gparser.Constant "static";
     ];;

let prsr_for_typename_inliner2=Gparser.Chain
     [
       Gparser.Constant "static";
       prsr_for_white;
       Gparser.Constant "inline";
       prsr_for_white;
       Gparser.Constant "const";
     ];;



let prsr_for_typename_inliner=Gparser.Disjunction
     [
       prsr_for_typename_inliner1;
       prsr_for_typename_inliner2;
       Gparser.Constant "__inline";
       Gparser.Constant "static";
       Gparser.Constant "extern";
     ];;

let prsr_for_whitened_typename_inliner=Gparser.Chain
    [
      prsr_for_typename_inliner;
      prsr_for_white;
    ];;

let prsr_for_fundecl1=
     Gparser.Chain
   [
      
      Gparser.Optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white_maybe;
      Gparser.Optional(prsr_for_uppercase_word);
      prsr_for_white_maybe;
      Gparser.House_with_doors ("(",")",[]);
      prsr_for_white_maybe;
      Gparser.Optional(Gparser.Constant "internal_function");
      Gparser.Constant ";";
   ];;

let prsr_for_fundecl=
   Gparser.Disjunction
    [
      prsr_for_fundecl1;
    ];;


let prsr_for_fundef1=
     Gparser.Chain
   [
      
      Gparser.Optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      Gparser.Optional(Gparser.Constant "internal_function");
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white;
      Gparser.Enclosure ("(",")");
      prsr_for_white;
      Gparser.Sample_negstar "{";     
      prsr_for_braced;
   ];;
   
let prsr_for_fundef2=
     Gparser.Chain
   [
      
      Gparser.Constant "RETSIGTYPE";
      prsr_for_white;
      prsr_for_variableword;
      prsr_for_white;
      Gparser.Enclosure ("(",")");
      prsr_for_white;
      Gparser.Sample_negstar "{";     
      prsr_for_braced;
   ];;   
   
let prsr_for_fundef=
   Gparser.Disjunction
    [
      prsr_for_fundef1;
      prsr_for_fundef2;
    ];;   

let prsr_for_structdef=
     Gparser.Chain
   [
      Gparser.Constant "struct";
      prsr_for_white;
      prsr_for_typeword;
      prsr_for_white_maybe;
      prsr_for_braced;
      prsr_for_white_maybe;
      Gparser.Constant ";";
   ];;

let elt_prsr=
   Gparser.Disjunction
     [
       prsr_for_inclusion; 
       prsr_for_lonely_def_directive;
       prsr_for_def_directive;
       prsr_for_undef_directive;
       prsr_for_individual_white;
       prsr_for_comment;
       prsr_for_vardecl;
       prsr_for_fundecl;
       prsr_for_fundef;
       prsr_for_structdef;
     ];;


let main_prsr=
   Gparser.Star elt_prsr;;



   
