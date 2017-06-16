(*

#use"Php_analizer/php_keyword.ml";;

*)



(* from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#keywords 

   added the "exit" keyword
*)

type t=
     T_ABSTRACT
    |T_AS
    |T_BREAK
    |T_CALLABLE
    |T_CASE
    |T_CATCH
    |T_CLASS
    |T_CONST
    |T_CONTINUE
    |T_DECLARE
    |T_DEFAULT
    |T_DO
    |T_ECHO
    |T_ELSE
    |T_ELSEIF
    |T_ENDDECLARE
    |T_ENDFOR
    |T_ENDFOREACH
    |T_ENDIF
    |T_ENDSWITCH
    |T_ENDWHILE
    |T_EXIT
    |T_EXTENDS
    |T_FINAL
    |T_FINALLY
    |T_FOR
    |T_FOREACH
    |T_FUNCTION
    |T_GLOBAL
    |T_GOTO
    |T_IF
    |T_IMPLEMENTS
    |T_INCLUDE
    |T_INCLUDE_ONCE
    |T_INSTEADOF
    |T_INTERFACE
    |T_LIST
    |T_NAMESPACE
    |T_PRINT
    |T_PRIVATE
    |T_PROTECTED
    |T_PUBLIC
    |T_REQUIRE
    |T_REQUIRE_ONCE
    |T_RETURN
    |T_STATIC
    |T_SWITCH
    |T_THROW
    |T_TRAIT
    |T_TRY
    |T_USE
    |T_VAR
    |T_WHILE
    |T_YIELD;;

let to_string=function
     T_ABSTRACT->"abstract"
    |T_AS->"as"
    |T_BREAK->"break"
    |T_CALLABLE->"callable"
    |T_CASE->"case"
    |T_CATCH->"catch"
    |T_CLASS->"class"
    |T_CONST->"const"
    |T_CONTINUE->"continue"
    |T_DECLARE->"declare"
    |T_DEFAULT->"default"
    |T_DO->"do"
    |T_ECHO->"echo"
    |T_ELSE->"else"
    |T_ELSEIF->"elseif"
    |T_ENDDECLARE->"enddeclare"
    |T_ENDFOR->"endfor"
    |T_ENDFOREACH->"endforeach"
    |T_ENDIF->"endif"
    |T_ENDSWITCH->"endswitch"
    |T_ENDWHILE->"endwhile"
    |T_EXIT->"exit"
    |T_EXTENDS->"extends"
    |T_FINAL->"final"
    |T_FINALLY->"finally"
    |T_FOR->"for"
    |T_FOREACH->"foreach"
    |T_FUNCTION->"function"
    |T_GLOBAL->"global"
    |T_GOTO->"goto"
    |T_IF->"if"
    |T_IMPLEMENTS->"implements"
    |T_INCLUDE->"include"
    |T_INCLUDE_ONCE->"include_once"
    |T_INSTEADOF->"insteadof"
    |T_INTERFACE->"interface"
    |T_LIST->"list"
    |T_NAMESPACE->"namespace"
    |T_PRINT->"print"
    |T_PRIVATE->"private"
    |T_PROTECTED->"protected"
    |T_PUBLIC->"public"
    |T_REQUIRE->"require"
    |T_REQUIRE_ONCE->"require_once"
    |T_RETURN->"return"
    |T_STATIC->"static"
    |T_SWITCH->"switch"
    |T_THROW->"throw"
    |T_TRAIT->"trait"
    |T_TRY->"try"
    |T_USE->"use"
    |T_VAR->"var"
    |T_WHILE->"while"
    |T_YIELD->"yield";;




let all_keywords =
[
     T_ABSTRACT;
     T_AS;
     T_BREAK;
     T_CALLABLE;
     T_CASE;
     T_CATCH;
     T_CLASS;
     T_CONST;
     T_CONTINUE;
     T_DECLARE;
     T_DEFAULT;
     T_DO;
     T_ECHO;
     T_ELSE;
     T_ELSEIF;
     T_ENDDECLARE;
     T_ENDFOR;
     T_ENDFOREACH;
     T_ENDIF;
     T_ENDSWITCH;
     T_ENDWHILE;
     T_EXIT;
     T_EXTENDS;
     T_FINAL;
     T_FINALLY;
     T_FOR;
     T_FOREACH;
     T_FUNCTION;
     T_GLOBAL;
     T_GOTO;
     T_IF;
     T_IMPLEMENTS;
     T_INCLUDE;
     T_INCLUDE_ONCE;
     T_INSTEADOF;
     T_INTERFACE;
     T_LIST;
     T_NAMESPACE;
     T_PRINT;
     T_PRIVATE;
     T_PROTECTED;
     T_PUBLIC;
     T_REQUIRE;
     T_REQUIRE_ONCE;
     T_RETURN;
     T_STATIC;
     T_SWITCH;
     T_THROW;
     T_TRAIT;
     T_TRY;
     T_USE;
     T_VAR;
     T_WHILE;
     T_YIELD

];;
 
 
exception Unknown_keyword_string of string;; 

let of_prudent_string s=
  Option.find_it (fun oprtr->to_string(oprtr)=s) all_keywords;; 
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_keyword_string(s))
  |Some(oprtr)->oprtr;;
  
let all_strings=Image.image to_string all_keywords;;        
 
  
  
   
