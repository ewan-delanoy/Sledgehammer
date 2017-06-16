(*

#use"Php_analizer/Php_syntax_types/script_includer.ml";;

*)

type t=
    Include
   |Include_once
   |Require
   |Require_once;;
   
exception Not_a_script_includer of Php_token.t;;   
   
let from_lexeme lxm=
  if lxm=Php_token.kwd"include"      then Include      else
  if lxm=Php_token.kwd"include_once" then Include_once else   
  if lxm=Php_token.kwd"require"      then Require      else
  if lxm=Php_token.kwd"require_once" then Require_once else   
  raise(Not_a_script_includer(lxm));;
    
let from_luxume lxm=
  try(Some(from_lexeme lxm)) with _->None;;