(*

#use"outside_comments_and_strings.ml";;

Detect in a text the parts which can possibly contain module
names, i.e. those parts which are outside comments and outside
strings.

Comments are a little more complicated than strings because they
can be nested. Also, note that we can have strings inside comments :
for example (* a "(*" b *) is a valid OCaml code snippet.

*)

(*
To keep the automaton simple, changes are notified as soon as
possible. Thus, the automaton toggles string_mode or changes
nesting comment depth as soon as the terminating character is
encountered.

*)

type state={
    depth : int;
    string_mode         : bool;
    lastchar_is_a_left_paren        : bool;
    lastchar_is_a_star              : bool;
    lastchar_is_a_backslash         : bool;
    penultchar_is_a_left_paren      : bool;
    interval_start : int;
    accumulator : (int*int*string) list;
};;


let one_more_step s n j c x=
   let d=x.depth in 
   let comment_opened_now=(x.lastchar_is_a_left_paren)&&(c='*')&&(not(x.string_mode))
   and comment_closed_now=
            (not(x.penultchar_is_a_left_paren))
            &&(x.lastchar_is_a_star)
            &&(c=')')
            &&(not(x.string_mode)) in
   let new_depth=(
   		if x.string_mode
        then d
        else
        if comment_opened_now
        then d+1
        else  
        if comment_closed_now
        then max 0 (d-1)
        else  d
   
   ) in
   let new_start=
      ((x.depth=0)&&(x.string_mode)&&(c='"')&&(not(x.lastchar_is_a_backslash)))
      ||
      ((x.depth=1)&&comment_closed_now)
      ||
      ((x.depth=0)&&comment_opened_now) in
    let opt_upper_bound=(
       if x.depth>0
       then None
       else
       if (c='"')&&(not(x.string_mode))&&(not(x.lastchar_is_a_backslash))
       then Some(j-1)
       else
       if comment_opened_now
       then Some(j-2)
       else 
       if (j=n)&&(not(x.string_mode))
       then Some(j)
       else None
    ) in
    let old_accu=x.accumulator in  
    let new_accu=(
       match opt_upper_bound with
       None->old_accu
       |Some(upper_bound)->
           let lower_bound=x.interval_start in
           if lower_bound>upper_bound
           then old_accu
           else let new_itv=Cull_string.interval s lower_bound upper_bound in 
               (lower_bound,upper_bound,new_itv)::old_accu
    ) in  
      
  {
    depth =new_depth;
    string_mode    =(if (x.lastchar_is_a_backslash)||(c<>'\"')
                     then x.string_mode
                     else not(x.string_mode));
    lastchar_is_a_left_paren   =(c='(');
    lastchar_is_a_star         =(c='*');
    lastchar_is_a_backslash    =(c='\\');
    penultchar_is_a_left_paren =x.lastchar_is_a_left_paren;
    interval_start=(if new_start then j+1 else x.interval_start);
    accumulator=new_accu;
};;

let initial_state=  
 {
    depth =0;
    string_mode    =false;
    lastchar_is_a_left_paren   =false;
    lastchar_is_a_star         =false;
    lastchar_is_a_backslash    =false;
    penultchar_is_a_left_paren =false;
    interval_start=1;
    accumulator=[];
};;

let rec iterator (s,n,j,st)=
    if j>n
    then List.rev(st.accumulator)
    else iterator(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
    
let good_substrings s=iterator(s,String.length s,1,initial_state);;    

(*


good_substrings "abcdef";;
good_substrings "(*abc*)def";;
good_substrings "ab(*cde*)f";;
good_substrings "ab\"cde\"f";;
good_substrings "\"abc\"def";;
good_substrings "ghi(*a(*b*)c*)def";;
good_substrings "ghi(**a(*b*)c**)def";;
good_substrings "ghi(**a\"b\"c**)def";;
good_substrings "123\"(*\"890\"*)\"567";;
good_substrings "123(*67\"90\"23*)67";;
good_substrings "let a=\"\\\"\" in a+1;;";;
good_substrings "let a='\\\"' in a+1;;";;


let nachste (s,n,j,st)=(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
let s0="123456'\\"^"\"'123456789";;
let n0=String.length s0;;
let v0=(s0,n0,1,initial_state);;
let ff=Memoized.small nachste v0;;
let gg n=match ff n with (_,_,_,st)->st;;


*)      

