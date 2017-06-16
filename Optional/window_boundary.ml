

(*
   To call this file in the ocaml interpreter, type
   
   #use "window_boundary.ml";;

Computations to decide the size of the graphic window and draw
the axes and some garduation on it.
The main cases are :

1) The user provides nothing except the function itself and the X-range.
2) The user provides the function, the X-range and the units.


*)

type paper_abscissa=Tia of int;; (* used to locate points on the printed paper ; in tenths of millimeters *)
type paper_ordinate=Tio of int;; (* used to locate points on the printed paper ; in tenths of millimeters *)

type abstract_abscissa_difference_in_one_centimeter=Tiau of float;; 
type abstract_ordinate_difference_in_one_centimeter=Tiou of float;; 

type minimal_abscissa=Mia of float;;
type minimal_ordinate=Mio of float;;

type centimeters_in_one_unit=Ciou of float;;

type optional_stuff=
    Forced_graph_paper
   |Forced_X_grid
   |Forced_Y_grid
   |X_left_offset of float
   |X_right_offset of float
   |Y_lower_offset of float
   |Y_upper_offset of float
   |Forced_X_min of float
   |Forced_X_max of float
   |Forced_Y_min of float
   |Forced_Y_max of float;;

let paper_abscissa_of_abscissa (Tiau au) (Mia xmin) x=
  Basic.nearest_int_of_float ((x-.xmin)/.au);;

let abscissa_of_paper_abscissa (Tiau au) (Mia xmin) (Tia i)=
  xmin+.(au*.(float_of_int i));;  

let paper_ordinate_of_ordinate (Tiou ou)(Mio ymin) y=
  Basic.nearest_int_of_float ((y-.ymin)/.ou);;

let ordinate_of_paper_ordinate (Tiou ou) (Mio ymin) (Tio i)=
  ymin+.(ou*.(float_of_int i));;  

type window_boundary_data={
  width : paper_abscissa; 
  height : paper_ordinate; 
  xmin : minimal_abscissa; 
  ymin : minimal_ordinate; 
  dx_in_cm : abstract_abscissa_difference_in_one_centimeter;
  dy_in_cm : abstract_ordinate_difference_in_one_centimeter;
  forced_graph_paper : bool;
  forced_x_grid : bool;
  forced_y_grid : bool;
};;

let coordinates_of_paper_coordinates (w:window_boundary_data) (ix,iy)=
   let x=abscissa_of_paper_abscissa w.dx_in_cm w.xmin ix
   and y=ordinate_of_paper_ordinate w.dy_in_cm w.ymin iy 
   in
   (x,y);;
   
let  paper_coordinates_of_coordinates  (w:window_boundary_data) (x,y)=
   let ix=paper_abscissa_of_abscissa w.dx_in_cm w.xmin x
   and iy=paper_ordinate_of_ordinate w.dy_in_cm w.ymin y 
   in
   (ix,iy);;   
   
let tikz_from_paper i=
    let di=Decimal_number.mult (Decimal_number.of_int i) (Decimal_number.power_of_ten (-2)) in
    Decimal_number.to_string (Decimal_number.general_rounded_transform 2 di);;  

let tikzify_abscissa w x=
  let ix=paper_abscissa_of_abscissa w.dx_in_cm w.xmin x in
  tikz_from_paper ix;;
  
let tikzify_ordinate w y=
  let iy=paper_ordinate_of_ordinate w.dy_in_cm w.ymin y in
  tikz_from_paper iy;;  

let draw_horizontal_line (w:window_boundary_data) y0=
  let t1=tikzify_ordinate w y0 
  and t2=tikz_from_paper ((function Tia i->i)(w.width)) in  
  "\\draw (O)++(0,"^t1^") -- ++("^t2^",0);";;
  
let draw_vertical_line (w:window_boundary_data) x0=
  let t1=tikzify_abscissa w x0
  and t2=tikz_from_paper ((function Tio i->i)(w.height)) in  
  "\\draw (O)++("^t1^",0) -- ++(0,"^t2^");";;  
  
let draw_horizontal_lines (w:window_boundary_data) ly=
  let t2=tikz_from_paper ((function Tia i->i)(w.width)) 
  and lt=Image.image (tikzify_ordinate w) ly in
  let s1="\\foreach \\y in {"^(String.concat "," lt)^"}"
  and s2="   {"
  and s3="     \\draw (O)++(0, \\y) -- ++("^t2^", 0);"
  and s4="   };"  
  in
  String.concat "\n" [s1;s2;s3;s4];;
  
let draw_vertical_lines (w:window_boundary_data) lx=
  let t2=tikz_from_paper ((function Tio i->i)(w.height)) 
  and lt=Image.image (tikzify_abscissa w) lx in
  let s1="\\foreach \\x in {"^(String.concat "," lt)^"}"
  and s2="   {"
  and s3="     \\draw (O)++(\\x, 0) -- ++(0, "^t2^");"
  and s4="   };"  
  in
  String.concat "\n" [s1;s2;s3;s4];;  
  
let draw_thick_horizontal_line (w:window_boundary_data) y0=
  let t1=tikzify_ordinate w y0 
  and t2=tikz_from_paper ((function Tia i->i)(w.width)) in  
  "\\draw [thick] (O)++(0,"^t1^") -- ++("^t2^",0);";;
  
let draw_thick_vertical_line (w:window_boundary_data) x0=
  let t1=tikzify_abscissa w x0
  and t2=tikz_from_paper ((function Tio i->i)(w.height)) in  
  "\\draw [thick] (O)++("^t1^",0) -- ++(0,"^t2^");";;  
  
let draw_thick_horizontal_lines (w:window_boundary_data) ly=
  let t2=tikz_from_paper ((function Tia i->i)(w.width)) 
  and lt=Image.image (tikzify_ordinate w) ly in
  let s1="\\foreach \\y in {"^(String.concat "," lt)^"}"
  and s2="   {"
  and s3="     \\draw [thick] (O)++(0, \\y) -- ++("^t2^", 0);"
  and s4="   };"  
  in
  String.concat "\n" [s1;s2;s3;s4];;
  
let draw_thick_vertical_lines (w:window_boundary_data) lx=
  let t2=tikz_from_paper ((function Tio i->i)(w.height)) 
  and lt=Image.image (tikzify_abscissa w) lx in
  let s1="\\foreach \\x in {"^(String.concat "," lt)^"}"
  and s2="   {"
  and s3="     \\draw [thick] (O)++(\\x, 0) -- ++(0, "^t2^");"
  and s4="   };"  
  in
  String.concat "\n" [s1;s2;s3;s4];;  
  
  
let draw_point  (w:window_boundary_data) (x,y)=
  let t1=tikzify_abscissa w x
  and t2=tikzify_ordinate w y
  in
  "\\draw (O)++("^t1^","^t2^") node {$\\bullet$};";;   
    
let epsilon_tikz_abscissa (w:window_boundary_data)=
      tikz_from_paper((paper_abscissa_of_abscissa w.dx_in_cm w.xmin (0.))+20);;
    
let epsilon_tikz_ordinate (w:window_boundary_data)=
      tikz_from_paper((paper_ordinate_of_ordinate w.dy_in_cm w.ymin (0.))+20);;
    
let negative_epsilon_tikz_abscissa (w:window_boundary_data)=
      tikz_from_paper((paper_abscissa_of_abscissa w.dx_in_cm w.xmin (0.))-20);;
    
let negative_epsilon_tikz_ordinate (w:window_boundary_data)=
      tikz_from_paper((paper_ordinate_of_ordinate w.dy_in_cm w.ymin (0.))-20);;    
    
let mark_abscissa (w:window_boundary_data) x yref=
   let t1=tikzify_abscissa w x 
   and t2=tikz_from_paper((paper_ordinate_of_ordinate w.dy_in_cm w.ymin yref)+20) in
   "\\draw[thick] (O)++("^t1^","^t2^") -- ++(0,-0.4);";;
   
let mark_ordinate (w:window_boundary_data) y xref=
   let t1= tikz_from_paper((paper_abscissa_of_abscissa w.dx_in_cm w.xmin xref)+20) 
   and t2=tikzify_ordinate w y in
   "\\draw[thick] (O)++("^t1^","^t2^") -- ++(-0.4,0);";;    
    
let shorten_float x=Decimal_number.to_string (Decimal_number.of_float 2 x);;     
    
let write_abscissa (w:window_boundary_data) x yref=
     let sx=shorten_float x in
     let t1=tikzify_abscissa w x
     and t2=tikz_from_paper((paper_ordinate_of_ordinate w.dy_in_cm w.ymin yref)-20)
     in
     "\\draw (O)++("^t1^","^t2^") node [fill=white,anchor=north] {$"^sx^"$};";;   
     
let write_ordinate (w:window_boundary_data) y xref=
     let sy=shorten_float y in
     let t1=tikz_from_paper((paper_abscissa_of_abscissa w.dx_in_cm w.xmin xref)-20)
     and t2=tikzify_ordinate w y
     in
     "\\draw (O)++("^t1^","^t2^") node [fill=white,anchor=east] {$"^sy^"$};";;   
     
let write_and_mark_abscissas w lx yref=
  let temp1=Image.image (fun x->
       if (max(x)(-.x)>0.00001) 
       then (write_abscissa w x yref)^"\n"^(mark_abscissa w x yref)
       else ""
  ) lx in
  String.concat "\n" temp1;;
  
let write_and_mark_ordinates w ly xref=
  let temp1=Image.image (fun y->
       if (max(y)(-.y)>0.00001) 
       then (write_ordinate w y xref)^"\n"^(mark_ordinate w y xref)
       else ""
  ) ly in
  String.concat "\n" temp1;;
     
let good_x_interval (w:window_boundary_data)=
  let x1=(function Mia(x)->x) (w.xmin)
  and x2=abscissa_of_paper_abscissa   w.dx_in_cm w.xmin w.width 
  and xstep=(function Tiau(x)->100.*.x) (w.dx_in_cm) in
  let j1=int_of_float(ceil(x1/.xstep)) 
  and j2=int_of_float(floor(x2/.xstep)) in
  Ennig.doyle (fun j->(float_of_int(j)*.xstep)) j1 j2;;
  
let good_y_interval (w:window_boundary_data)=
  let y1=(function Mio(y)->y) (w.ymin)
  and y2=ordinate_of_paper_ordinate   w.dy_in_cm w.ymin w.height
  and ystep=(function Tiou(y)->100.*.y) (w.dy_in_cm) in
  let j1=int_of_float(ceil(y1/.ystep)) 
  and j2=int_of_float(floor(y2/.ystep)) in
  Ennig.doyle (fun j->(float_of_int(j)*.ystep)) j1 j2;;
    
let slicy_x_interval (w:window_boundary_data)=
  let x1=(function Mia(x)->x) (w.xmin)
  and x2=abscissa_of_paper_abscissa   w.dx_in_cm w.xmin w.width 
  and xstep=(function Tiau(x)->10.*.x) (w.dx_in_cm) in
  let j1=int_of_float(ceil(x1/.xstep)) 
  and j2=int_of_float(floor(x2/.xstep)) in
  Ennig.doyle (fun j->(float_of_int(j)*.xstep)) j1 j2;;
  
let slicy_y_interval (w:window_boundary_data)=
  let y1=(function Mio(y)->y) (w.ymin)
  and y2=ordinate_of_paper_ordinate   w.dy_in_cm w.ymin w.height
  and ystep=(function Tiou(y)->10.*.y) (w.dy_in_cm) in
  let j1=int_of_float(ceil(y1/.ystep)) 
  and j2=int_of_float(floor(y2/.ystep)) in
  Ennig.doyle (fun j->(float_of_int(j)*.ystep)) j1 j2;;    
    

let is_x_centered (w:window_boundary_data)=
  let x1=(function Mia(x)->x) (w.xmin)
  and x2=abscissa_of_paper_abscissa   w.dx_in_cm w.xmin w.width  in
  (x1<=0.)&&(0.<=x2);;
  
let is_y_centered (w:window_boundary_data)=
  let y1=(function Mio(y)->y) (w.ymin)
  and y2=ordinate_of_paper_ordinate   w.dy_in_cm w.ymin w.height  in
  (y1<=0.)&&(0.<=y2);;

let write_good_abscissas w=
   let lx=good_x_interval w in
   if is_y_centered w
   then write_and_mark_abscissas w lx 0.
   else if w.forced_x_grid
        then let ymin=(fun (Mio y)->y)(w.ymin) in
             write_and_mark_abscissas w lx ymin 
        else "";;
        
let write_good_ordinates w=
   let ly=good_y_interval w in
   if is_x_centered w
   then write_and_mark_ordinates w ly 0.
   else if w.forced_y_grid
        then let xmin=(fun (Mia x)->x)(w.xmin) in
             write_and_mark_ordinates w ly xmin 
        else "";;        

let draw_x_axis (w:window_boundary_data)=
  let t1=tikz_from_paper 0 
  and t2=tikz_from_paper ((function Tia i->i)(w.width)+50) 
  and c=tikzify_ordinate w 0. in  
  "\\draw[thick,->] (O)++("^t1^","^c^") -- ++("^t2^",0);\n"^
  "\\draw (O)++("^t2^","^c^") node [fill=white,anchor=north west] {$x$};";;
  
let draw_y_axis (w:window_boundary_data)=
  let t1=tikz_from_paper 0
  and t2=tikz_from_paper ((function Tio i->i)(w.height)+50) 
  and c=tikzify_abscissa w 0. in  
  "\\draw[thick,->] (O)++("^c^","^t1^") -- ++(0,"^t2^");\n"^
  "\\draw (O)++("^c^","^t2^") node [fill=white,anchor=south east] {$y$};";;  

let write_origin (w:window_boundary_data)=
  let cx=tikzify_abscissa w 0.
  and cy=tikzify_ordinate w 0. in
  "\\draw (O)++("^cx^","^cy^") node [fill=white,anchor=north west] {$O$};";;  

let surroundings (w:window_boundary_data)=
  let cx=is_x_centered w
  and cy=is_y_centered w in
  let xii=good_x_interval w
  and yii=good_y_interval w in
  let s1="\\path (0,0) node (O) {};"
  and s2=(function ()->
     if w.forced_graph_paper
     then let xjj=slicy_x_interval w
          and yjj=slicy_y_interval w in
         (draw_horizontal_lines w yjj)^"\n"^
         (draw_vertical_lines w xjj)
     else ""     
  )() 
  and s3=(draw_thick_horizontal_lines w yii)^"\n"^
         (draw_thick_vertical_lines w xii)
  and s4=write_good_abscissas w         
  and s5=write_good_ordinates w
  and s6=Basic.functional_if(cx&&cy,draw_point w (0.,0.),"")
  and s7=Basic.functional_if(cx,draw_y_axis w,"") 
  and s8=Basic.functional_if(cy,draw_x_axis w,"") 
  and s9=Basic.functional_if(cx&&cy,write_origin w,"") 
  in
  String.concat "\n" [s1;s2;s3;s4;s5;s6;s7;s8;s9];;
  

let curve_from_list_of_points  (w:window_boundary_data) l=
   let temp1=Image.image (
      fun (x,y)->
        (paper_abscissa_of_abscissa w.dx_in_cm w.xmin x,
         paper_ordinate_of_ordinate w.dy_in_cm w.ymin y)
   ) l in
   let (p1,p2)=List.hd(temp1) and temp2=Listennou.universal_delta_list temp1 in
   let temp3=Image.image (fun ((x1,y1),(x2,y2))->(x2-x1,y2-y1) ) temp2 in  
   let temp6=List.filter (fun z->z<>(0,0)) temp3 in
   let tfp2=(fun (ix,iy)->(tikz_from_paper ix,tikz_from_paper iy)) in
   let (t1,t2)=tfp2(p1,p2) and temp4=Image.image tfp2 temp6 in
   let temp5=String.concat "" (Image.image (fun (t3,t4)->
   " -- ++("^t3^","^t4^")") temp4) in
   "\\draw [thick] (O)++("^t1^","^t2^")"^temp5^";";; 
  
let curves_from_lists_of_points  (w:window_boundary_data) ll=
   let temp1=Image.image (
      curve_from_list_of_points w
   ) ll in
  String.concat "\n" temp1;; 
  
let bounds_of_window (w:window_boundary_data)=
  let (xmin,ymin)=coordinates_of_paper_coordinates w (Tia 0,Tio 0)
  and (xmax,ymax)=coordinates_of_paper_coordinates w (w.width,w.height) in
  (xmin,xmax,ymin,ymax);;
  
let check_point (w:window_boundary_data) (x,y)=
  let (xmin,xmax,ymin,ymax)=bounds_of_window w in
  (xmin<=x)&&(x<=xmax)&&(ymin<=y)&&(y<=ymax);;
  
   
let complete_graph_from_lists_of_points (w:window_boundary_data) ll=
  let temp1=Image.image (Option.filter_and_test (check_point w)) ll in
  let temp2=List.flatten temp1 in
  (surroundings w)^"\n"^(curves_from_lists_of_points w temp2);;
     
let default_step fl=
  let p=int_of_float(floor((log(fl))/.(log(10.)))) in
  let d=Decimal_number.of_float 7 fl in
  let x=Decimal_number.mult d (Decimal_number.power_of_ten (-p)) in
  let tf=(fun s->Decimal_number.geq x (Decimal_number.of_string s)) in
  if tf"7.5"   then "0.75"   else
  if tf"5"     then "0.5"    else
  if tf"4"     then "0.4"    else
  if tf"3.75"  then "0.375"  else 
  if tf"2.5"   then "0.25"   else
  if tf"2"     then "0.2"    else 
  if tf"1.875" then "0.1875" else "0.1";;
  
let compute_step fl opt=
   if opt="" then default_step fl else opt;;   
   
let compute_x_left_offset l=
  match Option.find_and_stop 
    (function (X_left_offset d)->Some d |_->None) l with
  None->0.
  |Some(d0)->d0;;
  
let compute_x_right_offset l=
  match Option.find_and_stop 
    (function (X_right_offset d)->Some d |_->None) l with
  None->0.
  |Some(d0)->d0;;  
  
let compute_y_lower_offset l=
  match Option.find_and_stop 
    (function (Y_lower_offset d)->Some d |_->None) l with
  None->0.
  |Some(d0)->d0;;
         
let compute_y_upper_offset l=
  match Option.find_and_stop 
    (function (Y_upper_offset d)->Some d |_->None) l with
  None->0.
  |Some(d0)->d0;;         
         
let compute_x_min xrange l=
    match Option.find_and_stop 
    (function (Forced_X_min x)->Some x |_->None) l with
  None->Min.list(xrange)-.compute_x_left_offset(l)
  |Some(x)->x;;     
   
let compute_x_max xrange l=
    match Option.find_and_stop 
    (function (Forced_X_max x)->Some x |_->None) l with
  None->Max.list(xrange)+.compute_x_right_offset(l)
  |Some(x)->x;;     
   
let compute_y_min yrange l=
    match Option.find_and_stop 
    (function (Forced_Y_min y)->Some y |_->None) l with
  None->Min.list(yrange)-.compute_y_lower_offset(l)
  |Some(y)->y;;        
   
let compute_y_max yrange l=
    match Option.find_and_stop 
    (function (Forced_Y_max y)->Some y |_->None) l with
  None->Max.list(yrange)+.compute_y_upper_offset(l)
  |Some(y)->y;;        
   
   
let window_from_list_of_points l (opt_x,opt_y) opt_stuff=
   let xrange=Image.image fst l and yrange=Image.image snd l in
   let xmin0=compute_x_min xrange opt_stuff
   and xmax0=compute_x_max xrange opt_stuff
   and ymin0=compute_y_min yrange opt_stuff
   and ymax0=compute_y_max yrange opt_stuff in
   let dx=xmax0-.xmin0
   and dy=ymax0-.ymin0 in
   let dx_in_cm0=float_of_string(compute_step dx opt_x)*.0.01
   and dy_in_cm0=float_of_string(compute_step dy opt_y)*.0.01 in
   let  xwidth=int_of_float(ceil(dx/.dx_in_cm0))
   and yheight=int_of_float(ceil(dy/.dy_in_cm0)) in
    {
  width =Tia xwidth; 
  height =Tio yheight; 
  xmin =Mia xmin0; 
  ymin =Mio ymin0; 
  dx_in_cm =Tiau dx_in_cm0;
  dy_in_cm =Tiou dy_in_cm0;
  forced_graph_paper=List.mem Forced_graph_paper opt_stuff;
  forced_x_grid=List.mem Forced_X_grid opt_stuff;
  forced_y_grid=List.mem Forced_Y_grid opt_stuff;
};;
   
(*
   


     
 *)  
  
