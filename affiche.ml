open Jeu;;
open Solution;;
(********************************************************************)
                (*INTERACTION avec l'utilisateur*)

(* matrice de couleur 
    int array array -> voronoi -> Graphics.color option array array 
*)
let matrice_couleur m_r v =
  let w, h = v.dim in
  let mc = Array.make_matrix h w (Some Graphics.white) in
  for i = 0 to ( h-1 ) do
    for j = 0 to ( w-1 ) do
      mc.(i).(j) <- v.seeds.(m_r.(i).(j)).c
    done
  done;
mc
;;
(* tableau de seed 
   matrice_seed : voronoi -> seed array 
*)
let matrice_seed v = 
  let w = Array.length v.seeds in 
  let m = Array.make w (v.seeds.(0)) in 
  for i = 1 to (w-1) do
    m.(i) <- v.seeds.(i)
  done;
  m
;;
(* changer la couleur d'une région 
   int -> int -> Graphics.color -> seed array -> voronoi -> unit =
*)
let color_rs a b color m_s v = 
  let c_s = (closest_seed (a,b) v) in 
  begin
   (* m.(c_s).c;*)
    m_s.(c_s) <- {c = Some color;x = v.seeds.(c_s).x;y = v.seeds.(c_s).y};
    (*m_s.(c_s).c;*)
  end
;;
(*gerer les clics de la souris 
  Graphics.color -> seed array -> int array array -> voronoi -> unit *)
let rec next_move cou m_s m_r v =
  Graphics.auto_synchronize false ;
  let c_c = ref cou in
  let a = Graphics.wait_next_event[ Button_down; Key_pressed] in
  let b = Graphics.wait_next_event [ Button_up; Key_pressed] in 
  let x= a.mouse_x and y= a.mouse_y in
    match  m_s.(m_r.(x).(y)).c with
      None -> c_c := Graphics.white 
    | Some c -> c_c :=  c;
     (*rintf.printf "%d %d\n" x y;*) 
	begin
	 (* Printf.printf "%d %d\n" b.mouse_x b.mouse_y;*)
	  color_rs b.mouse_x b.mouse_y !c_c m_s v;
	  draw_voronoi m_s m_r v;
	  Graphics.synchronize();
	end;
      next_move !c_c m_s m_r v;;

