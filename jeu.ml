open Graphics;;
open Sat_solver;;
(*Initialisation des typde*)
type dir  = H | G | D | B
;;

type seeds = 
  { c : color option;
    x : int;
    y : int;
  }
;;
type voronoi = 
  { dim : int * int;
    seeds : seeds array
  }
;;
module Variables =struct
    type t = int*int
    let compare = compare
  end
;;
module Sat = Sat_solver.Make(Variables)
;;
(************************************************************************)
(*fonction*)

(*distance_euclide : calcul la distance euclidienne
(int , int) -> (int, int) -> int *)

let distance_euclide (x,y) (u,v) = (x - u) *  (x - u)  + (y - v) * (y - v);;
let distance_taxicab (x,y) (u,v) =
  let a = if x - u < 0 then ( x - u)*1 else x - u in
  let b = if y - v < 0 then ( y - v)*1 else y - v in
  a + b
;;

(*d
  La fonction qui calcule un euclide a partir d'une seed.
  array array -> seed -> array aray
*)

(* retourne le mimimun pour la position
   int * int -> voronoi -> int
*)
let closest_seed (x, y) v =
  let rec aux (x, y) i d_min cs =
    if i = Array.length v.seeds then cs
    else
      let d_e =
	distance_euclide (x,y)
	  (v.seeds.(i).x, v.seeds.(i).y) in
      if  i = 0 || d_e < d_min then
        aux (x,y) (i+1) d_e i
      else
        aux (x,y) (i+1) d_min cs
  in aux (x,y) 0 0 0

;;

(* matrice de region *)
let calcul_voronoi v =
  let w, h  = v.dim in
  let m = Array.make_matrix h w 0 in
  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      m.(i).(j) <- closest_seed (i, j) v
    done
  done;
  m
;;
(* fonction obligatoire :
   Affiche le diagramme de voronoi
   m_p : matrice de pixel
   v : Voronoi
   array array -> voronoi -> unit
 *)

(* defini une matrice d'adjaceance a partir d'un voronoi. *)

let next (i, j) dir = match dir with
  |H  -> (i - 1, j)
  |G  -> (i, j - 1)
  |D  -> (i, j + 1)
  |B  -> (i + 1, j)
;;

let no_depass (i, j) w h =
  not (i < 0 || j < 0 || i >= h || j >= w )
;;
let check_dir (i,j) dir w h =
  let ni, nj = next (i, j) dir in
  if(no_depass (ni,nj) w h)
  then (ni, nj)
  else
   (i, j)
;;
let nombre_de_couleur v =
  let c_bleu  = ref false
  and c_jaune = ref false
  and c_rouge = ref false
  and c_vert  = ref false in
  let x = ref 0 in
  for i =0 to (Array.length v.seeds) -1 do
    match v.seeds.(i).c with
    | None -> ()
    | Some color ->
      if color = blue then c_bleu := true
      else if color = yellow then c_jaune := true
      else if color = red then c_rouge := true
      else if color = green then c_vert := true
      else
        ()
  done;
  if !c_bleu then x:= !x+1;
  if !c_jaune then x := !x+1;
  if !c_rouge then x := !x+1;
  if !c_vert then x := !x+1;
  !x
;;
(* Calcul si le pixel est une frontiere.
   m_p = Matrice de pixel
   (i,j) = coordonne du pixel
   x_max  et  y_max = taille max du tableau
 * True si il y a une autre région a coté False sinon
 *)
let voisins_frontiere m_p (i,j) w h =
  let c = m_p.(i).(j) in
  let lcv =
    List.map (fun d -> check_dir(i,j) d w h)
      [H; B; G; D] in
  List.filter (fun (vi, vj) -> m_p.(vi).(vj) <> c) lcv
;;
(*
 * Donne la direction d'ou ce citue la frontire
 * return une direction
*)
let dir_frontiere m_p (i, j) w h =
  let c = m_p.(i).(j) in
  if (check_dir(i,j) H w h <> c) then H
  else if(check_dir(i,j) G w h <> c ) then G
  else if(check_dir(i,j) D w h <> c ) then D
  else B
;;
let draw_frontiere m_p =
  let x_max = Array.length m_p in
  let y_max = Array.length m_p.(0) in
  for i = 0 to x_max  do
    for j = 0 to y_max do
      if (voisins_frontiere m_p (i,j) x_max y_max) <> [] then
        begin
          set_color black;
          draw_rect i j 1 1;
        end
    done;
  done;;

(* Dessine le voronoi
 * m_p = matrice pixel
 * v = voronoi
 * return unit
*)
let draw_voronoi m_s m_r v = 
  let w, h = v.dim in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do 
      if (voisins_frontiere m_r (i, j) w h) <> []
      then set_color black
      else
	begin
	  match m_s.(m_r.(i).(j)).c with
	    None -> set_color white 
	  | Some c -> set_color c;
	end;
      plot i j;
    done
  done
;;

let draw_point_voronoi v =
  for i = 0 to Array.length v.seeds do
    fill_ellipse v.seeds.(i).x v.seeds.(i).y 2 2
  done;;
