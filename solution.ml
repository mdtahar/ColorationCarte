open Jeu;;
open Sat_solver;;
(* Convertie une couleur en int
   couleur -> int
 *)
let conv_int c =
  if c = Graphics.yellow then 3
  else if c = Graphics.blue then 0
  else if c = Graphics.green then 1
  else if c = Graphics.red then  2
  else -1
;;

(*convertie un int en couleur*)
let conv_couleur  i = 
  let c = ref (Some Graphics.white) in
  if i = 3 then c := Some Graphics.yellow
  else if i = 0 then c := Some Graphics.blue
  else if i = 1  then c :=  Some Graphics.green
  else if i = 2 then c :=  Some Graphics.red
  else  c := Some Graphics.white;
 !c;;
(*convertie un tableau de int en tableau de couleur*)
let conv_tableau_couleur t = 
  let taille = Array.length t in 
  let r = Array.make taille (Some Graphics.white) in 
  for i = 0 to taille-1 do 
    begin
      r.(i) <- conv_couleur  t.(i);
    end
  done;
  r;;
(*Ajoute si une couleur est presente dans le tableau*)
let ajout_couleur v l=
let liste = ref l in
let x = Array.length v.seeds in
  for i = 0 to x -1 do
    match v.seeds.(i).c with
    | None -> ()
    |Some c ->  liste := [[(true,(i,(conv_int c)))]]@(!liste);
  done;
!liste
;;


(*Calcul les adjacance de chaque région
return la matrice des adjacence*)
let adjacanes_voronoi m_r v =
  let n = Array.length v.seeds in
  let m_a = Array.make_matrix n n false in
  let w, h = v.dim in
  for i = 0 to h - 1  do
    for j = 0 to w - 1 do
      List.iter
	(fun (vi, vj) ->
          let seed1 = m_r.(i).(j) in
	  let seed2 = m_r.(vi).(vj) in
          m_a.(seed1).(seed2) <- true)
	(voisins_frontiere m_r (i, j) w h)
    done
  done;
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i = j then
        m_a.(i).(j) <- true
    done
  done;
  m_a
;;

let region_a_une_couleur m_a =
  let taille = Array.length m_a in
  let rec aux i l=
    if i = taille then l
    else
     aux (i+1)  ([[(true,(i,1));(true,(i,2));(true,(i,3));(true,(i,0))]]@l)
  in aux 0 []
;;
let region_a_une_couleur_n m_a n =
  let taille = Array.length m_a in
  let rec aux i l=
    if i = taille -1 then l
    else 
      begin
        if n = 4 then
          aux (i+1)  ([[(true,(i,1));(true,(i,2));(true,(i,3));(true,(i,0))]]@l)
        else if n=3 then
          aux (i+1)  ([[(true,(i,1));(true,(i,2));(true,(i,0))]]@l)
        else
           aux (i+1)  ([[(true,(i,1));(true,(i,0))]]@l)
      end
  in aux 0 []
;;

let region_au_plus_couleur m_a =
  let taille = Array.length m_a in
  let rec aux r l =
    if r = taille -1  then l
    else
        aux (r+1) [[(false,(r,0));(false,(r,2))];
                   [(false,(r,0));(false,(r,3))];
                   [(false,(r,0));(false,(r,1))];
                   [(false,(r,1));(false,(r,2))];
                   [(false,(r,1));(false,(r,3))];
                   [(false,(r,2));(false,(r,3))]]@l
  in aux 0  []
;;

let region_adjacence m_a =
  let taille = Array.length m_a in
  let l = ref [] in
  for i = 0 to taille -1 do
    for j = 0 to taille -1 do
      for x = 0 to 3 do
       (* if i=j then
          l :=!l
        else*)
        if i<> j && m_a.(i).(j) then
          l := !l@[[(false,(i,x));(false,(j,x))]]
          (*else if i=j then *)
      done
    done
  done;
    !l
;;

(* fonction qui vérifie si le voronoi est conforme a la solution *)
let verifie_voronoi m_s l =
  let rec aux l' acc =
    match l' with
    | [] -> acc
    | x::q ->
      begin
        let b,(h,c) = x in
        if b then aux q (acc &&( (m_s.(h).c) = (conv_couleur c)))
        else
          aux q (acc && not( (m_s.(h).c) = (conv_couleur c)))
      end
  in
  match l with
  |None -> failwith "Il n'y a pas de solution"
  |Some l' -> aux l' true
;;

let contact_list m_a = (region_adjacence m_a)
                       @(region_au_plus_couleur m_a)
                       @(region_a_une_couleur m_a)
;;
let calcul_solution m_a v=
  Sat.solve (ajout_couleur v (contact_list m_a))
;;

let rec auxtest t u =
      match  u with
      | [] -> ()
      | (a,(b,c))::q->(
        if a then
          t.(b) <- c;
        auxtest t  q)
;;
let complete_voronoi l v =
  let x = (Array.length v.seeds) in
  let t = Array.make x  5  in
  match l with
  | None -> failwith " Aucune solution possible"
  | Some l' ->
    (*let y = Array.length l'  in*)
    (let rec aux u =
      match  u with
      | [] -> ()
      | (a,(b,c))::q->(
        if a then
          t.(b) <- c;
        aux q)
    in aux l');
    (*il n'y a pas de couleur par defaut *)
    (*Les solutions du sat sont trié dans l'ordre*)
    for i=0 to x-1 do
      let couleur = ref 0 in
      if t.(i)  = (Graphics.white) then
        let rec aux q=
          match q with
          | [] -> ()
          | (a,(b,c))::q'-> if not a && b = i && c = !couleur then  couleur := !couleur +1;
            t.(b) <- !couleur; aux q';
        in aux l'
    done;
    t
;;
let complete_voronoi l v =
  let x = (Array.length v.seeds) in
  let t = Array.make x  5  in
  match l with
  | None -> failwith " Aucune solution possible"
  | Some l' ->
    (*let y = Array.length l'  in*)
    (let rec aux u =
      match  u with
      | [] -> ()
      | (a,(b,c))::q->(
        if a then
          t.(b) <- c;
        aux q)
    in aux l');
    (*il n'y a pas de couleur par defaut *)
    (*Les solutions du sat sont trié dans l'ordre*)
    for i=0 to x-1 do
      let couleur = ref 0 in
      if t.(i)  =  Graphics.white then
        let rec aux q=
          match q with
          | [] -> ()
          | (a,(b,c))::q'-> if not a && b = i && c = !couleur then  couleur := !couleur +1;
            t.(b) <- !couleur; aux q';
        in aux l'
    done;
    t
;;
(* Je returne un tableau de seeds avec les couleurs completer
 * m = matrice region -> v = voronoi -> array color
*)
let list_to_array l =
  let rec aux l2 a i =
    match l2 with
    | [] -> a
    | x::q -> a.(i) <- x; aux q a (i+1)
  in
  match l with
  |None -> None
  |Some l' -> let a  = Array.make (List.length l') (false,(0,0)) in
    Some (aux l' a 0)
;;
let draw_voronoi_solution t_c m_r v = 
  let w, h = v.dim in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do 
      if (voisins_frontiere m_r (i, j) w h) <> []
      then Graphics.set_color Graphics.black
      else
	begin
	  match t_c.(m_r.(i).(j)) with
	    None -> Graphics.set_color Graphics.white 
	  | Some c -> Graphics.set_color c;
	end;
      Graphics.plot i j;
    done
  done
;;
