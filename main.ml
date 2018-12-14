open Jeu;;
open Solution;;
open Affiche;;
open Graphics;;
open Example;;

let main() =
  let r = Random.int (List.length list)+1 in 
  let v = List.nth list r in 
  let hl,lh = v.dim in 
  let m_s = matrice_seed v in  
  let m = calcul_voronoi v in 
  let m_a = adjacanes_voronoi m v in
  let s = calcul_solution m_a v in
  let juste = verifie_voronoi m_s s in
  while not(juste) do 
    Graphics.set_window_title "4 Couleurs  !";
    open_graph(" "^(string_of_int (hl))^"x"^( string_of_int (lh)));
    auto_synchronize false ;
    draw_voronoi m_s m v ;
    synchronize();
    next_move (Graphics.red) m_s m v;

    if Graphics.key_pressed () then
      match Graphics.read_key () with
      | 'S' -> let c = complete_voronoi s v in
	       let t_c = conv_tableau_couleur c in 
	       draw_voronoi_solution t_c m v
      | 'Q' -> Graphics.close_graph()
      | _ -> ();
		
  done;
  Graphics.draw_string " vous avez gagné ! ";;


main();;

