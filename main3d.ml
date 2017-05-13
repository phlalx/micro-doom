open Geometrie

let compute_new_cam game key =
  let cam = game.Game3d.cam in
  let module C = Camera in
  let module G = Geom3d in
    match key with
      | '8' | 'k' -> C.translate_cam cam 
	  (G.homothetie (-1. *. game.Game3d.pas) (C.get_w cam))
      | '2' | 'j' -> C.translate_cam cam 
	  (G.homothetie game.Game3d.pas (C.get_w cam))
      | '4' | 'h' -> C.rotate_v cam game.Game3d.angle
      | '6' | 'l' -> C.rotate_v cam (-1. *. game.Game3d.angle)
      | 'q'  -> C.eloigne_ecran cam 1.1 
      | 'w'  -> C.eloigne_ecran cam 0.9
      | _ -> cam 

let collision game cam1 cam2 =
  let obs1 = Camera.get_obs cam1
  and obs2 = Camera.get_obs cam2
  and omega = Camera.get_omega cam2
  and omega2 = Camera.get_omega2 cam2
  in (Bsp.Bsp3d.collision game.Game3d.bsp obs1 obs2) 
     or (Bsp.Bsp3d.collision game.Game3d.bsp obs2 omega) 
     or (Bsp.Bsp3d.collision game.Game3d.bsp obs2 omega2) 

let monde game = Bsp.Bsp3d.painter 
		   game.Game3d.bsp
		   (Camera.get_obs game.Game3d.cam)  
      
let affiche_debug cam =
  print_string (Camera.to_string cam ^ "\n"); flush stdout

let rec jeu game =
  Affichage.display_polygones game.Game3d.lum game.Game3d.cam (monde game);
  let key = Graphics.read_key () in 
  let cur_cam = game.Game3d.cam in
  let new_cam = compute_new_cam game key in
  let c = collision game cur_cam new_cam in 
  let new_cam = if c then cur_cam else new_cam 
  in jeu { game with Game3d.cam = new_cam } 
	
let _ = 
  Affichage.init_display ();
  jeu Game3d.game  

