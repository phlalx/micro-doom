open Geometrie

let compute_new_cam game key =
  let cam = game.Game2d.cam in
  let module C = Camera in
  let module G = Geom3d in
    match key with
      | '8' | 'k' -> C.translate_cam cam 
	  (G.homothetie (-1. *. game.Game2d.pas) (C.get_w cam))
      | '2' | 'j' -> C.translate_cam cam 
	  (G.homothetie game.Game2d.pas (C.get_w cam))
      | '4' | 'h' -> C.rotate_v cam game.Game2d.angle
      | '6' | 'l' -> C.rotate_v cam (-1. *. game.Game2d.angle)
      | 'q'  -> C.eloigne_ecran cam 1.1 
      | 'w'  -> C.eloigne_ecran cam 0.9
      | _ -> cam 

let collision game cam1 cam2 =
  let obs1 = Geom2d3d.proj (Camera.get_obs cam1)
  and obs2 = Geom2d3d.proj (Camera.get_obs cam2)
  and omega = Geom2d3d.proj (Camera.get_omega cam2)
  and omega2 = Geom2d3d.proj (Camera.get_omega2 cam2)
  in (Bsp.Bsp2d.collision game.Game2d.bsp obs1 obs2) 
     or (Bsp.Bsp2d.collision game.Game2d.bsp obs2 omega) 
     or (Bsp.Bsp2d.collision game.Game2d.bsp obs2 omega2) 

let monde game = 
  let painter2d = Bsp.Bsp2d.painter 
      game.Game2d.bsp 
      (Geom2d3d.proj (Camera.get_obs game.Game2d.cam)) 
  in List.map (Geom2d3d.threeDfy game.Game2d.z_bot game.Game2d.z_top) painter2d 
      
let affiche_debug cam = print_string (Camera.to_string cam ^ "\n"); flush stdout

let rec jeu game =
  Affichage.display_polygones game.Game2d.lum game.Game2d.cam (monde game);
  let key = Graphics.read_key () in 
  let cur_cam = game.Game2d.cam in
  let new_cam = compute_new_cam game key in
  let c = collision game cur_cam new_cam in 
  let new_cam = if c then cur_cam else new_cam 
  in jeu { game with Game2d.cam = new_cam } 
	
let _ = 
  Affichage.init_display ();
  jeu (Game2d.game "game")  

(* monde 3d:
let monde game = Bsp.Bsp3d.painter game.bsp (Affichage.get_obs game.cam)  
*)
