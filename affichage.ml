open Geometrie

type luminosite = { ambiante: float; source: float }

let init_display _ = 
  Graphics.open_graph" 500x500+50-50";
  Graphics.auto_synchronize false

(* renvoie les trois composantes RGB *)
let get_rgb couleur =
  (float_of_int ((couleur/(256*256)) mod 256),
   float_of_int ((couleur/256) mod 256),
   float_of_int (couleur mod 256));;

(* renvoie la couleur d'un polygone en fonction de l'éclairage.
   elle depend de la couleur initiale, de la luminosite ambiante, de la
   distance à la source ponctuel (confondue avec le point de fuite de la
   camera) et de l'angle avec lequel un rayon partant de la source rencontre
   le polygone.*)
let couleur lum cam (p: Geom3d.polygone) =
  let co = Geom3d.difference (Camera.get_omega cam) p.Geom3d.centre
  and n = p.Geom3d.normal in
  let norme_co = sqrt (Geom3d.prod_scal co co) in
  let cos_nco = abs_float ( (Geom3d.prod_scal n co) /.norme_co) in
  let coeff = min
		1. 
		((lum.ambiante)+. (lum.source)*.cos_nco/.(1.+.norme_co))
  and (r, v, b) = get_rgb p.Geom3d.couleur
  in
    Graphics.rgb
      (max 0 (int_of_float (r*.coeff)))
      (max 0 (int_of_float (v*.coeff)))
      (max 0 (int_of_float (b*.coeff)));;

(* renvoie les coordonnee d'un point 3D dans le repère de la caméra *)
let proj_point cam x =
  let vect_u = Camera.get_u cam 
  and vect_v = Camera.get_v cam 
  and vect_w = Camera.get_w cam 
  and omega = Camera.get_omega cam 
  and pt_de_fuite = Camera.get_obs cam in
  let screen_plane =
    { Geom3d.forme = [||]; normal = vect_w; centre = omega; couleur = -1} in
  let res_3d_coord=Geom3d.intersection screen_plane (pt_de_fuite, x) in
    (Geom3d.prod_scal (Geom3d.difference res_3d_coord omega) vect_u,
     Geom3d.prod_scal (Geom3d.difference res_3d_coord omega) vect_v)
      
let proj_poly cam p =
  let vect_w = Camera.get_w cam 
  and omega = Camera.get_omega cam in
  let screen_plane =
    { Geom3d.forme = [||]; normal = vect_w; centre = omega; couleur = -1 } in
    match Geom3d.cote_poly screen_plane p with
      | Droite-> [||]
      | Gauche-> Array.map (proj_point cam) p.Geom3d.forme
      | Intersecte->
	  let (lp_g,_) = Geom3d.separe screen_plane p in
	    Array.map (proj_point cam) (List.hd lp_g).Geom3d.forme

let to_graph cam p = 
  let sizeX = Camera.get_size_x cam
  and sizeY = Camera.get_size_z cam in
  let r_x = (float_of_int (Graphics.size_x ())) /. sizeX
  and r_y = (float_of_int (Graphics.size_y ())) /. sizeY 
  and point_array = proj_poly cam p in
  let proj (x,y) = int_of_float (r_x *. x), int_of_float (r_y*.y) in
    Array.map proj point_array
      
let display_polygone lum cam p =
  Graphics.set_color (couleur lum cam p);
  Graphics.fill_poly (to_graph cam p) 
    
let display_polygones lum cam m = 
  Graphics.clear_graph ();
  ignore (List.map (display_polygone lum cam) m);
  Graphics.synchronize ()
