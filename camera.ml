open Geometrie

type t = {
  omega: Geom3d.point;  (* coin inferieur gauche de l'ecran*)
  vect_u: Geom3d.point; (* repere de *)
  vect_v: Geom3d.point; (* l'ecran   *)
  vect_w: Geom3d.point; (*normal à l'écran*)
  pt_de_fuite: Geom3d.point;
  size_x: float;
  size_z: float;
}

let get_u c = c.vect_u
let get_v c = c.vect_v
let get_w c = c.vect_w
let get_size_x c = c.size_x
let get_size_z c = c.size_z
let get_obs c = c.pt_de_fuite
let get_omega c = c.omega
let get_omega2 c = Geom3d.somme c.omega (Geom3d.homothetie c.size_x c.vect_u)

let vect_u = 1.,0.,0.
let vect_v = 0.,0.,1.
let vect_w = 0.,-1.,0.

let make obs dist size_x size_z =   
  let offset = size_x /. 2. , 0., size_z /. 2. in   
  let omega =
    Geom3d.difference 
      (Geom3d.difference obs (Geom3d.homothetie dist vect_w)) 
      offset
  in 
    { omega = omega;
      vect_u = vect_u; 
      vect_v = vect_v; 
      vect_w = vect_w; 
      pt_de_fuite = obs; 
      size_x = size_x;
      size_z = size_z;
    }
           
let translate_cam cam trans =
  { cam with 
      omega = Geom3d.somme cam.omega trans;
      pt_de_fuite = Geom3d.somme cam.pt_de_fuite trans; }

let get_offset_omega cam  =
  let size_x = cam.size_x /. -2.
  and size_z = cam.size_z /. -2. in
  let offset = 
    Geom3d.somme
      (Geom3d.somme
	 (Geom3d.homothetie size_x cam.vect_u)
	 (Geom3d.homothetie size_z cam.vect_v))
      cam.pt_de_fuite
  in  
  let omega_center = Geom3d.difference cam.omega offset in
    (offset,omega_center)
      
let rotate_v cam alpha =
  { cam with 
      vect_u = Geom3d.rotation_v alpha cam.vect_u;
      vect_w = Geom3d.rotation_v alpha cam.vect_w;
      omega =  
      Geom3d.somme cam.pt_de_fuite 
	(Geom3d.rotation_v alpha (Geom3d.difference cam.omega cam.pt_de_fuite)) ; 
 }

let eloigne_ecran cam f =
  let offset, omega_center = get_offset_omega cam in 
    { cam with omega = Geom3d. somme offset (Geom3d.homothetie f omega_center) }
      
let to_string cam =
  "observateur : " ^ (Geom3d.point_to_string cam.pt_de_fuite) ^ " " ^
  "omega : " ^ (Geom3d.point_to_string cam.omega)
