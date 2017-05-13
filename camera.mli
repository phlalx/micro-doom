(* t est le type des camera
   une camera est caracterisee par la position de l'observateur obs 
   un ecran defini
       par un repere (u,v,w), 
       sa taille selon x et selon z
       sa distance par rapport a l'ecran
       omega1 vecteur obs -> coin inferieur gauche de l'ecran 
       omega1 vecteur obs -> coin superieur droit l'ecran                 *)

type t 


(* make: observateur -> distance obs-ecran(d) -> sizeX -> sizeZ -> camera *)
(* si obs = (x,y,z), l'ecran est centre sur (x,y+d,z)                     *)
val make : Geometrie.Geom3d.point -> float -> float -> float -> t 

(* initialement u = (1,0,0)                                               *)
val get_u : t -> Geometrie.Geom3d.point

(* initialement v = (0,0,1)                                               *)
val get_v : t -> Geometrie.Geom3d.point

(* initialement w = (0,-1,0)                                              *)
(* attention : w est dirige vers l'observateur                            *)
val get_w : t -> Geometrie.Geom3d.point 

val get_obs : t -> Geometrie.Geom3d.point

(* omega est le coin en bas a gauche   de l'ecran                         *)
val get_omega : t -> Geometrie.Geom3d.point

(* omega2 est le coin en haut a droite de l'ecran                         *)
val get_omega2 : t -> Geometrie.Geom3d.point

(* taille de l'ecran suivant l'axe x                                      *)
val get_size_x : t -> float

(* taille de l'ecran suivant l'axe z                                      *)
val get_size_z : t -> float

val translate_cam : t -> Geometrie.Geom3d.point -> t  

(* rotation d'une camera selon l'axe z *)
val rotate_v : t -> float -> t 

(* eloigne l'ecran de l'observateur *)
val eloigne_ecran : t -> float -> t 

val to_string : t -> string

