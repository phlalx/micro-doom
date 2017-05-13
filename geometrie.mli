type side = Gauche | Droite | Intersecte

module type Geom =
  sig

    (* on assimile point et vecteur *)
    type point 
      
    type polygone = {
      forme : point array;
      normal : point;
      centre : point;
      couleur : Graphics.color;
    } 

    val polygone_of_array : point array -> Graphics.color -> polygone

   (* precision sera utilise dans cote_point, on considere qu'un point
      intersecte une droite (ou un plan en 3D) si il est à distance
      inférieur à precision de cette droite (ou plan)*)
    val precision : float ref 

    val somme : point -> point -> point

    val difference : point -> point -> point

    val homothetie : float -> point -> point

    val prod_scal : point -> point -> float 

    (* vecteur normal a un ensemble de points coplanaires *)
    val normal : point array -> point

    (* isobarycentre d'un ensemble de points *)
    val barycentre : point array -> point

    (* intersection entre la droite (a,b) et le plan du polygone p
       on suppose que prod_scal (diff b a) p.normal <>0            *)
    val intersection : polygone -> point * point -> point


    (*  renvoie false ssi l'intersection entre les deux points et le polygone   *)
    (*    est vide                                                                *)
    val intersection_segment : polygone -> point * point -> bool

    (*  renvoie Gauche si le point x est à gauche du polygone p,droite 
        s'il est a droite, Intersecte sinon. L'orientation Droite/Gauche 
        est prise, par rapport a l'orientation du polygone p             *)
    val cote_point : polygone -> point -> side

    (* renvoie Gauche si le polygone p est à gauche du polygone separateur,
       droite s'il est a droite, Intersecte sinon. L'orientation Droite/Gauche
       est prise, par rapport a l'orientation du polygone separateur    *)
    val cote_poly : polygone -> polygone -> side

    (* coupe le polygone p en plusieurs polygones (lp_g,lp_d) tels que les
       polygones de lp_g sont à gauche de separateur et ceux de lp_d à droite.
       on suppose ici que p est convexe.  *)
    val separe : polygone -> polygone -> polygone list * polygone list
    

    (* effectue une rotation d'un vecteur selon l'axe Oz *)
    val rotation_v : float -> point -> point
 
    val point_to_string : point -> string
  end

module Geom3d : Geom with type point = float * float * float
module Geom2d : Geom with type point = float * float

module Geom2d3d : sig

  (* projection d'un point sur le plan Oxy parallelement a l'axe Oz *)
  val proj : Geom3d.point -> Geom2d.point  

  (* transforme un segment 2d en un polygone 3d en lui donnant une hauteur de z0 a z1  *)
  (* ou z0 est le premier argument et z1 le deuxieme                                   *)
  val threeDfy : float -> float -> Geom2d.polygone -> Geom3d.polygone      

end
