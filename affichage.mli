type luminosite = { ambiante : float; source : float }

(* initialisation de l'affichage - ouvre une fenetre graphiquei             *) 
(* doit etre appele avant les fonctions display                             *)
val init_display : unit -> unit 
  
(* affichage d'un polygone en fonction de sa definition geometrique, de la  *)
(* luminosite, et de la camera                                              *)
val display_polygone : luminosite -> Camera.t -> 
  Geometrie.Geom3d.polygone -> unit
  
(* affichage successif d'une liste de polygone selon l'algorithme du peintre *)
val display_polygones : luminosite -> Camera.t ->
 Geometrie.Geom3d.polygone list -> unit 

