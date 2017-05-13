(* t contient toutes les informations relative au jeu a un moment donne *)

type t = {
  lum : Affichage.luminosite;  
  bsp : Bsp.Bsp2d.t;  (* bsp precalcule correspondant au monde *)
  z_bot : float;      (* z sol  *)
  z_top : float;      (* z plafond *)
  cam : Camera.t;     (* camera initiale *)
  pas : float;        (* pas de deplacement elementaire *)
  angle: float;       (* angle de rotation elementaire *)
}

(* parse le fichier de nom string et construit le jeu de type t correspondant *)
val game : string -> t 





    
