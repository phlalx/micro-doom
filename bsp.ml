
module type BSP =
sig
  type polygone 
    
  type point
    
  type monde = polygone list
      
  type t =
    | Feuille of monde
    | Noeud of t * polygone * t 
	
  val separe_list : polygone -> monde -> monde * monde
    
  val build_bsp : monde -> t 
    
  val collision : t -> point -> point -> bool 
    
  val painter : t -> point -> monde
end


module Make (G:Geometrie.Geom) : 
  BSP with type polygone = G.polygone and type point = G.point =
struct
  type polygone = G.polygone

  type point = G.point

  type monde = polygone list
      
  type t = 
    | Feuille of monde
    | Noeud of t * polygone * t
	
  let rec separe_list p m = 
    match m with
      | [] -> [], []
      | e :: m' -> 
	  let e1, e2 = G.separe p e 
	  and m1, m2 = separe_list p m' 
	  in e1 @ m1, e2 @ m2
	      
  let rec build_bsp m = 
    match m with
      | [] -> Feuille m
      | [ e ] -> Feuille m
      | p :: m' -> 
	  let mg, md = separe_list p m' 
	  in Noeud ((build_bsp mg), p, (build_bsp md))
	       
  let rec painter bsp c = 
    match bsp with
      | Feuille m -> m
      | Noeud (bg,p,bd) -> 
	  if G.cote_point p c = Geometrie.Gauche
	  then (painter bd c) @ (p :: painter bg c)
	  else (painter bg c) @ (p :: painter bd c)

  let rec collision bsp c1 c2 = 
    match bsp with
      | Feuille [p] -> G.intersection_segment p (c1,c2)
      | Noeud (bg,p,bd) -> 
	  (G.intersection_segment p (c1,c2))
	    or 
	  (collision bg c1 c2)
	    or 
	  (collision bd c1 c2)
	    
      | _ -> false
end

module Bsp2d = Make(Geometrie.Geom2d)
module Bsp3d = Make(Geometrie.Geom3d)
