
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

module Bsp3d : BSP with type polygone = Geometrie.Geom3d.polygone
		   and type point = Geometrie.Geom3d.point
  
module Bsp2d : BSP with type polygone = Geometrie.Geom2d.polygone
		   and type point = Geometrie.Geom2d.point
  
