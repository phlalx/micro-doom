type side = Gauche | Droite | Intersecte
    
module type Geom = sig
  
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
     inférieur à precision de cette droite (ou plan)                  *)
  val precision : float ref
  val somme : point -> point -> point
  val difference : point -> point -> point
  val homothetie : float -> point -> point
  val prod_scal : point -> point -> float
  val normal : point array -> point
  val barycentre : point array -> point
  val intersection : polygone -> point * point -> point
  val intersection_segment : polygone -> point * point -> bool
  val cote_point : polygone -> point -> side
  val cote_poly : polygone -> polygone -> side
  val separe : polygone -> polygone -> polygone list * polygone list
  val rotation_v : float -> point -> point
  val point_to_string : point -> string 
end

module Geom2d : Geom with type point = float * float = struct

  type point = float * float

  type polygone = {
    forme: point array;
    normal: point;
    centre: point;
    couleur: Graphics.color;}
      
  (* precision des calculs *)
  let precision = ref 0.005

  let somme (x1,y1) (x2,y2) = ( x1 +. x2, y1 +. y2)

  let difference (x1,y1) (x2,y2)= (x1 -. x2, y1 -. y2)

  let homothetie k (x, y) = (k *. x, k *. y)

  (* produit scalaire *)
  let prod_scal (x1,y1) (x2,y2) = x1 *. x2 +. y1 *. y2
  
  (* renvoie le vecteur normal a un segment
     on suppose que la taille de a est egale a 2 *)
  let normal (a: point array) =
    if Array.length a <> 2 then
      failwith "argument non conforme" 
    else 
      let (x, y) = difference a.(1) a. (0) in -.y, x

  (* renvoie le milieu d'un segment *)
  let barycentre (a: point array) =
     if Array.length a <> 2 then
       failwith "argument non conforme dans barycentre" 
     else 
       let (x1, y1) = a.(0) 
       and (x2, y2) = a.(1) in
	 (x1 +. x2) /. 2., (y1 +. y2) /. 2.
      
  let polygone_of_array (a: point array) (c: Graphics.color)=
     if Array.length a <> 2 then 
       failwith "argument non conforme dans polygone_of_array" 
     else 
       { forme = a;
	 normal = normal a;
	 centre = barycentre a;
	 couleur = c; }
	
  (* intersection entre la droite (a,b) et la droite portant p
     on suppose que prod_scal (diff b a) p.normal <>0*)
  let intersection p (a, b) =
    if Array.length p.forme <> 2 then 
      failwith "argument non conforme dans intersection" 
    else let rapport = 
      prod_scal (difference p.centre a) p.normal 
      /. prod_scal (difference b a) p.normal in
      somme a (homothetie rapport (difference b a))
	

  (* renvoie Gauche si le point x est à gauche du polygone p,droite s'il est
     a droite, Interse cte sinon. L'orientation Droite/Gauche est prise, par
     rapport a l'orientation du polygone p *)
  let cote_point p x =
    let proj = prod_scal x p.normal -. prod_scal p.centre p.normal in
    if proj > !precision then Droite
    else if proj< -. !precision then Gauche
    else Intersecte

  (* renvoie Gauche si le polygone p est à gauche du polygone separateur,
     droite s'il est a droite, Intersecte sinon. L'orientation Droite/Gauche
     est prise, par rapport a l'orientation du polygone separateur *)
  let cote_poly separateur p =
    let l_side = List.rev_map (cote_point separateur) (Array.to_list p.forme)
    in
    let exists_G = List.exists (fun c->c = Gauche) l_side
    and exists_D = List.exists (fun c->c = Droite) l_side in
    match exists_G, exists_D with
      | true, true -> Intersecte
      | true, _ -> Gauche
      | _ -> Droite

  let intersection_segment p1 (x2, y2) =
    let p2 = polygone_of_array [| x2; y2 |] p1.couleur 
    and x1 = p1.forme.(0)
    and y1 = p1.forme.(1)
    in
      match (cote_point p1 x2, cote_point p1 y2,
	     cote_point p2 x1, cote_point p2 y1) with 
	| (Gauche, Droite, Gauche, Droite) 
	| (Droite, Gauche, Droite, Gauche) 
	| (Gauche, Droite, Droite, Gauche) 
	| (Droite, Gauche, Gauche, Droite) -> true 
        | _ -> false
	    
  (* coupe le polygone p en plusieurs polygones (lp_g,lp_d) tels que les
     polygones de lp_g sont à gauche de separateur et ceux de lp_d à droite.
     on suppose ici que p est convexe. *)      
  let separe (separateur: polygone) (p: polygone) = 
    match cote_poly separateur p with
      | Gauche -> [p], []
      | Droite -> [], [p]
      | Intersecte ->
	  match p.forme with 
	   |[| p1; p2 |] -> 
	      let pg,pd =  
		if cote_point separateur p1 = Gauche then p1,p2 else p2,p1 in
	      let x = intersection separateur (p1, p2) in
		[polygone_of_array [| pg; x |] p.couleur],
		[polygone_of_array [| x; pd |] p.couleur]
	   |_ -> failwith "argument non valide dans separe"

  let rotation_v alpha (x, y) = 
    x*.cos(alpha) -. y *. sin(alpha), y *. cos(alpha) +. x *. sin(alpha)

  let point_to_string (x, y) = 
    "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")" 
end

module Geom3d:Geom with type point = float * float * float  = struct
  
  type point = float*float*float

  type polygone = {
    forme: point array;
    normal: point;
    centre: point;
    couleur: Graphics.color;}
      
  (* precision des calculs *)
  let precision = ref 0.005

  let somme (x1,y1,z1) (x2,y2,z2) = (x1+.x2,y1+.y2,z1+.z2)
  let difference (x1,y1,z1) (x2,y2,z2) = (x1-.x2,y1-.y2,z1-.z2)
  let homothetie k (x,y,z) = (k*.x,k*.y,k*.z)

  let prod_scal (x1,y1,z1) (x2,y2,z2) =  x1*.x2+.y1*.y2+.z1*.z2
  
  (* produit vectoriel, utile pour calculer la normale à un polygone 3D *)
  let prod_vect (x1,y1,z1) (x2,y2,z2) = 
    (y1*.z2-.z1*.y2,z1*.x2-.x1*.z2,x1*.y2-.y1*.x2)

  (* renvoie le vecteur normal a un ensemble de points coplanaires
     on suppose que la taille de a est superieure ou egale a 3 *)
  let normal (a: point array) = 
    let res = ref (0.,0.,0.)
    and n = Array.length a
    and i = ref 2
    in
      while !i<n & !res = (0.,0.,0.) do
	res := prod_vect (difference a.(1) a.(0)) (difference a.(!i) a.(0));
	i := !i+1
      done;
      if !res=(0.,0.,0.) then
	failwith "polygone degenere"
      else 
	homothetie (1./.(sqrt (prod_scal !res !res))) !res
	  
  (* renvoie l'isobarycentre d'un ensemble de points *)
  let barycentre (a: point array)=
    let n = Array.length a
    and g = ref (0.,0.,0.) in
    for i = 0 to n-1 do
      g := somme !g a.(i)
    done;
      homothetie (1. /. float_of_int n) !g
      
  let polygone_of_array (a: point array) (c: Graphics.color)=
    let n = Array.length a in
      if n < 3 then failwith "polygone_of_array: pas assez de points"
      else 
	{ forme = a;
	  normal = normal a;
	  centre = barycentre a;
	  couleur = c; }
	
  (* intersection entre la droite (a,b) et le plan du polygone p
     on suppose que prod_scal (diff b a) p.normal <>0 *)
  let intersection p (a, b) = 
    let rapport = prod_scal (difference p.centre a) p.normal
		  /. prod_scal (difference b a) p.normal 
    in somme a (homothetie rapport (difference b a))
	 
  (* renvoie Gauche si le point x est à gauche du polygone p,droite s'il est
     a droite, Intersecte sinon. L'orientation Droite/Gauche est prise, 
     par rapport a l'orientation du polygone p *)
  let cote_point p x =
    let proj = prod_scal x p.normal-.prod_scal p.centre p.normal in
      if proj> !precision then Droite
      else if proj< -. !precision then Gauche else Intersecte
      
  (* renvoie Gauche si le polygone p est à gauche du polygone separateur,
     droite s'il est a droite, Intersecte sinon. L'orientation Droite/Gauche 
     est prise, par rapport a l'orientation du polygone separateur *)
  let cote_poly separateur p =
    let l_side = List.rev_map (cote_point separateur) (Array.to_list p.forme) in
    let exists_G = List.exists (fun c -> c = Gauche) l_side
    and exists_D = List.exists (fun c -> c = Droite) l_side in
      match exists_G,exists_D with
	| true, true -> Intersecte
	| true, _ -> Gauche
	| _ -> Droite
	   
  let intersection_segment p (a, b) = false
	  
  (* coupe le polygone p en plusieurs polygones (lp_g,lp_d) tels que les
     polygones de lp_g sont à gauche de separateur et ceux de lp_d à droite.
     on suppose ici que p est convexe. *)
  let separe (separateur: polygone) (p: polygone) = 
    let lp_g = ref []
    and lp_d = ref [] in
    (* find where to start scanning the vertices (just after changing side) *)
    let rec starting_point side acc = function
      | [] -> separe_aux side acc []
      | x :: q ->
	  let side_x = cote_point separateur x in
	    match side with
	      | Intersecte->starting_point side_x (x::acc) q
	      | _ ->
		  if (side = Gauche && side_x = Droite)
		    or (side = Droite && side_x = Gauche) then (
		      let y = List.hd acc in
		      let z = intersection separateur (x,y) in
			separe_aux side_x [x;z] (q@(List.rev (z::acc)))
		    )
		  else starting_point side (x::acc) q
		    
    (* separe le polygone en parcourant la liste des sommets. On mets à
       jours lp_g ou lp_d quand on change de cote. *)
    and separe_aux side acc = function
      | [] -> 
	  let p = { forme = Array.of_list acc;
		    normal = p.normal;
		    centre = barycentre (Array.of_list acc);
		    couleur = p.couleur;} in
	    if side = Gauche then lp_g := p :: !lp_g else lp_d := p :: !lp_d
      | x :: q->
	  let side_x = cote_point separateur x in
	    match side, side_x with
	      | Intersecte, _ -> separe_aux side_x (x::acc) q
	      | _ ->
		  if (side = Gauche && side_x = Droite)
		    or (side = Droite && side_x = Gauche) then (
		      let y = List.hd acc in
		      let z = intersection separateur (x,y) in
			separe_aux side (z::acc) [];
			separe_aux side_x [x;z] q
		    )
		  else separe_aux side (x::acc) q
    in
      starting_point Intersecte [] (Array.to_list (p.forme));
      (!lp_g,!lp_d)

  let rotation_v alpha (x,y,z) = 
    x*.cos(alpha) -. y *. sin(alpha), y *. cos(alpha) +. x *. sin(alpha), z

 let point_to_string (x,y,z) =
   "(" ^ string_of_float x ^ "," ^ string_of_float y ^ "," ^ 
   string_of_float z ^ ")" 
end


module Geom2d3d =
  struct
    let proj (x,y,_) = x,y

    let threeDfy z_bot z_top p = 
      match p with
	| { Geom2d.forme = [| xp1, yp1; xp2, yp2 |]; couleur = c; } -> 
	    let p3d = [| xp1, yp1, z_bot; xp1,yp1,z_top; 
			 xp2, yp2, z_top; xp2,yp2,z_bot |] in
	      { Geom3d.forme = p3d;
		normal =Geom3d.normal p3d;
		centre = Geom3d.barycentre p3d;
		couleur = c;}
	| _ -> assert false 
end
