open Geometrie

type t = {
  lum : Affichage.luminosite;
  bsp : Bsp.Bsp3d.t;
  cam : Camera.t;
  pas : float;
  angle : float;
}

let monde = 
  [ [|(150.,150.,0.);(150.,250.,0.);(150.,250.,100.);(150.,150.,100.)|] ;
    [|(150.,250.,0.);(50.,250.,0.);(50.,250.,100.);(150.,250.,100.)|] ;
    [|(50.,250.,0.);(50.,300.,0.);(50.,300.,100.);(50.,250.,100.)|] ;
    [|(50.,300.,0.);(250.,300.,0.);(250.,300.,100.);(50.,300.,100.)|] ;
    [|(250.,300.,0.);(250.,250.,0.);(250.,250.,100.);(250.,300.,100.)|] ;
    [|(250.,250.,0.);(200.,250.,0.);(200.,250.,100.);(250.,250.,100.)|] ;
    [|(200.,250.,0.);(200.,150.,0.);(200.,150.,100.);(200.,250.,100.)|] ;
    [|(200.,150.,0.);(150.,150.,0.);(150.,150.,100.);(200.,150.,100.)|]  ]

let observateur = 175., 175.,50.
let distance_fenetre = 15.   
let window_x = 20.
let window_z = 30.
let pas = 10.0
let angle = 3.1415 /. 30.
let lum_ambiante = 0.5
let lum_source = 16.

let camera = Camera.make observateur distance_fenetre window_x window_z 

let lumiere = { Affichage.ambiante = lum_ambiante; 
		Affichage.source = lum_source }

let random_color _ =
  let max_color = 256*256*256-1 in Random.int max_color

let labyrinthe =
  List.rev_map 
    (fun x -> Geom3d.polygone_of_array x (random_color ())) 
    monde 

let bsp3d = Bsp.Bsp3d.build_bsp labyrinthe 

let game = 
  { lum = lumiere;
    bsp = bsp3d; 
    cam = camera; 
    pas = pas;
    angle = angle }


