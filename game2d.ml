open Geometrie

type t = {
  lum : Affichage.luminosite;
  bsp : Bsp.Bsp2d.t;
  z_bot : float;
  z_top : float;
  cam : Camera.t;
  pas : float;
  angle : float;
}

let to_bsp monde = 
  let to_poly_list = 
    let random_color _ =
      let max_color = 256 * 256 * 246 - 1 in Random.int max_color 
    in List.rev_map (fun x ->Geom2d.polygone_of_array x (random_color ()))
  in Bsp.Bsp2d.build_bsp (to_poly_list monde)

let open_file f = try open_in f with _ -> failwith ("Could not find " ^ f)

type token = Lbr | Com | Rbr | Id of string | Float of float | Eq | Lpar | Rpar 

let valchiffre c = float_of_int (int_of_char c - int_of_char '0') 

let rec pentiere n = parser
 | [< '  '0'..'9' as c; s >] -> pentiere (10. *. n +. valchiffre c) s
 | [< >] -> n

let rec pvirgule_aux coef n  =
  parser
    | [< '  '0'..'9' as c; s >] -> 
	pvirgule_aux (coef *. 10.) (n +. (valchiffre c) /. coef) s
    | [< >] -> n 

let rec pvirgule = 
  parser 
    | [< ''.'; res = pvirgule_aux 10. 0. >] -> res 
    | [< >] -> 0.

let parse_ident c s =
  let string_of_char c = String.make 1 c in
  let rec ident_aux = 
    parser
      | [< '  'a'..'z' as c; s = ident_aux >] -> string_of_char c ^ s
      | [< >] -> "" in
    string_of_char c ^ ident_aux s

let rec next_token = parser
  | [< '  ' '|'\n'; tk = next_token >] -> tk
  | [< '  'a'..'z' as c; s = parse_ident c >] -> Some (Id s) 
  | [< '  '0'..'9' as c; 
       e = pentiere (valchiffre c);
       v = pvirgule >] -> 
print_float (e +. v); print_string " "; flush stdout;

Some (Float (e +. v))  
  | [< ''[' >] -> Some (Lbr)
  | [< '',' >] -> Some (Com)
  | [< '']' >] -> Some (Rbr)
  | [< ''(' >] -> Some (Lpar)
  | [< '')' >] -> Some (Rpar)
  | [< ''=' >] -> Some (Eq)
  | [< >] -> None

let lex s = Stream.from (fun _ -> next_token s)

let parse_point_3d =
  parser
    | [< 'Lpar; 'Float(x); 'Com; 'Float(y); 'Com; 'Float(z); 'Rpar >] -> x,y,z

let parse_point_2d =
  parser
    | [< 'Lpar; 'Float(x); 'Com; 'Float(y); 'Rpar >] -> x,y

let parse_property_float s1 =
  parser
    | [< 'Id(s); 'Eq; 'Float(x) >] -> if s = s1 then x else failwith ("parse error"
	^ s ^ " " ^ s1)

let parse_property_point s1 =
  parser
    | [< 'Id(s); 'Eq; p = parse_point_3d >] -> 
	if s = s1 then p else failwith "parse error"

let parse_segment =
  parser 
    | [< 'Lbr; p1 = parse_point_2d; p2 = parse_point_2d ;'Rbr >] -> [| p1;p2 |]

let rec parse_monde =
  parser 
    | [< s = parse_segment; m = parse_monde >] -> s :: m  
    | [<>] -> []

let parse_property_monde s1 =
  parser
    | [< 'Id(s); 'Eq; m = parse_monde >] -> 
	if s = s1 then m else failwith "parse error" 

let game s = 
  let stream = Stream.of_channel (open_file s) in
  let lex_stream = lex stream in
  let observateur = parse_property_point "observateur" lex_stream 
  and zbot = parse_property_float "zbot" lex_stream  
  and ztop = parse_property_float "ztop" lex_stream 
  and distance_fenetre = parse_property_float "distancefenetre" lex_stream 
  and window_x = parse_property_float "windowx" lex_stream
  and window_z = parse_property_float "windowz" lex_stream
  and pas = parse_property_float "pas" lex_stream
  and angle = parse_property_float "angle" lex_stream
  and lum_ambiante = parse_property_float "lumambiante" lex_stream
  and lum_source = parse_property_float "lumsource" lex_stream 
  and monde = parse_property_monde "monde" lex_stream  in
    { lum = {Affichage.ambiante = lum_ambiante; Affichage.source = lum_source};
      bsp = to_bsp monde;
      z_bot = zbot;
      z_top = ztop;
      cam = Camera.make observateur distance_fenetre window_x window_z; 
      pas = pas;
      angle = angle }
