open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq = let (id, e) = eq in match e with (*donne les variables libres d'une expression*)
  | Earg (Avar x) -> [x]
  | Enot (Avar x) -> [x]
  | Ebinop (b,Avar x1,Avar x2) -> [x1;x2]
  | Ebinop (b,Avar x,Aconst _) -> [x]
  | Ebinop (b,Aconst _,Avar x) -> [x]
  | Emux (Avar x1, Avar x2, Avar x3) -> [x1;x2;x3]
  | Emux (Avar x1, Avar x2, Aconst _) -> [x1;x2]
  | Emux (Avar x1, Aconst _, Avar x2) -> [x1;x2]
  | Emux (Aconst _, Avar x1, Avar x2) -> [x1;x2]
  | Emux (Avar x, Aconst _, Aconst _) -> [x]
  | Emux (Aconst _, Avar x, Aconst _) -> [x]
  | Emux (Aconst _, Aconst _, Avar x) -> [x]
  | Erom (_, _, Avar x) -> [x]
  | Eram (_,_,Avar x, _, _, _) -> [x]
  | Econcat (Avar x1, Avar x2) -> [x1;x2]
  | Econcat (Avar x1, Aconst _) -> [x1]
  | Econcat (Aconst _, Avar x2) -> [x2]
  | Eslice (_,_,Avar x) -> [x] 
  | Eselect (_,Avar x) -> [x]
  | _ -> []

let rec recupere_equations env l = match l with (*à partir de l'identifiant d'une variable, retrouve l'équation associée dans un dictionnaire*)
  |[] -> []
  |t::q -> (try let eq = Hashtbl.find env t in [eq]
    with Not_found -> [])@(recupere_equations env q)
     
let ajoute_si_absent g id = if not (existe g id) then Graph.add_node g id (*ajoute un noeud au graphe si celui-ci est absent*)
     
let schedule p = (*effectue le tri topologique des équations du programme p*)
  let g = Graph.mk_graph () in
  let env = Hashtbl.create (List.length p.p_eqs) in
  let rec aux l = match l with
    |[] -> (try Graph.topological g
      with Cycle -> raise Combinational_cycle)
    |eq::q -> let l = (read_exp eq) in let (id,e) = eq in Hashtbl.add env id eq ;
         ajoute_si_absent g id; List.iter (ajoute_si_absent g) l; List.iter (Graph.add_edge g id) l; aux q
  in
  let l = (aux p.p_eqs) in
  let new_l = (recupere_equations env l) in
  let new_p = { p_eqs = new_l; p_inputs = p.p_inputs ; p_outputs = p.p_outputs; p_vars = p.p_vars;} in new_p

                                                                                    
let ram_rom p = (*cherche dans les équations du programme si celui-ci nécessite une ROM et/ou une RAM*)
  let rec aux l ram rom = match l with
    | [] -> ram,rom
    | (_, Erom _)::q -> aux q ram true
    | (_, Eram _)::q -> aux q true rom
    | _::q -> aux q ram rom
  in
  aux p.p_eqs false false 
