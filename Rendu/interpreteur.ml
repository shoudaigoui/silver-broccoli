open Netlist_ast
open Scheduler

let binaire_decimal t = (*transforme un nombre binaire en nombre décimal*)
  let n = Array.length t in
  let res = ref 0 in
  let acc = ref 1 in
  for i=0 to (n-1) do
    if t.(n-1-i) = true
    then (res := !res + !acc ; acc := !acc*2)
    else (acc := !acc*2)
  done; !res

let transform_string_value c = (*transforme une chaine de caractère en un VBit ou un VBitArray, si cela est possible*)
  let n = String.length c in
  let bool_of_int c = match c with
    | '0' -> false
    | '1' -> true
    | _ -> failwith "Entrée incorrecte"
  in
  if n = 1 then (VBit (bool_of_int c.[0]))
    else (let t = Array.make n false in
  for i=0 to (n-1) do
    t.(i) <- bool_of_int c.[i]
  done; VBitArray t)    
                           
let value_to_bool a = match a with (*récupère à partir d'un VBit, le booléen qu'il représente*)
  | VBit x -> x
  | VBitArray _ -> failwith "Mux/RAM(write_enable) : value_to_bool : A VBit and not a VBitArray was expected"

let bool_to_int = function
  |true -> 1
  |false -> 0
             
     
let value_to_addr = function
  (*Convertie un type value en le type int, type reelement utilise pour acceder a la rom/ram.*)
  (*Dans le cas d'un VBitArray, les boolens sont interpretes comme des 0/1 
    et les bits de poids forts sont ceux au début du tableau, i.e. a gauche.*)
  |VBit(false) -> 0
  |VBit(true) -> 1
  |VBitArray(t) -> Array.fold_left
                     ( fun y x -> 2*y + (bool_to_int x) )
                     0 t
     
let value_to_bool_array a = match a with (*récupère à partir d'un VBitArray, le tableau de booléens qu'il représente*)
  | VBit x -> failwith "ROM/RAM : value_to_bool_array : a VBitArray was expected (read_addr, write_addr,data)"
  | VBitArray x -> x
                            
                            
let print_value = function (*affiche un VBit ou un VBitArray sous forme d'un nombre binaire*)
  | VBit b -> (if b=true then Pervasives.print_int 1 else Pervasives.print_int 0); Pervasives.print_string "\n"
  | VBitArray b -> (Array.iter (fun x -> let y = (if x=true then 1 else 0)
                                       in Pervasives.print_int y) b); Pervasives.print_string "\n"

let map2 f t1 t2 = (*extension de la fonction map pour qu'elle puisse prendre en argument 2 tableaux*)
  let n = Array.length t1 in
                   if n <> Array.length t2 then failwith "Tableaux de longueur differentes"
                   else let t = Array.make n false in                                        
  for i=0 to (n-1) do
    t.(i) <- (f t1.(i) t2.(i))
  done;t


let eval_not x = match x with (*evaluation d'un not*)
  | VBit a -> VBit (not a)
  | VBitArray a -> VBitArray (Array.map (fun x -> not x) a)
  
let eval_binop_VBit b x1 x2 = match b with (*evaluation d'une opération binaire pour des VBit*)
    |And -> x1 && x2
    |Or -> x1 || x2
    |Nand -> not (x1 && x2)
    |Xor -> ((not x1)&&x2) || (x1&&(not x2))
  

let eval_binop b x y = match b,x,y with (*évaluation générale d'une opération binaire*)
  |b, VBit x1, VBit x2 -> VBit (eval_binop_VBit b x1 x2)
  |b, VBitArray x1, VBitArray x2 -> VBitArray (map2 (fun a1 a2 -> eval_binop_VBit b a1 a2) x1 x2)
  |_ -> failwith "Vbit b Vbitarray : impossible"

let eval_concat x1 x2 = match x1,x2 with (*évaluation de la concaténation*)
  | VBitArray t1, VBitArray t2 -> VBitArray (Array.concat [t1;t2])
  | VBit t1 , VBitArray t2 -> VBitArray (Array.concat [[|t1|];t2])
  | VBitArray t1, VBit t2 -> VBitArray (Array.concat [t1;[|t2|]])
  | VBit t1, VBit t2 -> VBitArray [|t1;t2|]

let eval_slice i j x = match x with (*évaluation d'un slice*)
  | VBitArray t -> VBitArray (Array.sub t i (j-i+1))
  | _ -> failwith "Slice requires a VBitArray"

let eval_select i x = match x with (*evaluation d'un select*)
  | VBitArray t -> VBit t.(i)
  | VBit b when i=0 -> VBit b
  | _ -> failwith "Select requires a VBitArray"


let default_value = function
  (*Donne une valeur par defaut, donc avec uniquement des false, pour intialiser une variable.*)
  |0 -> failwith "Essaie de creer une value vide"
  |1 -> VBit(false)
  |n -> VBitArray( Array.make n false )
                  
let eval_rm addr_size word_size read_addr trm = (*evaluation de la ROM et de la RAM*)
  try
    Hashtbl.find trm read_addr
  with
    Not_found -> default_value word_size
(*trm.((binaire_decimal read_addr))*)
           

let expr_arg ctx = function (*gère le passage des expressions de type arg à des expressions de type value*)
  | Aconst x -> x
  | Avar x -> (Env.find x ctx)
       
 (*La fonction expr_simple gère l'interprétation de toutes les expressions. 

Retour : (est_reg, id_reg, est_ram, (write_enable, write_addr, data), v)

est_reg indique si on vient d'évaluer un registre
id_reg donne dans le cas d'un registre l'ident qui donnera la prochaine valeur du registre
est_ram indique si on vient d'évaluer une RAM
(write_enable,write_addr,data) sont dans le cas d'une RAM les arguments de celle-ci 
v est l'élément de type value que donne l'évaluation d'une expression (arbitraire pour un registre)*)

                
let expr_simple ctx tram trom = function
  | Earg x -> false, "", false, (Avar "", Avar "", Avar ""), expr_arg ctx x
                                                                            
  | Ereg x -> true, x, false, (Avar "", Avar "", Avar ""), (VBit false)
                                                                   
  | Enot x -> false, "", false, (Avar "", Avar "", Avar ""), eval_not (expr_arg ctx x)
                                                                            
  | Ebinop (b,x,y) -> false, "", false, (Avar "", Avar "", Avar ""), (eval_binop b (expr_arg ctx x) (expr_arg ctx y))
                                                                             
  | Emux (a,b,c) -> if value_to_bool (expr_arg ctx a)
                    then (false,"",false,(Avar "",Avar "",Avar ""),expr_arg ctx b)
                    else (false,"",false,(Avar "",Avar "",Avar ""),expr_arg ctx c)
                           
  | Erom (addr_size,word_size,read_addr) ->
     false,"",false,(Avar "",Avar "",Avar ""),
     (eval_rm addr_size word_size
                         (value_to_addr (expr_arg ctx read_addr))
                         trom)

  | Eram (addr_size,word_size,read_addr,write_enable,write_addr,data) ->
     (let res = (eval_rm addr_size word_size
               (value_to_addr (expr_arg ctx read_addr))
               tram)
     in
     (false, "", true, (write_enable,write_addr,data), (res)))

  | Econcat (a,b) -> false, "", false, (Avar "",Avar "",Avar ""), (eval_concat (expr_arg ctx a) (expr_arg ctx b))
                                                                          
  | Eslice (i,j,x) -> (*Convention : On prendra l'élément à l'indice i inclus et l'élément à l'indice j exclus*)
     false, "", false, (Avar "",Avar "",Avar ""), (eval_slice i j (expr_arg ctx x))
                                                          
  | Eselect (i,x) -> false, "", false, (Avar "",Avar "",Avar ""), (eval_select i (expr_arg ctx x))
                                                                    
                                                                    
let expr_equation ctx tram trom eq vars=
  (*gère l'évaluation des équations : utilise expr_simple et remplace dans le contexte les variables qui recoivent une nouvelle valeur*)
  let (id,e) = eq in
  let est_reg,id_reg,est_ram,trio_ram,x = expr_simple ctx tram trom e in
  let meme_type x t = match x,t with
    | VBit _, TBit -> true
    | VBitArray a, TBitArray n when Array.length a = n -> true
    | VBit _ , TBitArray 1 -> true
    | _ -> false
  in
  let initialise_reg id_reg vars = match (Env.find id_reg vars) with
    | TBit -> VBit false
    | TBitArray n -> VBitArray (Array.make n false)
  in let t = Env.find id vars in
     if est_reg
     then (if (not (Env.mem id ctx))
           then (let new_ctx = Env.add id (initialise_reg id_reg vars) ctx in (new_ctx,(id,id_reg),est_ram,trio_ram))
            else (ctx,(id,id_reg),est_ram,trio_ram))
     else
       (if meme_type x t
       then (let new_ctx = Env.add id x ctx in new_ctx,("",""),est_ram,trio_ram)
       else (failwith (String.concat " " [id;": Mauvais type pour la variable"])))
                                    

let read_input p id = (*demande à l'utilisateur la saisie d'une chaine de caractère constituée de 0 et de 1 de taille correcte*)
  let entree_valable b = match b with (*vérifie si un char b vaut '0' ou '1'*)
    | '0' | '1' -> true
    | _ -> false
  in
  let entree_valable_chaine c n = (*verifie si une chaine de caractère est constituée de 0 et de 1*)
    let ind = ref true in
    let i = ref 0 in
    while (!i<n) && (!ind = true) do
      (ind := entree_valable c.[!i];
      incr i)
    done ; !ind
  in
  let ind = ref true in
  let x = ref "" in
  while !ind do
    print_string id;
    print_string " = ";
    x := read_line ();
    let n = String.length !x in
    if ((n = 1) && (Env.find id p.p_vars = TBit) && (entree_valable (!x).[0]))
        || ((Env.find id p.p_vars = TBitArray n) && (entree_valable_chaine (!x) n))
    then ind := false
    else print_string "Mauvaise entrée\n"
            done; !x
  
    

let expr_program ctx p trom tram = (*gère l'évaluation complète du programme*)
  let rec ajoute_entrees l ctx = match l with (*ajout des variables input dans le contexte*)
    | [] -> ctx
    | t::q -> let x = read_input p t in
              let y = transform_string_value x in let ctx = Env.add t y ctx in ajoute_entrees q ctx
  in
  let rec evalue_equations l ctx liste_registre liste_ram = match l with (*evaluation des equations les unes après les autres*)
    | [] -> ctx,liste_registre,liste_ram
    | t::q -> (let new_ctx,couple,est_ram,trio = expr_equation ctx tram trom t p.p_vars in
              (if (fst couple) <> ""
               then (evalue_equations q new_ctx (couple::liste_registre) liste_ram)
               else
                 if est_ram
                 then (evalue_equations q new_ctx liste_registre (trio::liste_ram))
                 else (evalue_equations q new_ctx liste_registre liste_ram)))
  in
  let rec retourne_sorties l ctx = match l with (*affichage des outputs*)
    | [] -> ()
    | t::q -> print_string t; print_string " = "; print_value (Env.find t ctx) ; retourne_sorties q ctx
  in
  let rec change_registre l ctx = match l with (*modification des registres*)
    | [] -> ctx
    | (id,id2)::q -> let new_ctx = Env.add id (Env.find id2 ctx) ctx in change_registre q new_ctx
  in
  let rec change_ram l ctx tram = match l with (*modification de la RAM*)
    | [] -> tram
    | (write_enable,write_addr,data)::q when (value_to_bool (expr_arg ctx write_enable)) ->
       Hashtbl.add tram (value_to_addr (expr_arg ctx write_addr)) (expr_arg ctx data)


       (*tram.((binaire_decimal (value_to_bool_array (expr_arg ctx write_addr))))
	 <- (value_to_bool_array (expr_arg ctx data))*) ;
       change_ram q ctx tram
    | _::q -> change_ram q ctx tram
  in let ctx = ajoute_entrees p.p_inputs ctx
  in let ctx,liste_registre,liste_ram = evalue_equations p.p_eqs ctx [] []
  in print_string "\n"; retourne_sorties p.p_outputs ctx; print_string "\n";
  let ctx = change_registre (List.rev liste_registre) ctx
  in let tram = change_ram (List.rev liste_ram) ctx tram in ctx,tram


