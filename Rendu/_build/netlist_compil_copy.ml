open Scheduler
open Netlist
open Netlist_printer
open Interpreteur
open Netlist_ast
open Graph

let print_only = ref false
let number_steps = ref 1
exception Empty_input  
                       
let bool_of_char c = match c with (*transforme un char '0' ou '1' en booléen*)
  | '0' -> false
  | '1' -> true
  | _ -> failwith "bool_of_char requires '0' or '1'"

let rec read_int_try_again () = (*read_int qui recommence tant que l'entrée n'est pas un int*)
  try read_int ()
  with Failure "int_of_string" -> print_string "Mauvaise entrée : entier demandé\n"; read_int_try_again ()

let bonne_entree c word_size = (*verifie que la chaine de caractère donnée en entrée a la bonne taille et est composée uniquement de 0 et de 1*) 
  let ind = ref true in
  let i = ref 0 in
  let n = String.length c in
  while (!i < n) && (!ind) do
    let x = c.[!i] in
    (if (x <> '1') && (x <> '0')
    then ind := false
     else ());
    incr i  
  done ; (!ind) && (String.length c = word_size)
                                                     
let rec read_line_try_again word_size = (*read_line qui recommence tant que l'entrée n'est pas correcte*)
  let c = read_line () in
  if bonne_entree c word_size
  then c
  else (print_string "Mauvaise entrée : nombre binaire de taille word_size attendu\n"; read_line_try_again word_size)

let char_to_bool = function
  |'0' -> false
  |'1' -> true
  | _ -> failwith "Boolean interpretation of the char is ambiguous."
    
let rec trad_value s = match String.length s with
  (*Convertie une chaine de caractere en une valeur de type value.*)
  |0 -> raise Empty_input
  |1 -> (VBit( char_to_bool s.[0] ) )
  |n -> (print_string s ; print_newline () ;
	 (VBitArray(
             Array.init
               n
               (function i -> char_to_bool s.[i])
           )
         ))

let default_value = function
  (*Donne une valeur par defaut, donc avec uniquement des false, pour intialiser une variable.*)
  |0 -> failwith "Essaie de creer une value vide"
  |1 -> VBit(false)
  |n -> VBitArray( Array.make n false )
                                                     
let ram_input () = (*Si le programme nécessite une ROM/RAM, alors ram_rom_input demande la saisie de celle-ci*)
     print_string "addr_size : " ;
     let addr_size = read_int_try_again () in
  try
    begin
      let channel = open_in ("ram") in
      let (t : (int, value) Hashtbl.t) = Hashtbl.create 17 in
      try
	(for i = 0 to addr_size -1
	do
          Hashtbl.add t i (trad_value( input_line( channel) ))
	done;
	t)
      with End_of_file -> (
	close_in channel ;
	t)
    end
  with
    Sys_error _ -> (print_string ("la ram n'a pas été définie"); print_newline ();
		    let t = Hashtbl.create 17 in t)

let rom_input () = (*Si le programme nécessite une ROM/RAM, alors ram_rom_input demande la saisie de celle-ci*)
     print_string "addr_size : " ;
     let addr_size = read_int_try_again () in
  try
    begin
      let channel = open_in ("rom") in
      let (t : (int, value) Hashtbl.t) = Hashtbl.create 17 in
      try
	(for i = 0 to addr_size -1
	do
          Hashtbl.add t i (trad_value( input_line( channel) ))
	done;
	t)
      with End_of_file -> (
	close_in channel ;
	t)
    end
  with
    Sys_error _ -> (print_string ("la rom n'a pas été définie"); print_newline ();
      let t = Hashtbl.create 17 in t)

                   
let simulate n p = (*simule n cycles du programme demandés par l'utilisateur*)
  let ctx = Env.empty in
  let tram = ram_input () in
  let trom = rom_input () in
  let p = Scheduler.schedule p in
  let rec aux i ctx tram = match i with
    | 0 -> ()
    | i -> let new_ctx,new_tram = Interpreteur.expr_program ctx p trom tram in aux (i-1) new_ctx new_tram
  in aux n ctx tram

let compile filename = (*transforme un fichier Netlist en programme (de type program) qui peut être interprété en OCaml par mon interpréteur*)
    try
      let p = Netlist.read_file filename in
      begin try
          let p = Scheduler.schedule p in
          Netlist_printer.print_program stdout p;
        with
          | Scheduler.Combinational_cycle ->
              Format.eprintf "The netlist has a combinatory cycle.@.";
              exit 2
      end;
      if not !print_only then (simulate !number_steps p) else ()
    with
      | Netlist.Parse_error s -> Format.eprintf "An error occurred: %s@." s; exit 2

let main () = (*permet de définir les arguments de la ligne de commande et de lancer l'interprétation*)
    Arg.parse
      ["-print", Arg.Set print_only, "Only print the result of scheduling";
       "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
      compile
      ""
  ;;

  main ()
