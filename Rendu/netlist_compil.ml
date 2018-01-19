open Scheduler
open Netlist
open Netlist_printer
open Interpreteur
open Netlist_ast
open Graph

let print_only = ref false
let number_steps = ref 1
                       
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
                                                     
let ram_rom_input rm = (*Si le programme nécessite une ROM/RAM, alors ram_rom_input demande la saisie de celle-ci*)
  if rm
  then
    (print_string "addr_size : " ;
     let addr_size = read_int_try_again () in
     print_string "word_size : ";
     let word_size = read_int_try_again () in
     let taille = int_of_float (2.**(float_of_int addr_size)) in
     let trm = Array.make_matrix taille word_size (false) in
     print_string "Commencez la saisie :\n" ;
     for i = 0 to taille-1 do
       let c = read_line_try_again word_size in
       (for j = 0 to word_size-1 do
         trm.(i).(j) <- bool_of_char c.[j]
        done)
     done ; trm)
   else Array.make 0 [||]

                   
let simulate n p ram rom = (*simule n cycles du programme demandés par l'utilisateur*)
  let ctx = Env.empty in
  if ram then print_string "veuillez saisir la RAM :\n" else () ;
  let tram = ram_rom_input ram in
  if rom then print_string "veuillez saisir la ROM :\n" else () ;
  let trom = ram_rom_input rom in
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
      let ram,rom = Scheduler.ram_rom p in
      if not !print_only then (simulate !number_steps p ram rom) else ()
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
