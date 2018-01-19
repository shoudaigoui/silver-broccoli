let bonne_entree c =
  let ind = ref true in
  let i = ref 0 in
  let n = String.length c in
  while (!i < n) && (!ind) do
    let x = c.[!i] in
    (if (x <> '1') && (x <> '0')
    then ind := false
     else ());
    incr i
  done ; !ind
                                                     
let rec read_line_try_again () =
  let c = read_line () in 
  if bonne_entree c
  then c
  else (print_string "Mauvaise entrée : nombre binaire de taille word_size attendu\n"; read_line_try_again ())

let c = read_line_try_again ()
                                                      
