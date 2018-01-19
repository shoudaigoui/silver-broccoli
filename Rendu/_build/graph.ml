exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

                    
let node_for_label g x = (*recherche le noeud de label x*)
  List.find (fun n -> n.n_label = x) g.g_nodes

let existe g x = (*vérifie si un noeud de label x est présent dans le graphe*)
  let rec aux l x = match l with
  |[] -> false
  |n::q when n.n_label = x -> true
  |_::q -> aux q x
in aux g.g_nodes x
    
let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g = (*remet les marques de tous les noeuds à NotVisited*)
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g = (*permet de trouver les éléments qui ont un degré entrant de 0*)
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g = (*determine si le graphe a un cycle*)
  let rec cycle_un_noeud listNode = match listNode with (*recherche si à partir d'un noeud, on peut trouver un cycle en remontant la liste des noeuds qui lui sont reliés*)
    |[] -> false
    |n::q -> (if n.n_mark = InProgress
              then true
              else (n.n_mark <- InProgress; let x = cycle_un_noeud (n.n_link_to) in n.n_mark <- Visited; x||(cycle_un_noeud q)))
     
  in let rec cycle_global l = match l with (*applique la fonction cycle_un_noeud à l'ensemble des noeuds non traités*)
       |[] -> false
       |n::q when n.n_mark = NotVisited -> n.n_mark <- InProgress;
                                           let x = cycle_un_noeud (n.n_link_to) in n.n_mark <- Visited; (if x then true else cycle_global q)
       |_::q -> cycle_global q
  
  in cycle_global g.g_nodes

   
let topological g = (*donne le tri topologique d'un graphe si celui-ci ne possède pas de cycle*)
    let rec aux l result = match l with
      |[] -> result
      |t::q -> if t.n_mark = NotVisited then (t.n_mark <- Visited;
      					(aux q (result@(aux t.n_link_to [])@[t.n_label])))
          else aux q result
    in
    if (has_cycle g) then raise Cycle
         else clear_marks g; aux g.g_nodes []
