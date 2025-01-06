open Read_input
open Base

let make_graph lst =
  let graph = Hashtbl.create (module String) in

  let rec populate_graph conn_lst =
    match conn_lst with
    | [] -> graph
    | hd :: tl ->
        let cur_nodes = String.split ~on:'-' hd in
        let n1, n2 = (List.hd_exn cur_nodes, List.nth_exn cur_nodes 1) in
        let n1_cur =
          Hashtbl.find graph n1
          |> Option.value ~default:(Set.empty (module String))
        in
        let n2_cur =
          Hashtbl.find graph n2
          |> Option.value ~default:(Set.empty (module String))
        in
        Hashtbl.set graph ~key:n1 ~data:(Set.add n1_cur n2);
        Hashtbl.set graph ~key:n2 ~data:(Set.add n2_cur n1);
        populate_graph tl
  in
  populate_graph lst

let starts_with_t str = Char.equal 't' (String.get str 0)

let starts_with_t_any str_lst =
  List.length (List.filter str_lst ~f:(fun s -> starts_with_t s)) > 0

let update_network_set set lst =
  let sorted_lst = List.sort lst ~compare:(fun x y -> String.compare x y) in
  Set.add !set (String.concat ~sep:"," sorted_lst)

(* bron-kerbosch with pivoting, for finding maximal clique *)
(* https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm *)
(* https://www.mancoosi.org/~abate/finding-maximal-cliques-and-independent-sets-undirected-graph-bron%E2%80%93kerbosch-algorithm.html *)
(* todo: it would be nice to use z3 and an integer program to solve for the maximal clique*)

let rec bron_kerbosch (r : (string, Base.String.comparator_witness) Base.Set.t)
    (p : (string, Base.String.comparator_witness) Base.Set.t)
    (x : (string, Base.String.comparator_witness) Base.Set.t)
    (graph :
      ( string,
        (string, Base.String.comparator_witness) Base.Set.t )
      Base.Hashtbl.t) =
  if Set.is_empty p && Set.is_empty x then [ Set.to_list r ]
  else
    let _, _, result =
      Set.fold p ~init:(p, x, []) ~f:(fun (p, x, acc) vertex ->
          let r' = Set.add r vertex in
          let p' = Set.inter p (Hashtbl.find_exn graph vertex) in
          let x' = Set.inter x (Hashtbl.find_exn graph vertex) in
          ( Set.remove p vertex,
            Set.add x vertex,
            bron_kerbosch r' p' x' graph @ acc ))
    in

    result

let solve_part_1 input_lst =
  let network_trips = ref (Set.empty (module String)) in
  let graph = make_graph input_lst in
  let initial_nodes = Hashtbl.keys graph in

  let rec find_networks_of_three graph set nodes =
    match nodes with
    | [] -> !set
    | n1 :: tl ->
        let nodes2 = Hashtbl.find_exn graph n1 in
        Set.iter nodes2 ~f:(fun n2 ->
            let nodes3 = Hashtbl.find_exn graph n2 in
            Set.iter nodes3 ~f:(fun n3 ->
                let network = Hashtbl.find_exn graph n3 in
                if Set.mem network n1 && starts_with_t_any [ n1; n2; n3 ] then
                  set := update_network_set set [ n1; n2; n3 ]));
        find_networks_of_three graph set tl
  in

  find_networks_of_three graph network_trips initial_nodes
  |> Set.length |> Int.to_string

let solve_part_2 input_lst =
  let graph = make_graph input_lst in

  let p = Set.of_list (module String) (Hashtbl.keys graph) in
  let x = Set.empty (module String) in
  let r = Set.empty (module String) in

  let cliques = bron_kerbosch r p x graph in

  match
    List.max_elt cliques ~compare:(fun x1 x2 ->
        compare (List.length x1) (List.length x2))
  with
  | None -> failwith "no cliques for part 2"
  | Some clique ->
      String.concat ~sep:","
        (List.sort clique ~compare:(fun a b -> String.compare a b))

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_2
