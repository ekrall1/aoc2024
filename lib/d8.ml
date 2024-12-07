open Base
open Read_input

let get_frequencies input =
  input |> fun instr ->
  String.fold instr
    ~init:(Map.empty (module Char))
    ~f:(fun acc char ->
      if Char.equal char '.' || Char.equal char '\n' then acc
      else
        Map.update acc char ~f:(function None -> 1 | Some count -> count + 1))

let make_grid input = input |> Read_input.string_to_lines

let find_coordinates grid unique_chars =
  let coord_map = Hashtbl.create (module Char) in
  List.iteri grid ~f:(fun row line ->
      String.iteri line ~f:(fun col char ->
          if Map.mem unique_chars char then
            Hashtbl.update coord_map char ~f:(function
              | None -> [ (row, col) ]
              | Some coords -> (row, col) :: coords)));
  Hashtbl.map coord_map ~f:List.rev

let find_new_points (x1, y1) (x2, y2) =
  let forward = (x1 + (x1 - x2), y1 + (y1 - y2)) in
  let backward = (x2 - (x1 - x2), y2 - (y1 - y2)) in
  (forward, backward)

let check_coord_in_grid (x, y) grid =
  let rows = List.length grid - 1 in
  let cols = String.length (List.hd_exn grid) - 1 in
  x >= 0 && x <= rows && y >= 0 && y <= cols

let part1_pairs coord_map =
  Hashtbl.fold coord_map ~init:[] ~f:(fun ~key:_ ~data:coords acc ->
      let rec compute_pairs acc = function
        | [] -> acc
        | c1 :: rest ->
            let new_pairs =
              List.map rest ~f:(fun c2 ->
                  let forward, backward = find_new_points c1 c2 in
                  (forward, backward))
            in
            compute_pairs (acc @ new_pairs) rest
      in
      acc @ compute_pairs [] coords)

let solve_part_1 input =
  let unique_freqs = input |> get_frequencies in
  let grid = input |> make_grid in
  let coord_map = find_coordinates grid unique_freqs in
  let pairwise = part1_pairs coord_map in

  let rows = List.length grid - 1 in
  let cols = String.length (List.hd_exn grid) - 1 in

  let set = ref (Set.empty (module String)) in

  List.iter pairwise ~f:(fun (anode1, anode2) ->
      if
        fst anode1 >= 0
        && fst anode1 <= rows
        && snd anode1 >= 0
        && snd anode1 <= cols
      then
        set := Set.add !set (Printf.sprintf "%d,%d" (fst anode1) (snd anode1));
      if
        fst anode2 >= 0
        && fst anode2 <= rows
        && snd anode2 >= 0
        && snd anode2 <= cols
      then
        set := Set.add !set (Printf.sprintf "%d,%d" (fst anode2) (snd anode2)));

  Set.length !set |> Int.to_string

let gather_points (x1, y1) (x2, y2) grid =
  let rec gather (x, y) dx dy acc =
    let next = (x + dx, y + dy) in
    if not (check_coord_in_grid next grid) then acc
    else gather next dx dy (next :: acc)
  in
  let dx = x1 - x2 in
  let dy = y1 - y2 in
  (gather (x2, y2) (-dx) (-dy) [ (x2, y2) ], gather (x1, y1) dx dy [ (x1, y1) ])

let part2_points coord_map grid =
  Hashtbl.fold coord_map ~init:[] ~f:(fun ~key:_ ~data:coords acc ->
      let rec compute_pairs acc = function
        | [] -> acc
        | c1 :: rest ->
            let new_pairs =
              List.map rest ~f:(fun c2 ->
                  let points = gather_points c1 c2 grid in
                  points)
            in
            compute_pairs (acc @ new_pairs) rest
      in
      acc @ compute_pairs [] coords)

let solve_part_2 input =
  let unique_freqs = input |> get_frequencies in
  let grid = input |> make_grid in
  let coord_map = find_coordinates grid unique_freqs in
  let anode_lists = part2_points coord_map grid in
  let set = ref (Set.empty (module String)) in
  List.iter anode_lists ~f:(fun anodes ->
      List.iter (fst anodes) ~f:(fun anode ->
          set := Set.add !set (Printf.sprintf "%d,%d" (fst anode) (snd anode)));
      List.iter (snd anodes) ~f:(fun anode ->
          set := Set.add !set (Printf.sprintf "%d,%d" (fst anode) (snd anode))));
  Set.length !set |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
