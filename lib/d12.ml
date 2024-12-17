open Base
open Read_input

let directions = [ (0, 1); (0, -1); (-1, 0); (1, 0) ]
let tuple_equal (x1, y1) (x2, y2) = Int.equal x1 x2 && Int.equal y1 y2

let out_of_bounds x grid =
  fst x < 0
  || fst x > List.length grid - 1
  || snd x < 0
  || snd x > String.length (List.nth_exn grid (fst x)) - 1

let get_neighbors cur plant grid =
  let neighbors =
    List.map directions ~f:(fun x -> (fst cur + fst x, snd cur + snd x))
  in

  List.filter_map neighbors ~f:(fun x ->
      if out_of_bounds x grid then None
      else
        let nxt_pl = String.get (List.nth_exn grid (fst x)) (snd x) in
        match Char.equal nxt_pl plant with
        | false -> None
        | true -> Some (fst x, snd x))

let count_corners pos grid cur =
  let corner_dirs =
    [
      ((1, 0), (0, -1)); ((1, 0), (0, 1)); ((-1, 0), (0, -1)); ((-1, 0), (0, 1));
    ]
  in

  let corners = ref 0 in

  List.iter corner_dirs ~f:(fun x ->
      let adjacent1 = (fst pos + fst (fst x), snd pos + snd (fst x)) in
      let adjacent2 = (fst pos + fst (snd x), snd pos + snd (snd x)) in
      let diag =
        ( fst pos + fst (fst x) + fst (snd x),
          snd pos + snd (fst x) + snd (snd x) )
      in

      let adjacent1_plant =
        if out_of_bounds adjacent1 grid then '*'
        else String.get (List.nth_exn grid (fst adjacent1)) (snd adjacent1)
      in
      let adjacent2_plant =
        if out_of_bounds adjacent2 grid then '*'
        else String.get (List.nth_exn grid (fst adjacent2)) (snd adjacent2)
      in
      let diag_plant =
        if out_of_bounds diag grid then '*'
        else String.get (List.nth_exn grid (fst diag)) (snd diag)
      in

      if
        (not (Char.equal adjacent1_plant cur))
        && not (Char.equal adjacent2_plant cur)
        || Char.equal adjacent1_plant cur
           && Char.equal adjacent2_plant cur
           && not (Char.equal diag_plant cur)
      then corners := !corners + 1);
  !corners

let dfs (start : int * int) plant grid =
  let perim_acc = ref 0 in
  let rec search (stack : (int * int) list) visited =
    match stack with
    | [] -> (visited, List.length visited * !perim_acc)
    | hd :: tl ->
        if List.mem visited ~equal:tuple_equal hd then search tl visited
        else
          let neighbors = get_neighbors hd plant grid in
          perim_acc := !perim_acc + (4 - List.length neighbors);
          let filtered_neighbors =
            List.filter neighbors ~f:(fun x ->
                not (List.mem visited ~equal:tuple_equal x))
          in
          search (filtered_neighbors @ tl) (hd :: visited)
  in
  search [ start ] []

let bfs (start : int * int) plant grid =
  let rec search (stack : (int * int) list) visited =
    match stack with
    | [] -> (visited, List.length visited)
    | hd :: tl ->
        if List.mem visited ~equal:tuple_equal hd then search tl visited
        else
          let neighbors = get_neighbors hd plant grid in
          let filtered_neighbors =
            List.filter neighbors ~f:(fun x ->
                not (List.mem visited ~equal:tuple_equal x))
          in
          search (tl @ filtered_neighbors) (hd :: visited)
  in
  search [ start ] []

let solve_part_1 input =
  let cumulative_price = ref 0 in
  let lines = input |> Read_input.string_to_lines in
  let visited_set = ref Set.Poly.empty in
  for r = 0 to List.length lines - 1 do
    for c = 0 to String.length (List.nth_exn lines r) - 1 do
      let cur = String.get (List.nth_exn lines r) c in
      if not (Set.Poly.mem !visited_set (r, c)) then (
        let res = dfs (r, c) cur lines in
        cumulative_price := !cumulative_price + snd res;
        List.iter (fst res) ~f:(fun x ->
            visited_set := Set.Poly.add !visited_set x))
    done
  done;

  !cumulative_price |> Int.to_string

let solve_part_2 input =
  let cumulative_price = ref 0 in
  let lines = input |> Read_input.string_to_lines in
  let visited_set = ref Set.Poly.empty in
  for r = 0 to List.length lines - 1 do
    for c = 0 to String.length (List.nth_exn lines r) - 1 do
      let cur = String.get (List.nth_exn lines r) c in
      let corners = ref 0 in
      if not (Set.Poly.mem !visited_set (r, c)) then (
        let res = bfs (r, c) cur lines in
        List.iter (fst res) ~f:(fun x ->
            corners :=
              if not (Set.Poly.mem !visited_set (r, c)) then
                !corners + count_corners x lines cur
              else !corners;
            visited_set := Set.Poly.add !visited_set x);
        cumulative_price := !cumulative_price + (snd res * !corners))
    done
  done;

  !cumulative_price |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
