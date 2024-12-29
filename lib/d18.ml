open Read_input
open Base

(* RAM run
   part 1 - minimum number of steps through the maze after 1024 bytes fall
   part 2 - first falling byte that blocks all exits
*)

module IntPair = struct
  type t = int * int [@@deriving sexp]

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let rec sexp_of_t t = sexp_of_t t
  end)
end

let push lst (priority, value) =
  lst :=
    List.merge
      ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
      !lst
      [ (priority, value) ]

let pop lst =
  match !lst with
  | [] -> None
  | hd :: tl ->
      lst := tl;
      Some hd

let directions = [ (1, 0); (-1, 0); (0, -1); (0, 1) ]

let check_valid grid x y visited_set =
  if x >= 0 && x < Array.length grid && y >= 0 && y < Array.length grid.(0) then
    let elem = grid.(x).(y) in
    let in_visited = Set.mem !visited_set (x, y) in
    (not (String.equal elem "#")) && not in_visited
  else false

let initialize_grid size falling bytes =
  let arr = Array.init size ~f:(fun _ -> Array.create ~len:size "") in
  let input_bytes = Array.sub falling ~pos:0 ~len:bytes in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      arr.(i).(j) <- Printf.sprintf "."
    done
  done;

  for i = 0 to Array.length input_bytes - 1 do
    let by, bx = input_bytes.(i) in
    arr.(bx).(by) <- Printf.sprintf "#"
  done;

  arr

let initialize_grid_p2 size =
  let arr = Array.init size ~f:(fun _ -> Array.create ~len:size "") in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      arr.(i).(j) <- Printf.sprintf "."
    done
  done;
  arr

let update_grid_p2 arr (bx, by) =
  arr.(bx).(by) <- Printf.sprintf "#";
  arr

let dijkstra grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in

  let visited = ref (Set.empty (module IntPair)) in

  let queue = ref [] in
  let min_dist = ref 0 in

  push queue (0, (0, 0));

  while not (List.is_empty !queue) do
    match pop queue with
    | None -> ()
    | Some (dist, (x, y)) ->
        if x = rows - 1 && y = cols - 1 then min_dist := dist
        else
          List.iter directions ~f:(fun (dx, dy) ->
              let nx, ny = (x + dx, y + dy) in
              if check_valid grid nx ny visited then (
                push queue (dist + 1, (nx, ny));
                visited := Set.add !visited (nx, ny)))
  done;

  !min_dist

let convert_byte_to_tuple byte =
  let lst = String.split ~on:',' byte in
  (Int.of_string (List.hd_exn lst), Int.of_string (List.nth_exn lst 1))

let solve_part_1 input size bytes =
  let falling =
    input |> Read_input.string_to_lines |> Array.of_list
    |> Array.map ~f:(fun x -> convert_byte_to_tuple x)
  in
  let grid = initialize_grid size falling bytes in
  dijkstra grid |> Int.to_string

let solve_part_2 input size =
  let falling =
    input |> Read_input.string_to_lines |> Array.of_list
    |> Array.map ~f:(fun x -> convert_byte_to_tuple x)
  in
  let grid = initialize_grid_p2 size in

  let rec search_p2 grid byte =
    let cur_byte = falling.(byte) in
    let new_grid = update_grid_p2 grid cur_byte in
    let res = dijkstra new_grid in
    match res with
    | 0 ->
        String.concat ~sep:","
          [ Int.to_string (fst cur_byte); Int.to_string (snd cur_byte) ]
    | _ -> search_p2 new_grid (byte + 1)
  in

  search_p2 grid 0

let part1 (file_name : string) : string =
  let size = ref 0 in
  let bytes = ref 0 in
  if String.is_substring file_name ~substring:"test_data" then (
    size := 7;
    bytes := 12)
  else (
    size := 71;
    bytes := 1024);

  let input = file_name |> Read_input.read_input_file in
  solve_part_1 input !size !bytes

let part2 (file_name : string) : string =
  let size = ref 0 in
  let bytes = ref 0 in
  if String.is_substring file_name ~substring:"test_data" then (
    size := 7;
    bytes := 12)
  else (
    size := 71;
    bytes := 1024);

  let input = file_name |> Read_input.read_input_file in
  solve_part_2 input !size
