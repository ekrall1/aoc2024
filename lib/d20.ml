open Read_input
open Base

(* Race condition
   part 1 - find the number of shortcuts that save ge 100 ps
   part 2 - now the shortcuts can have any distance up to 20
*)

(* hash table with tuple keys *)
module TupleHT = struct
  module T = struct
    type t = int * int [@@deriving sexp]

    let compare (x1, y1) (x2, y2) =
      match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

    let rec sexp_of_t t = sexp_of_t t
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

type grid = Grid of { layout : string array; rows : int; cols : int }
type start_end = StartEnd of { start_pos : int * int; end_pos : int * int }

type shortcuts =
  | Shortcuts of { sc : (TupleHT.t, ((int * int) * int) list) Base.Hashtbl.t }

type input =
  | Input of { grid : grid; start_end : start_end; shortcuts : shortcuts }

type directions = Directions of { dydx : (int * int) list }

type dfs_result =
  | DFSResult of {
      visited : (int * int) list;
      length : int;
      distance_map : (TupleHT.t, int) Base.Hashtbl.t;
    }

(* some helpers *)
let tuple_equal (x1, y1) (x2, y2) = Int.equal x1 x2 && Int.equal y1 y2

let out_of_bounds pos (Grid grid : grid) =
  fst pos < 0
  || fst pos > grid.rows - 1
  || snd pos < 0
  || snd pos > grid.cols - 1

let dirs : directions =
  Directions { dydx = [ (0, 1); (0, -1); (-1, 0); (1, 0) ] }

let get_neighbors cur_pos (Grid grid : grid) offset =
  let layout = grid.layout in

  let neighbors =
    let dydx = match dirs with Directions { dydx } -> dydx in
    List.map dydx ~f:(fun (dx, dy) ->
        (fst cur_pos + (dx * offset), snd cur_pos + (dy * offset)))
  in

  List.filter neighbors ~f:(fun (i, j) ->
      (not (out_of_bounds (i, j) (Grid grid)))
      && (not (tuple_equal (i, j) cur_pos))
      && not (Char.equal (String.get layout.(i) j) '#'))

let manhattan_distance (x1, y1) (x2, y2) = Int.abs (x2 - x1) + Int.abs (y2 - y1)

let get_shortcuts cur_pos coords dist =
  let skips = ref [] in
  let neighbors =
    List.filter coords ~f:(fun n ->
        (not (tuple_equal n cur_pos)) && manhattan_distance cur_pos n <= dist)
  in
  List.iter neighbors ~f:(fun m ->
      skips := (m, manhattan_distance cur_pos m) :: !skips);
  !skips

(* input *)
let check_for_shortcuts (path_lst : (int * int) list) dist =
  let shortcuts_hm = Hashtbl.create (module TupleHT) in

  List.iter path_lst ~f:(fun (i, j) ->
      let shortcut_data = get_shortcuts (i, j) path_lst dist in
      Hashtbl.set shortcuts_hm ~key:(i, j) ~data:shortcut_data);

  shortcuts_hm

let set_inputs (Grid grid : grid) ~part =
  let rows = grid.rows in
  let cols = grid.cols in
  let layout = grid.layout in

  let rec points_on_path i j start_pos ending lst =
    if i >= rows then ((start_pos, ending), lst)
    else if j >= cols then points_on_path (i + 1) 0 start_pos ending lst
    else
      let elem = String.get layout.(i) j in
      match elem with
      | '#' -> points_on_path i (j + 1) start_pos ending lst
      | 'E' ->
          let lst = (i, j) :: lst in
          points_on_path i (j + 1) start_pos (i, j) lst
      | 'S' ->
          let lst = (i, j) :: lst in
          points_on_path i (j + 1) start_pos (i, j) lst
      | _ ->
          let lst = (i, j) :: lst in
          points_on_path i (j + 1) start_pos ending lst
  in

  let (start_pos, end_pos), path_lst = points_on_path 0 0 (0, 0) (0, 0) [] in

  let shortcut_max = if part = 1 then 2 else 20 in
  let shortcuts_hm = check_for_shortcuts path_lst shortcut_max in
  (StartEnd { start_pos; end_pos }, Shortcuts { sc = shortcuts_hm })

let make_input_data str_input ~part =
  let arr = str_input |> Read_input.string_to_lines |> Array.of_list in
  let grid =
    Grid { layout = arr; rows = Array.length arr; cols = String.length arr.(0) }
  in
  let start_end, shortcuts = set_inputs grid ~part in
  Input { grid; start_end; shortcuts }

(* solution algorithms *)
let dfs grid start : dfs_result =
  let traveled = Hashtbl.create (module TupleHT) in

  let rec search (stack : (int * int) list) visited dist =
    match stack with
    | [] ->
        DFSResult
          { visited; length = List.length visited - 1; distance_map = traveled }
    | hd :: tl ->
        if List.mem visited ~equal:tuple_equal hd then search tl visited dist
        else (
          Hashtbl.set traveled ~key:(fst hd, snd hd) ~data:dist;
          let neighbors = get_neighbors hd grid 1 in
          let filtered_neighbors =
            List.filter neighbors ~f:(fun x ->
                not (List.mem visited ~equal:tuple_equal x))
          in
          search (filtered_neighbors @ tl) (hd :: visited) (dist + 1))
  in

  search [ start ] [] 0

let get_shortcut_count (Shortcuts shortcuts : shortcuts)
    (DFSResult res : dfs_result) (threshold : int) : int * int =
  let p2_shortcuts = shortcuts.sc in
  let distances = res.distance_map in
  let savings = ref [] in
  Hashtbl.iter_keys p2_shortcuts ~f:(fun key ->
      let initial_distance =
        Hashtbl.find distances key |> Option.value ~default:0
      in
      let shorter_distances =
        Hashtbl.find p2_shortcuts key
        |> Option.value ~default:[ ((-1, -1), -1) ]
      in
      List.iter shorter_distances ~f:(fun d ->
          savings :=
            initial_distance
            - (Hashtbl.find distances (fst d) |> Option.value ~default:0)
            - snd d
            :: !savings));
  let filtered_savings_ge_threshold =
    List.filter !savings ~f:(fun x -> x > 0 && x >= threshold)
  in
  let filtered_savings_eq_threshold =
    List.filter filtered_savings_ge_threshold ~f:(fun x -> x = threshold)
  in
  ( List.length filtered_savings_ge_threshold,
    List.length filtered_savings_eq_threshold )

let solve_part_1 (file_name : string) (threshold : int) =
  let (Input input) =
    file_name |> Read_input.read_input_file |> make_input_data ~part:1
  in
  let (StartEnd start_end) = input.start_end in
  let (Shortcuts shortcuts) = input.shortcuts in
  let (DFSResult res) = dfs input.grid start_end.end_pos in
  get_shortcut_count (Shortcuts shortcuts) (DFSResult res) threshold

let solve_part_2 (file_name : string) (threshold : int) =
  let (Input input) =
    file_name |> Read_input.read_input_file |> make_input_data ~part:2
  in
  let (StartEnd start_end) = input.start_end in
  let (Shortcuts shortcuts) = input.shortcuts in
  let (DFSResult res) = dfs input.grid start_end.end_pos in
  get_shortcut_count (Shortcuts shortcuts) (DFSResult res) threshold

let part1 (file_name : string) : string =
  let threshold =
    if String.is_substring file_name ~substring:"test_data" then 2 else 100
  in
  let ans = solve_part_1 file_name threshold in
  Printf.sprintf "ge: %d, eq: %d" (fst ans) (snd ans)

let part2 (file_name : string) : string =
  let threshold =
    if String.is_substring file_name ~substring:"test_data" then 76 else 100
  in
  let ans = solve_part_2 file_name threshold in
  Printf.sprintf "ge: %d, eq: %d" (fst ans) (snd ans)
