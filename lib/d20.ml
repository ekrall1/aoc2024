open Read_input
open Base

(* Race condition
   part 1 - find the number of shortcuts that save ge 100 ps
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
  | Shortcuts of { p1 : (TupleHT.t, (int * int) list) Base.Hashtbl.t }

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

let get_shortcuts_p1 cur_pos (Grid grid : grid) offset =
  let layout = grid.layout in
  let dydx = match dirs with Directions { dydx } -> dydx in

  let next_neighbors =
    List.map dydx ~f:(fun (dy, dx) ->
        (fst cur_pos + (dy * offset), snd cur_pos + (dx * offset)))
  in

  let walls =
    List.filter next_neighbors ~f:(fun (i, j) ->
        (not (out_of_bounds (i, j) (Grid grid)))
        && (not (tuple_equal (i, j) cur_pos))
        && Char.equal (String.get layout.(i) j) '#')
  in

  let shortcuts = ref [] in
  List.iter walls ~f:(fun (i, j) ->
      List.iter dydx ~f:(fun (dy, dx) ->
          shortcuts := (i + dy, j + dx) :: !shortcuts));

  List.filter !shortcuts ~f:(fun (i, j) ->
      (not (out_of_bounds (i, j) (Grid grid)))
      && (not (tuple_equal (i, j) cur_pos))
      && not (Char.equal (String.get layout.(i) j) '#'))

(* input *)
let check_for_shortcuts_p1 cur_pos (Grid grid : grid) =
  let skips = get_shortcuts_p1 cur_pos (Grid grid) 1 in
  skips

let set_inputs_p1 (Grid grid : grid) =
  let rows = grid.rows in
  let cols = grid.cols in
  let layout = grid.layout in
  let shortcuts_hm = Hashtbl.create (module TupleHT) in

  let rec finder i j starting ending =
    if i >= rows then (starting, ending)
    else if j >= cols then finder (i + 1) 0 starting ending
    else
      let elem = String.get layout.(i) j in
      match elem with
      | 'E' ->
          Hashtbl.set shortcuts_hm ~key:(i, j)
            ~data:(check_for_shortcuts_p1 (i, j) (Grid grid));
          finder i (j + 1) starting (i, j)
      | 'S' ->
          Hashtbl.set shortcuts_hm ~key:(i, j)
            ~data:(check_for_shortcuts_p1 (i, j) (Grid grid));
          finder i (j + 1) (i, j) ending
      | '.' ->
          Hashtbl.set shortcuts_hm ~key:(i, j)
            ~data:(check_for_shortcuts_p1 (i, j) (Grid grid));
          finder i (j + 1) starting ending
      | _ -> finder i (j + 1) starting ending
  in

  let startend = finder 0 0 (0, 0) (0, 0) in
  ( StartEnd { start_pos = fst startend; end_pos = snd startend },
    Shortcuts { p1 = shortcuts_hm } )

let make_input_data str_input =
  let arr = str_input |> Read_input.string_to_lines |> Array.of_list in
  let grid =
    Grid { layout = arr; rows = Array.length arr; cols = String.length arr.(0) }
  in
  let start_end, shortcuts = set_inputs_p1 grid in
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

let get_shortcut_count_p1 (Shortcuts shortcuts : shortcuts)
    (DFSResult res : dfs_result) (threshold : int) : int * int =
  let p1_shortcuts = shortcuts.p1 in
  let distances = res.distance_map in
  let savings = ref [] in
  Hashtbl.iter_keys p1_shortcuts ~f:(fun key ->
      let initial_distance =
        Hashtbl.find distances key |> Option.value ~default:0
      in
      let shorter_distances =
        Hashtbl.find p1_shortcuts key |> Option.value ~default:[ (-1, -1) ]
      in
      List.iter shorter_distances ~f:(fun d ->
          savings :=
            initial_distance
            - (Hashtbl.find distances d |> Option.value ~default:0)
            - 2
            :: !savings));
  let filtered_savings_ge_threshold =
    List.filter !savings ~f:(fun x -> x > 0 && x >= threshold)
  in
  let filtered_savings_eq_threshold =
    List.filter filtered_savings_ge_threshold ~f:(fun x -> x = threshold)
  in
  ( List.length filtered_savings_ge_threshold,
    List.length filtered_savings_eq_threshold )

let solve_part_1 (Input input : input) (threshold : int) =
  let (StartEnd start_end) = input.start_end in
  let (Shortcuts shortcuts) = input.shortcuts in
  let (DFSResult res) = dfs input.grid start_end.end_pos in
  get_shortcut_count_p1 (Shortcuts shortcuts) (DFSResult res) threshold

let solve_part_2 file_name = file_name

let part1 (file_name : string) : string =
  let (Input input) =
    file_name |> Read_input.read_input_file |> make_input_data
  in
  let threshold =
    if String.is_substring file_name ~substring:"test_data" then 2 else 100
  in
  let ans = solve_part_1 (Input input) threshold in
  Printf.sprintf "ge: %d, eq: %d" (fst ans) (snd ans)

let part2 (file_name : string) : string = solve_part_2 file_name
