open Read_input
open Base

(**
Reindeer Maze
part 1 - minimum number of steps through the maze, with 1000 pt penaltiy for each turn
part 2 - find the unumber of nique points across all best paths through the maze
*)

let get_start_end grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in

  let startpos = ref (0, 0) in
  let endpos = ref (0, 0) in

  for i = 0 to rows - 1 do
    let rowstr = grid.(i) in
    for j = 0 to cols - 1 do
      if Char.equal (String.get rowstr j) 'S' then startpos := (i, j);
      if Char.equal (String.get rowstr j) 'E' then endpos := (i, j)
    done
  done;

  (!startpos, !endpos)

let directions = [ (1, 0); (-1, 0); (0, -1); (0, 1) ]

let allowed_directions cur_dir =
  let dx, dy = cur_dir in
  [ (dx, dy); (dy, -dx); (-dy, dx) ]

let initialize_hm hm grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let dir_arr = Array.of_list directions in
  let dir_rows = Array.length dir_arr in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      for k = 0 to dir_rows - 1 do
        let dir = dir_arr.(k) in
        let pos_str = Printf.sprintf "%d,%d,%d,%d" (fst dir) (snd dir) i j in
        Hashtbl.set hm ~key:pos_str ~data:Int.max_value
      done
    done
  done;
  hm

let valid_pos grid pos rows cols =
  let x = fst pos in
  let y = snd pos in
  let ch = String.get grid.(x) y in
  x < rows && y < cols && not (Char.equal ch '#')

let push lst (priority, (dir, value)) =
  lst :=
    List.merge
      ~compare:(fun (p1, (_, _)) (p2, (_, _)) -> Int.compare p1 p2)
      !lst
      [ (priority, (dir, value)) ]

let pop lst =
  match !lst with
  | [] -> None
  | hd :: tl ->
      lst := tl;
      Some hd

(* logic to handle updates to hash maps for distance and predecessors by maze position*)

let update_end_new_dist distances pred new_distance str_cur_pos str_end_pos
    str_neighbor =
  let update = ref None in
  let cur_min =
    Hashtbl.find distances str_end_pos |> Option.value ~default:Int.max_value
  in
  if new_distance < cur_min then (
    Hashtbl.set distances ~key:str_end_pos ~data:new_distance;
    update := Some str_neighbor;
    Hashtbl.set pred ~key:str_neighbor ~data:[ str_cur_pos ]);

  !update

let update_end_add_dist pred str_cur_pos str_end_pos =
  let cur_lst = Hashtbl.find pred str_end_pos |> Option.value ~default:[] in
  Hashtbl.set pred ~key:str_end_pos ~data:(cur_lst @ [ str_cur_pos ]);
  ()

let gather_preds str_start_pos str_end_pos preds =
  let tiles = ref (Set.empty (module String)) in
  let queue = Queue.create () in
  let visited = ref (Set.empty (module String)) in
  Queue.enqueue queue str_end_pos;

  let rec find_set_len () =
    if Queue.is_empty queue then Set.length !tiles
    else
      let p = Queue.dequeue_exn queue in
      let p_str_lst = String.split ~on:',' p in
      let p_str =
        String.concat ~sep:","
          [ List.nth_exn p_str_lst 2; List.nth_exn p_str_lst 3 ]
      in
      if not (String.equal p_str str_start_pos) then
        if not false then (
          tiles := Set.add !tiles p_str;
          visited := Set.add !visited p_str;
          List.iter (Hashtbl.find_exn preds p) ~f:(fun x ->
              Queue.enqueue queue x));
      find_set_len ()
  in

  find_set_len () + 1

let dijkstra grid =
  let start_pos, end_pos = get_start_end grid in
  let rows = Array.length grid in
  let cols = String.length grid.(0) in

  let distances = Hashtbl.create (module String) in
  let pred = Hashtbl.create (module String) in
  let visited = Hashtbl.create (module String) in
  let end_long_str = ref "" in

  let queue = ref [] in
  let min_dist = ref 0 in

  let str_start_pos =
    Printf.sprintf "%d,%d,%d,%d" 0 1 (fst start_pos) (snd start_pos)
  in
  let str_end_pos = Printf.sprintf "%d,%d" (fst end_pos) (snd end_pos) in

  let distances = initialize_hm distances grid in

  Hashtbl.set distances ~key:str_start_pos ~data:0;
  push queue (0, ((0, 1), start_pos));

  while not (List.is_empty !queue) do
    match pop queue with
    | None -> ()
    | Some (_, (cur_pos_dir, (x, y))) ->
        let str_cur_pos =
          Printf.sprintf "%d,%d,%d,%d" (fst cur_pos_dir) (snd cur_pos_dir) x y
        in

        let filter_dirs = allowed_directions cur_pos_dir in

        Hashtbl.set visited ~key:str_cur_pos ~data:"yes";

        List.iter filter_dirs ~f:(fun (dx, dy) ->
            let neighbor = (x + dx, y + dy) in
            let str_neighbor =
              Printf.sprintf "%d,%d,%d,%d" dx dy (fst neighbor) (snd neighbor)
            in
            if valid_pos grid neighbor rows cols then
              let turn =
                Int.abs (dx * fst cur_pos_dir) + Int.abs (dy * snd cur_pos_dir)
              in

              (* adder for turns *)
              let adder = if turn = 0 then 1000 else 0 in
              let cur_distance =
                adder
                + (Hashtbl.find distances str_cur_pos
                  |> Option.value ~default:Int.max_value)
              in

              let neighbor_distance =
                Hashtbl.find distances str_neighbor
                |> Option.value ~default:Int.max_value
              in

              (* adder for taking a step forward *)
              let new_distance = cur_distance + 1 in

              let found_end =
                fst neighbor = fst end_pos && snd neighbor = snd end_pos
              in

              (* if it is the best path to the neighbor, update*)
              if new_distance < neighbor_distance then (
                Hashtbl.set distances ~key:str_neighbor ~data:new_distance;

                (* put the neighbor in the queue*)
                push queue
                  (new_distance, ((dx, dy), (fst neighbor, snd neighbor)));

                (* for part 2, keep predecessors map*)
                Hashtbl.set pred ~key:str_neighbor ~data:[ str_cur_pos ];

                (* got to the end of the maze*)
                if found_end then
                  let check_end =
                    update_end_new_dist distances pred new_distance str_cur_pos
                      str_end_pos str_neighbor
                  in
                  end_long_str :=
                    match check_end with None -> !end_long_str | Some x -> x)
              else if new_distance = neighbor_distance then (
                (* if the distance is a tie, keep track of predecessor list for part 2*)
                let cur_lst =
                  Hashtbl.find pred str_neighbor |> Option.value ~default:[]
                in
                Hashtbl.set pred ~key:str_neighbor
                  ~data:(cur_lst @ [ str_cur_pos ]);

                if found_end then
                  update_end_add_dist pred str_cur_pos str_end_pos))
  done;

  let str_start_pos = Printf.sprintf "%d,%d" (fst start_pos) (snd start_pos) in

  min_dist :=
    Hashtbl.find distances str_end_pos |> Option.value ~default:Int.max_value;

  (!min_dist, gather_preds str_start_pos !end_long_str pred)

let solve_part_1 input =
  let grid = input |> Read_input.string_to_lines |> Array.of_list in
  dijkstra grid |> fst |> Int.to_string

let solve_part_2 input =
  let grid = input |> Read_input.string_to_lines |> Array.of_list in
  dijkstra grid |> snd |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) =
  file_name |> Read_input.read_input_file |> solve_part_2
