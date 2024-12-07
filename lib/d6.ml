open Base
open Read_input

let string_to_lines string_input =
  (* parse input string to a list of strings (lines)*)
  Stdlib.String.split_on_char '\n' string_input

let dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let get_initial_coords (grid : string list) =
  let rows = List.length grid - 1 in
  let cols = String.length (List.nth_exn grid 0) - 1 in
  let rec find_in_row r =
    if r > rows then None
    else
      let rec find_in_col c =
        if c > cols then None
        else if Char.equal (String.get (List.nth_exn grid r) c) '^' then
          Some (r, c)
        else find_in_col (c + 1)
      in
      match find_in_col 0 with
      | Some coords -> Some coords
      | None -> find_in_row (r + 1)
  in
  find_in_row 0

let next_orientation cur =
  match cur with
  | 1, 0 -> (0, -1)
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 0, -1 -> (-1, 0)
  | _ -> failwith "invalid direction"

let move_to_exit coords orientation grid =
  let visited = Hash_set.create (module String) in
  let rows = List.length grid - 1 in
  let cols = String.length (List.hd_exn grid) - 1 in
  let rec check_next pos orient movements =
    let next_r = fst pos + fst orient in
    let next_c = snd pos + snd orient in
    let str_pos = Printf.sprintf "%d,%d" next_r next_c in
    if next_r < 0 || next_r > rows || next_c < 0 || next_c > cols then movements
    else if Char.equal (String.get (List.nth_exn grid next_r) next_c) '#' then
      check_next pos (next_orientation orient) movements
    else if Hash_set.mem visited str_pos then
      (* If position already visited, skip increment *)
      check_next (next_r, next_c) orient movements
    else
      let () = Hash_set.add visited str_pos in
      check_next (next_r, next_c) orient (movements + 1)
  in
  let str_pos = Printf.sprintf "%d,%d" (fst coords) (snd coords) in
  let () = Hash_set.add visited str_pos in
  check_next coords orientation 1

let check_cycle coords orientation grid block_pos =
  let visited = Hash_set.create (module String) in
  let rows = List.length grid - 1 in
  let cols = String.length (List.hd_exn grid) - 1 in
  let rec check_next pos orient =
    let next_r = fst pos + fst orient in
    let next_c = snd pos + snd orient in
    let str_pos =
      Printf.sprintf "%d,%d,%d,%d" next_r next_c (fst orient) (snd orient)
    in
    if next_r < 0 || next_r > rows || next_c < 0 || next_c > cols then false
    else if
      Char.equal (String.get (List.nth_exn grid next_r) next_c) '#'
      || String.equal block_pos (Printf.sprintf "%d,%d" next_r next_c)
    then check_next pos (next_orientation orient)
    else if Hash_set.mem visited str_pos then
      (* If position already visited, and in the same direction, it's a loop *)
      true
    else
      let () = Hash_set.add visited str_pos in
      check_next (next_r, next_c) orient
  in
  let str_pos =
    Printf.sprintf "%d,%d,%d,%d" (fst coords) (snd coords) (fst orientation)
      (snd orientation)
  in
  let () = Hash_set.add visited str_pos in
  check_next coords orientation

let part2_check coords orientation grid =
  let rows = List.length grid - 1 in
  let cols = String.length (List.hd_exn grid) - 1 in
  let checks = ref [] in
  for r = 0 to rows do
    for c = 0 to cols do
      if
        Char.equal (String.get (List.nth_exn grid r) c) '#'
        || Char.equal (String.get (List.nth_exn grid r) c) '^'
      then checks := !checks @ [ false ]
      else
        let block_pos = Printf.sprintf "%d,%d" r c in
        checks := !checks @ [ check_cycle coords orientation grid block_pos ]
    done
  done;
  List.fold_left !checks ~init:0 ~f:(fun acc i -> if i then acc + 1 else acc)

let solve_part_1 (input : string) =
  let grid = string_to_lines input in
  let orientation = List.nth_exn dirs 1 in
  let coords = get_initial_coords grid in
  match coords with
  | Some c -> move_to_exit c orientation grid |> Int.to_string
  | None -> failwith "cannot find coordinates"

let solve_part_2 (input : string) =
  let grid = string_to_lines input in
  let orientation = List.nth_exn dirs 1 in
  let coords = get_initial_coords grid in
  match coords with
  | Some c -> part2_check c orientation grid |> Int.to_string
  | None -> failwith "cannot find coordinates"

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
