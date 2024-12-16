open Base
open Read_input

let directions =
  [
    (1, 0); (* right *) (-1, 0); (* left *) (0, 1); (* up *) (0, -1); (* down *)
  ]

let rec explore lst row col row_len col_len visited =
  let current_visited = !visited in
  let updated_visited = Set.Poly.add current_visited (row, col) in
  visited := updated_visited;

  let found_nine = ref 0 in

  List.iter directions ~f:(fun (dr, dc) ->
      let nextrow = row + dr in
      let nextcol = col + dc in

      if
        nextrow >= 0 && nextrow <= row_len && nextcol >= 0 && nextcol <= col_len
        && not (Set.Poly.mem updated_visited (nextrow, nextcol))
      then
        let cur_val = List.nth_exn (List.nth_exn lst row) col in
        let next_val = List.nth_exn (List.nth_exn lst nextrow) nextcol in
        if cur_val + 1 = next_val then
          if next_val = 9 then (
            (* update the visited set and 9s *)
            let current_visited = !visited in
            let updated_visited =
              Set.Poly.add current_visited (nextrow, nextcol)
            in
            visited := updated_visited;
            found_nine := !found_nine + 1)
          else
            let sub_acc = explore lst nextrow nextcol row_len col_len visited in
            found_nine := !found_nine + sub_acc);

  !found_nine

let rec explore_p2 lst row col row_len col_len visited =
  let updated_visited = Set.Poly.add visited (row, col) in
  let found_nine = ref 0 in

  List.iter directions ~f:(fun (dr, dc) ->
      let nextrow = row + dr in
      let nextcol = col + dc in
      if
        nextrow >= 0 && nextrow <= row_len && nextcol >= 0 && nextcol <= col_len
        && not (Set.Poly.mem updated_visited (nextrow, nextcol))
      then
        let cur_val = List.nth_exn (List.nth_exn lst row) col in
        let next_val = List.nth_exn (List.nth_exn lst nextrow) nextcol in
        if cur_val + 1 = next_val then
          if next_val = 9 then found_nine := !found_nine + 1
          else
            let sub_acc =
              explore_p2 lst nextrow nextcol row_len col_len updated_visited
            in
            found_nine := !found_nine + sub_acc);

  !found_nine

let count_good_paths lst row col row_len col_len =
  let visited = ref Set.Poly.empty in
  let acc = explore lst row col row_len col_len visited in
  (* debug *)
  (* Stdlib.Printf.printf "trailhead (%d, %d), 9s %d\n" row col acc; *)
  acc

let count_good_paths_p2 lst row col row_len col_len =
  let initial_visited = Set.Poly.empty in
  explore_p2 lst row col row_len col_len initial_visited

let travel_list lst (row_len, col_len) part =
  let good_trailheads = ref 0 in
  for row = 0 to row_len do
    for col = 0 to col_len do
      if List.nth_exn (List.nth_exn lst row) col = 0 then
        good_trailheads :=
          !good_trailheads
          +
          if part = 1 then count_good_paths lst row col row_len col_len
          else count_good_paths_p2 lst row col row_len col_len
    done
  done;

  !good_trailheads

let convert_to_int lst =
  let rows = List.length lst - 1 in

  let newlst = ref [] in

  for row = 0 to rows do
    let newrow =
      List.map
        (String.to_list (List.nth_exn lst row))
        ~f:(fun x -> Int.of_string (String.of_char x))
    in
    newlst := newrow :: !newlst
  done;

  List.rev !newlst

let solve_part_1 input =
  let init_lst = Read_input.string_to_lines input |> convert_to_int in
  let rowcol_lens =
    (List.length init_lst - 1, List.length (List.hd_exn init_lst) - 1)
  in
  travel_list init_lst rowcol_lens 1 |> Int.to_string

let solve_part_2 input =
  let init_lst = Read_input.string_to_lines input |> convert_to_int in
  let rowcol_lens =
    (List.length init_lst - 1, List.length (List.hd_exn init_lst) - 1)
  in
  travel_list init_lst rowcol_lens 2 |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
