open Base
open Read_input

let dirs =
  [
    (0, 1);
    (* Horizontal right *)
    (0, -1);
    (* Horizontal left *)
    (1, 0);
    (* Vertical down *)
    (-1, 0);
    (* Vertical up *)
    (1, 1);
    (* Diagonal down-right *)
    (1, -1);
    (* Diagonal down-left *)
    (-1, 1);
    (* Diagonal up-right *)
    (-1, -1);
    (* Diagonal up-left *)
  ]

let square_dirs = [ (1, 1); (-1, 1); (-1, -1); (1, -1) ]

let is_ok_true = function
  | Base.List.Or_unequal_lengths.Ok true -> true
  | _ -> false

let rows_cols_length (lst : string list) =
  let rows = List.length lst in
  let cols =
    String.length (match List.hd lst with Some x -> x | None -> "")
  in
  (rows, cols)

let char_getter lst row col (row_len, col_len) =
  if row >= 0 && col >= 0 && row < row_len && col < col_len then
    match Stdlib.List.nth_opt lst row with
    | Some x -> Some (String.get x col)
    | None -> None
  else None

let rec check_if_word_exists_in_direction grid (row, col) (drow, dcol)
    (row_len, col_len) idx =
  let word = "XMAS" in
  if idx = String.length word then true
  else
    let current_xmas = String.get "XMAS" idx in
    match char_getter grid row col (row_len, col_len) with
    | Some current_char when Char.equal current_char current_xmas ->
        check_if_word_exists_in_direction grid
          (row + drow, col + dcol)
          (drow, dcol) (row_len, col_len) (idx + 1)
    | _ -> false

let match_char grid row col (row_len, col_len) letter =
  match char_getter grid row col (row_len, col_len) with
  | Some current_char when Char.equal current_char letter -> true
  | _ -> false

let check_for_mas_around_a grid row col (row_len, col_len) =
  let around =
    List.map
      ~f:(fun (drow, dcol) ->
        match char_getter grid (row + drow) (col + dcol) (row_len, col_len) with
        | Some x -> x
        | None -> ' ')
      square_dirs
  in

  is_ok_true (List.for_all2 around [ 'S'; 'S'; 'M'; 'M' ] ~f:Char.equal)
  || is_ok_true (List.for_all2 around [ 'M'; 'M'; 'S'; 'S' ] ~f:Char.equal)
  || is_ok_true (List.for_all2 around [ 'M'; 'S'; 'S'; 'M' ] ~f:Char.equal)
  || is_ok_true (List.for_all2 around [ 'S'; 'M'; 'M'; 'S' ] ~f:Char.equal)

let check_for_mas grid (row, col) (row_len, col_len) =
  match match_char grid row col (row_len, col_len) 'A' with
  | true -> check_for_mas_around_a grid row col (row_len, col_len)
  | false -> false

let search_for_all_occurrences lst (row_len, col_len) =
  let results = ref 0 in
  for row = 0 to row_len - 1 do
    for col = 0 to col_len - 1 do
      List.iter
        ~f:(fun dir ->
          if
            check_if_word_exists_in_direction lst (row, col) dir
              (row_len, col_len) 0
          then results := !results + 1)
        dirs
    done
  done;
  !results

let search_for_mas_occurrences lst (row_len, col_len) =
  let results = ref 0 in
  for row = 0 to row_len - 1 do
    for col = 0 to col_len - 1 do
      if check_for_mas lst (row, col) (row_len, col_len) then
        results := !results + 1
    done
  done;
  !results

let solve_part_1 (input : string) =
  let grid = Read_input.string_to_lines input in
  let row_len, col_len = rows_cols_length grid in
  search_for_all_occurrences grid (row_len, col_len) |> Int.to_string

let solve_part_2 (input : string) =
  let grid = Read_input.string_to_lines input in
  let row_len, col_len = rows_cols_length grid in
  search_for_mas_occurrences grid (row_len, col_len) |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
