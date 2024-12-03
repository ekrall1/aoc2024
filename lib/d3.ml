open Base
open Read_input

let extract_products (input_str : string) =
  (* get products from pattern-matching multiplication strings *)
  let reg_exp = Re.Posix.compile_pat "mul\\(([0-9]+),([0-9]+)\\)" in
  let rec get_matches idx =
    match Re.exec_opt ~pos:idx reg_exp (Stdlib.String.trim input_str) with
    | None -> []
    | Some matching ->
        let product =
          Int.of_string (Re.Group.get matching 1)
          * Int.of_string (Re.Group.get matching 2)
        in
        let stop_pos = Re.Group.stop matching 0 in
        product :: get_matches stop_pos
  in
  get_matches 0

let match_doit (idx : int) (input_str : string) =
  (* match do() or don't() string and return the position along with the value *)
  let doit_reg_exp = Re.Posix.compile_pat "do(n't)?\\(\\)" in
  let doit_idx, doit_str =
    match Re.exec_opt ~pos:idx doit_reg_exp input_str with
    | None -> (-1, "")
    | Some matching_doit ->
        (Re.Group.stop matching_doit 0, Re.Group.get matching_doit 0)
  in
  (doit_idx, doit_str)

let update_doit (doit_str : string) =
  (* helper *)
  if String.equal doit_str "don't()" then false
  else if String.equal doit_str "do()" then true
  else failwith "Invalid regex match was encountered"

let match_and_update_do_dont (doit_idx : int) (mult_start_pos : int)
    (doit_str : string) (doit_current : bool) =
  (* return boolean to indicate enable or disable next multiplcation,
     based on index of do/don't match, index of multiplication string, and current enable/disable setting*)
  match
    doit_idx <= mult_start_pos
    && (not (String.equal doit_str ""))
    && doit_idx > 0
  with
  | true -> update_doit doit_str
  | false -> doit_current

let extract_products_do_dont (input_str : string) =
  (*get products from pattern-matching multiplication strings,
    taking account of do() or don't() instructions *)
  let mult_reg_exp = Re.Posix.compile_pat "mul\\(([0-9]+),([0-9]+)\\)" in
  let rec get_matches (idx : int) (doit : bool) =
    let doit_idx, doit_str = match_doit idx input_str in
    match Re.exec_opt ~pos:idx mult_reg_exp (Stdlib.String.trim input_str) with
    | None -> []
    | Some matching ->
        let start_pos = Re.Group.start matching 0 in
        let stop_pos = Re.Group.stop matching 0 in
        let updated_doit =
          match_and_update_do_dont doit_idx start_pos doit_str doit
        in
        let product =
          Int.of_string (Re.Group.get matching 1)
          * Int.of_string (Re.Group.get matching 2)
          * if updated_doit then 1 else 0
        in
        product :: get_matches stop_pos updated_doit
  in
  get_matches 0 true

let sum_product (products : int list) =
  products |> List.fold_left ~init:0 ~f:(fun acc x -> acc + x)

let solve_part_1 (input : string) =
  input |> extract_products |> sum_product |> Int.to_string

let solve_part_2 (input : string) =
  input |> extract_products_do_dont |> sum_product |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
