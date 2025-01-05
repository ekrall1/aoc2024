open Read_input
open Base

let find_secret_number sn max_iter =
  let rec process x i =
    if Int.equal i max_iter then x
    else
      let x =
        x lxor (x lsl 6) land 0xFFFFFF |> fun y ->
        y lxor (y asr 5) land 0xFFFFFF |> fun z ->
        z lxor (z lsl 11) land 0xFFFFFF
      in
      process x (i + 1)
  in
  process sn 0

let solve_part_1 input_lst =
  let rec solver lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> solver tl (acc + find_secret_number (Int.of_string hd) 2000)
  in
  solver input_lst 0 |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_1

let part2 (file_name : string) : string =
  Printf.sprintf "cannot run part 2 for %s; not implemented" file_name
