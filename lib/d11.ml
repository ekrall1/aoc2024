open Base
open Read_input

let initialize input =
  let lst = input |> Read_input.string_to_lines in
  String.split ~on:' ' (List.nth_exn lst 0)
  |> List.map ~f:(fun x -> Int.of_string x)

let stone_rules e =
  if e = 0 then [ 1 ]
  else if Int.rem (String.length (Int.to_string e)) 2 = 0 then
    let str_lst = String.to_list (Int.to_string e) in
    let h1, h2 = List.split_n str_lst (List.length str_lst / 2) in
    let first = String.concat (List.map h1 ~f:(fun c -> Char.to_string c)) in
    let second = String.concat (List.map h2 ~f:(fun c -> Char.to_string c)) in
    [ Int.of_string first; Int.of_string second ]
  else [ e * 2024 ]

let split_stones lst max_blinks =
  let rec update hm cur_blink =
    let hlst = Hashtbl.keys hm in
    let new_hm = Hashtbl.create (module Int) in
    List.iter hlst ~f:(fun k ->
        let changed_stones = stone_rules k in
        let current_num =
          match Hashtbl.find hm k with None -> 0 | Some v -> v
        in
        List.iter changed_stones ~f:(fun s ->
            Hashtbl.update new_hm s ~f:(fun x ->
                match x with None -> current_num | Some y -> y + current_num)));

    if cur_blink = max_blinks - 1 then
      List.fold_left (Hashtbl.data new_hm) ~init:0 ~f:(fun x acc -> acc + x)
    else update new_hm (cur_blink + 1)
  in

  let hm = Hashtbl.create (module Int) in
  List.iter lst ~f:(fun e -> Hashtbl.add_exn hm ~key:e ~data:1);
  update hm 0

let solve_part_1 input =
  let lst = initialize input in
  let count_lst = List.map lst ~f:(fun x -> split_stones [ x ] 25) in
  Int.to_string (List.fold_left count_lst ~init:0 ~f:(fun acc x -> acc + x))

let solve_part_2 input =
  let lst = initialize input in
  let count_lst = List.map lst ~f:(fun x -> split_stones [ x ] 75) in
  Int.to_string (List.fold_left count_lst ~init:0 ~f:(fun acc x -> acc + x))

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
