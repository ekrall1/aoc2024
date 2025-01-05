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

let get_ones num = Int.rem num 10

let create_idx (a, b, c, d) range =
  a + (b * range) + (c * range * range) + (d * range * range * range)

let get_last_four lst =
  let cur = get_ones (List.nth_exn lst 0) in
  let d0 = get_ones (List.nth_exn lst 4) - get_ones (List.nth_exn lst 3) in
  let d1 = get_ones (List.nth_exn lst 3) - get_ones (List.nth_exn lst 2) in
  let d2 = get_ones (List.nth_exn lst 2) - get_ones (List.nth_exn lst 1) in
  let d3 = get_ones (List.nth_exn lst 1) - cur in

  let idx = create_idx (d0, d1, d2, d3) 19 in

  (idx, cur)

let secret_number_map sn max_iter hm =
  let visited = ref (Set.empty (module Int)) in
  let rec process x i sn_lst =
    if Int.equal max_iter i then hm
    else
      let next_sn = find_secret_number x 1 in
      let next_lst = next_sn :: sn_lst in
      (if i >= 4 then
         let key, kval = get_last_four next_lst in
         if not (Set.mem !visited key) then (
           visited := Set.add !visited key;
           Hashtbl.update hm key ~f:(function
             | None -> kval
             | Some value -> value + kval)));
      process next_sn (i + 1) (next_sn :: sn_lst)
  in

  process sn 0 []

let solve_part_1 input_lst =
  let rec solver lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> solver tl (acc + find_secret_number (Int.of_string hd) 2000)
  in
  solver input_lst 0 |> Int.to_string

let solve_part_2 input_lst =
  let hm = Hashtbl.create (module Int) in

  let rec populate_hm lst diff_hm =
    match lst with
    | [] -> diff_hm
    | hd :: tl ->
        let new_diff_hm = secret_number_map (Int.of_string hd) 2000 diff_hm in
        populate_hm tl new_diff_hm
  in

  let final_hm = populate_hm input_lst hm in
  let data =
    List.sort (Hashtbl.data final_hm) ~compare:(fun x y -> Int.compare y x)
  in
  List.hd_exn data |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_2
