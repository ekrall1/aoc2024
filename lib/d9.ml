open Base
open Read_input

let get_array_indices fsys files free =
  let rec loop_fsys fileindices freeindices arr =
    let curfile, restfile =
      match fileindices with
      | hd :: tl -> (hd, tl)
      | _ -> failwith "index error"
    in

    let curfree, restfree =
      match freeindices with
      | hd :: tl -> (hd, tl)
      | _ -> failwith "index error"
    in

    let stop_pos = curfile <= curfree in
    match stop_pos with
    | true -> arr
    | false ->
        let tmp = arr.(curfile) in
        arr.(curfile) <- arr.(curfree);
        arr.(curfree) <- tmp;
        loop_fsys restfile restfree arr
  in

  loop_fsys (List.rev files) free fsys

let get_array_indices_p2 args =
  let fsys, files, free = args in
  let fs = Array.copy fsys in
  let files_arr = Array.of_list files in
  let free_arr = Array.of_list free in

  let rec find_and_move file file_len idx =
    if idx >= Array.length free_arr then ()
    else
      let fr, free_len = free_arr.(idx) in
      if file_len <= free_len && file > fr then (
        for j = 0 to file_len - 1 do
          let tmp = fs.(fr + j) in
          fs.(fr + j) <- fs.(file + j);
          fs.(file + j) <- tmp
        done;

        free_arr.(idx) <- (fr + file_len, free_len - file_len))
      else find_and_move file file_len (idx + 1)
  in

  for file_idx = Array.length files_arr - 1 downto 0 do
    let file, file_len = files_arr.(file_idx) in
    find_and_move file file_len 0
  done;

  fs

let make_initial_list diskmap =
  let fm = ref [] in
  let files = ref [] in
  let free = ref [] in
  let cur = ref 0 in
  Array.iteri diskmap ~f:(fun idx x ->
      let x = Int.of_string (String.of_char x) in
      match idx % 2 with
      | 0 ->
          files := !files @ List.init x ~f:(fun i -> List.length !fm + i);
          fm := !fm @ List.init x ~f:(fun _ -> !cur);
          cur := !cur + 1
      | _ ->
          free := !free @ List.init x ~f:(fun i -> List.length !fm + i);
          fm := !fm @ List.init x ~f:(fun _ -> -1));
  (Array.of_list !fm, !files, !free)

let make_initial_list_p2 diskmap =
  let fm = ref [] in
  let files = ref [] in
  let free = ref [] in
  let cur = ref 0 in
  Array.iteri diskmap ~f:(fun idx x ->
      let x = Int.of_string (String.of_char x) in
      match idx % 2 with
      | 0 ->
          files := !files @ [ (List.length !fm, x) ];
          fm := !fm @ List.init x ~f:(fun _ -> !cur);
          cur := !cur + 1
      | _ ->
          free := !free @ [ (List.length !fm, x) ];
          fm := !fm @ List.init x ~f:(fun _ -> -1));
  (Array.of_list !fm, !files, !free)

let solve_part_1 input =
  let fsys, files, free =
    input |> Read_input.string_to_lines |> List.hd_exn |> String.to_list
    |> Array.of_list |> make_initial_list
  in
  let compressed = List.of_array (get_array_indices fsys files free) in
  compressed
  |> List.mapi ~f:(fun idx x -> (idx, x))
  |> List.filter ~f:(fun (_, x) -> x <> -1)
  |> List.fold_left ~init:0 ~f:(fun acc (idx, x) -> acc + (idx * x))
  |> Int.to_string

let solve_part_2 input =
  let fsys, files, free =
    input |> Read_input.string_to_lines |> List.hd_exn |> String.to_list
    |> Array.of_list |> make_initial_list_p2
  in
  get_array_indices_p2 (fsys, files, free)
  |> List.of_array
  |> List.mapi ~f:(fun idx x -> (idx, x))
  |> List.filter ~f:(fun (_, x) -> x <> -1)
  |> List.fold_left ~init:0 ~f:(fun acc (idx, x) -> acc + (idx * x))
  |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
