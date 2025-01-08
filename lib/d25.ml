open Read_input
open Base

let match_full_row kstr = String.for_all kstr ~f:(fun c -> Char.equal c '#')

let update_hashtbl hm key elem skip =
  match skip with
  | true -> ()
  | false ->
      let lst = Hashtbl.find hm key |> Option.value ~default:[] in
      Hashtbl.set hm ~key ~data:(lst @ [ elem ])

let make_input_map input_lst =
  let ht_key = Hashtbl.create (module Int) in
  let ht_lock = Hashtbl.create (module Int) in
  List.iteri input_lst ~f:(fun idx l ->
      let len = List.length l - 1 in
      match match_full_row (List.hd_exn l) with
      | true ->
          List.iteri l ~f:(fun i x -> update_hashtbl ht_lock idx x (i = 0))
      | false ->
          List.iteri l ~f:(fun i x -> update_hashtbl ht_key idx x (i = len)));
  (ht_key, ht_lock)

let get_transpose_arr lst =
  let arr = Array.of_list (List.map lst ~f:(fun x -> String.to_array x)) in
  Array.transpose_exn arr

let get_heights arr =
  Array.map arr ~f:(fun x ->
      Array.fold x ~init:0 ~f:(fun acc e ->
          if Char.equal e '#' then acc + 1 else acc))

let compare_heights lst1 lst2 len =
  let arr1, arr2 =
    (get_heights (get_transpose_arr lst1), get_heights (get_transpose_arr lst2))
  in
  let valid = ref true in
  Array.iter2_exn arr1 arr2 ~f:(fun a b -> if a + b > len then valid := false);
  !valid

let solve_part_1 (keys_hm, locks_hm) =
  let keys_lst = Hashtbl.data keys_hm in
  let lock_lst = Hashtbl.data locks_hm in
  let rec compare_keys_and_locks cur acc =
    match cur with
    | hd :: tl ->
        let new_acc = ref acc in
        List.iter lock_lst ~f:(fun v ->
            let unlock = compare_heights hd v (List.length hd - 1) in
            new_acc := if unlock then !new_acc + 1 else !new_acc);
        compare_keys_and_locks tl !new_acc
    | [] -> acc
  in
  compare_keys_and_locks keys_lst 0 |> Int.to_string

let part1 (file_name : string) =
  let input_lst =
    file_name |> Read_input.read_input_file
    |> Str.split (Str.regexp "\n\n")
    |> List.map ~f:(fun x -> Read_input.string_to_lines x)
  in
  input_lst |> make_input_map |> solve_part_1

let part2 (_ : string) = Printf.sprintf "the end"
