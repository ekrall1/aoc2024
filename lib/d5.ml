open Base
open Read_input

let string_to_lines_ordering_rules string_input =
  (* parse input string to a list of strings (lines) that contain the character '|'*)
  Read_input.string_to_lines string_input
  |> List.filter ~f:(fun x -> String.mem x '|')
  |> List.map ~f:(fun i -> String.split ~on:'|' i)

let string_to_lines_updates string_input =
  (* parse input string to a list of strings (lines) that contain the character ','*)
  List.filter (Stdlib.String.split_on_char '\n' string_input) ~f:(fun x ->
      String.mem x ',')
  |> List.map ~f:(fun i -> String.split ~on:',' i)

let validate_rule (rule : string list) (lst : string list) =
  let head = match rule with [] -> "" | hd :: _ -> hd in

  let idx1 =
    match List.findi lst ~f:(fun _ x -> String.equal x head) with
    | Some (idx, _) -> idx
    | None -> -1
  in

  if idx1 < 0 then true
  else
    let tail = match List.rev rule with [] -> "" | hd :: _ -> hd in

    let idx2 =
      match List.findi lst ~f:(fun _ x -> String.equal x tail) with
      | Some (idx, _) -> idx
      | None -> -1
    in

    idx2 = -1 || idx1 < idx2

let check_rules (rules : string list list) (update : string list) =
  List.map rules ~f:(fun rule -> validate_rule rule update)

let iterate_updates_and_check_rules (updates : string list list)
    (rules : string list list) =
  List.map
    (List.map updates ~f:(fun update -> check_rules rules update))
    ~f:(fun lst -> List.for_all lst ~f:(fun i -> i))

let get_mid (lst : string list) =
  let len = List.length lst in
  let mid_idx = len / 2 in
  (* if len % 2 = 0 then List.nth_exn lst (mid_idx - 1) *)
  List.nth_exn lst mid_idx

let middle_nums (lst : string list list) =
  List.map lst ~f:(fun x -> Int.of_string (get_mid x))

let solve_part_1 (input : string) =
  let updates = string_to_lines_updates input in
  let rules = string_to_lines_ordering_rules input in
  let update_mask = iterate_updates_and_check_rules updates rules in
  let good_updates =
    match List.zip updates update_mask with
    | Ok zipped ->
        List.filter_map zipped ~f:(fun (value, mask) ->
            if mask then Some value else None)
    | Unequal_lengths -> failwith "lists are not of equal length"
  in
  List.fold_left (middle_nums good_updates) ~init:0 ~f:(fun acc i -> acc + i)
  |> Int.to_string

let is_correct_order set rules =
  List.for_all rules ~f:(fun rule ->
      match rule with
      | [ first; second ] -> (
          match
            ( List.findi set ~f:(fun _ x -> String.equal x first),
              List.findi set ~f:(fun _ x -> String.equal x second) )
          with
          | Some (i1, _), Some (i2, _) -> i1 < i2
          | _ -> true)
      | _ -> true)

let order set rules =
  let rec loop set =
    if is_correct_order set rules then set
    else
      let applicable =
        List.filter rules ~f:(fun rule ->
            match rule with
            | [ first; second ] ->
                List.mem set ~equal:String.equal first
                && List.mem set ~equal:String.equal second
            | _ -> false)
      in
      let set' =
        List.fold_left applicable ~init:set ~f:(fun set rule ->
            match rule with
            | [ first; second ] -> (
                match
                  ( List.findi set ~f:(fun _ x -> String.equal x first),
                    List.findi set ~f:(fun _ x -> String.equal x second) )
                with
                | Some (i1, _), Some (i2, _) when i1 > i2 ->
                    (* Swap the positions of the elements at i1 and i2 *)
                    List.mapi set ~f:(fun idx x ->
                        if idx = i1 then Option.value_exn (List.nth set i2)
                        else if idx = i2 then Option.value_exn (List.nth set i1)
                        else x)
                | _ -> set)
            | _ -> set)
      in
      loop set'
  in
  loop set

let solve_part_2 (input : string) =
  let updates = string_to_lines_updates input in
  let rules = string_to_lines_ordering_rules input in
  let update_mask = iterate_updates_and_check_rules updates rules in
  let bad_updates =
    match List.zip updates update_mask with
    | Ok zipped ->
        List.filter_map zipped ~f:(fun (value, mask) ->
            if not mask then Some value else None)
    | Unequal_lengths -> failwith "lists are not of equal length"
  in
  List.fold_left
    (middle_nums (List.map bad_updates ~f:(fun update -> order update rules)))
    ~init:0
    ~f:(fun acc i -> acc + i)
  |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> solve_part_2
