open Read_input
open Base

(* Linen layout
   part 1 - find how many patterns are possible from combos of given towels
*)

type input = Input of { towels : string list; patterns : string list }

let make_input input =
  let lsts = input |> Read_input.string_to_lines in

  let towels = String.split ~on:',' (List.hd_exn lsts) in
  let towels_strp = List.map towels ~f:(fun x -> String.strip x) in
  let patterns =
    List.filter_mapi lsts ~f:(fun i x -> if i > 1 then Some x else None)
  in

  Input { towels = towels_strp; patterns }

let solve_part_1 set_of_towels set_of_patterns =
  let memo = Hashtbl.create (module String) in

  let rec matching towels pattern =
    match Hashtbl.find memo pattern with
    | Some result -> result
    | None ->
        let result =
          if String.is_empty pattern then true
          else
            Set.exists towels ~f:(fun t ->
                String.is_prefix pattern ~prefix:t
                && matching towels
                     (String.drop_prefix pattern (String.length t)))
        in
        Hashtbl.set memo ~key:pattern ~data:result;
        result
  in

  let get_count =
    Set.count set_of_patterns ~f:(fun p -> matching set_of_towels p)
  in

  get_count |> Int.to_string

let solve_part_2 set_of_towels set_of_patterns =
  let memo = Hashtbl.create (module String) in

  let rec matching towels pattern =
    match Hashtbl.find memo pattern with
    | Some result -> result
    | None ->
        let result =
          if String.is_empty pattern then 1
          else
            let count = ref 0 in
            Set.iter towels ~f:(fun t ->
                if String.is_prefix pattern ~prefix:t then
                  count :=
                    !count
                    + matching towels
                        (String.drop_prefix pattern (String.length t)));
            !count
        in
        Hashtbl.set memo ~key:pattern ~data:result;
        result
  in

  let get_count =
    Set.fold_right set_of_patterns ~init:0 ~f:(fun p acc ->
        acc + matching set_of_towels p)
  in

  get_count |> Int.to_string

let part1 (file_name : string) : string =
  let input = file_name |> Read_input.read_input_file |> make_input in
  let towels, patterns =
    match input with
    | Input { towels; patterns } ->
        ( Set.of_list (module String) towels,
          Set.of_list (module String) patterns )
  in
  solve_part_1 towels patterns

let part2 (file_name : string) : string =
  let input = file_name |> Read_input.read_input_file |> make_input in
  let towels, patterns =
    match input with
    | Input { towels; patterns } ->
        ( Set.of_list (module String) towels,
          Set.of_list (module String) patterns )
  in
  solve_part_2 towels patterns
