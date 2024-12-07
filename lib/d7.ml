open Base
open Read_input
open Str

let string_to_lines string_input =
  (* parse input string to a list of strings (lines)*)
  Stdlib.String.split_on_char '\n' string_input

let apply_operator op a b =
  match op with
  | '+' -> a + b
  | '*' -> a * b
  | '|' ->
      Int.of_string (String.concat ~sep:"" [ Int.to_string a; Int.to_string b ])
  | _ -> failwith "Unknown operator"

let get_lhs_rhs input_line =
  let modified_str =
    global_replace (regexp ":\\ \\|\\ ") "," (Stdlib.String.trim input_line)
  in
  let parsed = String.split modified_str ~on:',' in
  match parsed with
  | hd :: tl -> (Int.of_string hd, List.map tl ~f:(fun i -> Int.of_string i))
  | [] -> failwith "no input found on this line"

let rec get_combinations n operators =
  if n = 0 then Sequence.singleton []
  else
    Sequence.bind (Sequence.of_list operators) ~f:(fun op ->
        Sequence.map
          ~f:(fun rest -> op :: rest)
          (get_combinations (n - 1) operators))

let evaluate_with_operators lhs operators =
  let rec operate_on_lhs acc lhs operators =
    match (lhs, operators) with
    | [], [] ->
        acc
    | _ :: [], [] -> acc
    | x :: xs, op :: ops ->
        let new_acc = apply_operator op acc x in
        operate_on_lhs new_acc xs ops
    | _ -> failwith "Mismatch in lhs and operators"
  in
  match lhs with
  | [] -> failwith "lhs cannot be empty"
  | x :: xs -> operate_on_lhs x xs operators

let try_operators (vals : int * int list) (operators : char list) =
  let lhs = snd vals in
  let rhs = fst vals in

  let num_combinations = List.length lhs - 1 in
  let found =
    Sequence.find (get_combinations num_combinations operators) ~f:(fun combo ->
        evaluate_with_operators lhs combo = rhs)
  in
  Option.map found ~f:(fun _ -> rhs)

let solve_part_with_operators input operators =
  let lines = string_to_lines input in
  let answers =
    List.map lines ~f:(fun x -> try_operators (get_lhs_rhs x) operators)
  in
  List.fold answers ~init:0 ~f:(fun acc x ->
      match x with Some i -> acc + i | None -> acc)
  |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> fun input ->
  solve_part_with_operators input [ '+'; '*' ]

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> fun input ->
  solve_part_with_operators input [ '+'; '*'; '|' ]
