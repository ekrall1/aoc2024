open Base
open Str
open Read_input

type problem_input = Input of int list list

let string_to_lines string_input =
  (* parse input string to a list of strings (lines)*)
  Stdlib.String.split_on_char '\n' string_input

let parse_line_to_parts (line : string) : string list =
  (* parse each line into a list of strings, one for each number*)
  let modified_str =
    global_replace (regexp "\\ \\ \\ ") "," (Stdlib.String.trim line)
  in
  String.split modified_str ~on:','

let parts_to_int (parts : string list) : int list =
  (* convert list of two strings to list of two ints*)
  if List.length parts <> 2 then failwith ("invalid input" ^ String.concat parts)
  else
    [ Int.of_string (List.hd_exn parts); Int.of_string (List.nth_exn parts 1) ]

let make_lists (string_input : string) : problem_input =
  (* form input lists from string input*)
  string_to_lines string_input
  |> List.map ~f:(fun line ->
         let parts = parse_line_to_parts line in
         parts_to_int parts)
  |> fun x -> Input x

let solve_part_1 (Input i : problem_input) =
  (* sort the lists and add up the absolute differences of each pair of numbers *)
  let col1 = List.map ~f:List.hd_exn i |> List.sort ~compare:Int.compare in
  let col2 =
    List.map ~f:(fun lst -> List.nth_exn lst 1) i
    |> List.sort ~compare:Int.compare
  in
  List.fold2_exn col1 col2 ~init:0 ~f:(fun acc a b -> acc + abs (a - b))
  |> Int.to_string

let get_count (num : int) (lst : int list) =
  List.fold_left lst ~init:0 ~f:(fun count a ->
      if a = num then count + 1 else count)

let solve_part_2 (Input i : problem_input) =
  (*similarity score*)
  let col1 = List.map ~f:List.hd_exn i in
  let col2 = List.map ~f:(fun lst -> List.nth_exn lst 1) i in
  List.fold_left col1 ~init:0 ~f:(fun acc a -> acc + (a * get_count a col2))
  |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> make_lists |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> make_lists |> solve_part_2
