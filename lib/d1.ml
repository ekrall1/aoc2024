open Base
open Str

type problem_input = Input of int list list

let read_input_file (file_name : string) : string =
  (* read the input file and return a single string *)
  let input_channel = Stdlib.open_in file_name in
  let string_input =
    Stdlib.really_input_string input_channel
      (Stdlib.in_channel_length input_channel)
  in
  Stdlib.close_in input_channel;
  string_input

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
    [
      Int.of_string (Stdlib.List.nth parts 0);
      Int.of_string (Stdlib.List.nth parts 1);
    ]

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

let part1 (file_name : string) : string =
  file_name |> read_input_file |> make_lists |> solve_part_1