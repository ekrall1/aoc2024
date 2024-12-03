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
    global_replace (regexp "\\ ") "," (Stdlib.String.trim line)
  in
  String.split modified_str ~on:','

let make_reports (string_input : string) : problem_input =
  (* form report sequences from string input*)
  string_to_lines string_input
  |> List.map ~f:(fun line ->
         let parts = parse_line_to_parts line in
         List.map ~f:Int.of_string parts)
  |> fun x -> Input x

let valid_inc (lst : int list) =
  (* strictly increasing and max increase of 3 *)
  match lst with
  | [] | [ _ ] -> true
  | hd :: tl ->
      List.fold tl ~init:(Some hd) ~f:(fun acc x ->
          match acc with
          | None -> None
          | Some prev when x - prev > 0 && x - prev < 4 -> Some x
          | _ -> None)
      |> Option.is_some

let valid_dec (lst : int list) =
  (* strictly decreasing and max decrease of 3 *)
  match lst with
  | [] | [ _ ] -> true
  | hd :: tl ->
      List.fold tl ~init:(Some hd) ~f:(fun acc x ->
          match acc with
          | None -> None
          | Some prev when x - prev < 0 && x - prev > -4 -> Some x
          | _ -> None)
      |> Option.is_some

let strictly_inc_or_dec (lst : int list) = valid_inc lst || valid_dec lst

let solve_part_1 (Input i : problem_input) =
  let mapped_bools = List.map ~f:strictly_inc_or_dec i in
  List.fold_left ~init:0
    ~f:(fun count x -> if x then count + 1 else count)
    mapped_bools
  |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> read_input_file |> make_reports |> solve_part_1

let part2 (file_name : string) : string = file_name |> read_input_file
