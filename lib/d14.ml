open Read_input
open Base

(**given position and velocity (p and v), along with grid width and height (w and h), find the quadrant after s time steps
points in the middle row or column are not in a quadrant
*)

let find_coord len start v s =
  match v < 0 with
  | true -> Int.rem (len + Int.rem (start + (s * v)) len) len
  | false -> Int.rem (start + (s * v)) len

let quadrant w h pos v s =
  let final_pos =
    (find_coord w (fst pos) (fst v) s, find_coord h (snd pos) (snd v) s)
  in
  let mid = (w / 2, h / 2) in
  let q =
    if fst final_pos = fst mid || snd final_pos = snd mid then 5
    else
      match (fst final_pos < fst mid, snd final_pos < snd mid) with
      | true, true -> 1
      | false, true -> 2
      | true, false -> 3
      | false, false -> 4
  in
  (q, final_pos)

(** input processing *)
let get_tuple input_str =
  match String.split ~on:',' input_str with
  | [] -> failwith "input error - empty row"
  | hd :: tl ->
      let last = List.hd_exn (List.rev tl) in
      let e1 =
        Int.of_string (List.hd_exn (List.rev (String.split ~on:'=' hd)))
      in

      let e2 = Int.of_string last in
      (e1, e2)

let parse_pos_and_velocity lst =
  List.map lst ~f:(fun elem ->
      let inner = String.split ~on:' ' elem in
      (get_tuple (List.hd_exn inner), get_tuple (List.nth_exn inner 1)))

let map_input_tuples_to_quadrants lst width height s =
  let hm = Hashtbl.create (module Int) in
  List.iter lst ~f:(fun elem ->
      let pos = fst elem in
      let v = snd elem in
      let q = quadrant width height pos v s in
      Hashtbl.update hm (fst q) ~f:(function None -> 1 | Some x -> x + 1));
  hm

let map_input_tuples_to_coords lst width height s =
  let hm = Hashtbl.create (module Int) in
  List.iter lst ~f:(fun elem ->
      let pos = fst elem in
      let v = snd elem in
      let q = quadrant width height pos v s in
      Hashtbl.update hm (fst q) ~f:(function
        | None -> [ snd q ]
        | Some x -> x @ [ snd q ]));
  hm

(** solution *)
let extract_quadrant_count q hm = Hashtbl.find hm q |> Option.value ~default:0

let quadrants = [ 1; 2; 3; 4 ]

let solve_part_1 lst width height s =
  let hm = map_input_tuples_to_quadrants lst width height s in
  let counts =
    List.map quadrants ~f:(fun quad -> extract_quadrant_count quad hm)
  in
  List.fold_left counts ~init:1 ~f:(fun acc x -> acc * x) |> Int.to_string

let solve_part_2 lst width height =
  let rec look_for_pattern acc =
    let hm = map_input_tuples_to_coords lst width height acc in
    let center = Hashtbl.find hm 5 |> Option.value ~default:[] in
    let q1 = Hashtbl.find hm 1 |> Option.value ~default:[] in
    let q2 = Hashtbl.find hm 2 |> Option.value ~default:[] in
    let q3 = Hashtbl.find hm 3 |> Option.value ~default:[] in
    let q4 = Hashtbl.find hm 4 |> Option.value ~default:[] in
    let all_quads = q1 @ q2 @ q3 @ q4 @ center in

    let rec check_surrounding tmp =
      match tmp with
      | hd :: tl ->
          if
            List.exists all_quads ~f:(fun x ->
                fst x = fst hd - 1 && snd x = snd hd)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd + 1 && snd x = snd hd)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd && snd x = snd hd - 1)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd && snd x = snd hd + 1)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd - 1 && snd x = snd hd - 1)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd - 1 && snd x = snd hd + 1)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd + 1 && snd x = snd hd - 1)
            && List.exists all_quads ~f:(fun x ->
                   fst x = fst hd + 1 && snd x = snd hd + 1)
          then true
          else check_surrounding tl
      | [] -> false
    in

    if check_surrounding all_quads then acc else look_for_pattern (acc + 1)
  in

  look_for_pattern 1 |> Int.to_string

let part1 (file_name : string) =
  (* test data and actual data have different grids, and these are exogenously defined*)
  let width, height =
    match String.is_substring file_name ~substring:"d14.txt" with
    | true -> (101, 103)
    | false -> (11, 7)
  in

  let input = file_name |> Read_input.read_input_file in
  let input_lst = input |> Read_input.string_to_lines in
  let tuple_lst = parse_pos_and_velocity input_lst in

  solve_part_1 tuple_lst width height 100

let part2 (file_name : string) =
  if String.is_substring file_name ~substring:"test_data" then
    "Cannot run part 2 on test data, use the day 14 aoc puzzle input"
  else
    (* test data and actual data have different grids, and these are exogenously defined
       part 2, finding where the robots form a christmas tree, only runs on the actual data
       the test data doesn't form the same pattern the way we are checking for it
    *)
    let width, height =
      match String.is_substring file_name ~substring:"d14.txt" with
      | true -> (101, 103)
      | false -> failwith "invalid input file specification"
    in

    let input = file_name |> Read_input.read_input_file in
    let input_lst = input |> Read_input.string_to_lines in
    let tuple_lst = parse_pos_and_velocity input_lst in

    solve_part_2 tuple_lst width height
