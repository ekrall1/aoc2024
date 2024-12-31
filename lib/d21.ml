open Read_input
open Base

module IntPair = struct
  type t = int * int [@@deriving sexp]

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let rec sexp_of_t t = sexp_of_t t
  end)
end

(* hash table with tuple keys *)
module TupleHT = struct
  module T = struct
    type t = int * int [@@deriving sexp]

    let compare (x1, y1) (x2, y2) =
      match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

    let rec sexp_of_t t = sexp_of_t t
  end

  include T
  include Comparable.Make (T)
end

type keypad =
  | Keypad of {
      pad : string array;
      rows : int;
      cols : int;
      map : (char, int * int) Base.Hashtbl.t;
    }

let keypad_map arr =
  let hm = Hashtbl.create (module Char) in

  Array.iteri arr ~f:(fun i s ->
      String.iteri s ~f:(fun j c -> Hashtbl.set hm ~key:c ~data:(i, j)));
  hm

let make_keypad rows : keypad =
  Keypad
    {
      pad = Array.of_list rows;
      rows = List.length rows;
      cols = String.length (List.hd_exn rows);
      map = keypad_map (Array.of_list rows);
    }

let numeric_keypad = make_keypad [ "789"; "456"; "123"; "#0A" ]
let directional_keypad = make_keypad [ "#^A"; "<v>" ]
let tuple_equal (x1, y1) (x2, y2) = Int.equal x1 x2 && Int.equal y1 y2

let create_graph (Keypad kp) invalid =
  let graph = Hashtbl.create (module String) in

  let alist = Hashtbl.to_alist kp.map in

  List.iter alist ~f:(fun (key1, (x1, y1)) ->
      List.iter alist ~f:(fun (key2, (x2, y2)) ->
          let component1 =
            if y1 > y2 then String.make (y1 - y2 |> abs) '<' else ""
          in
          let component2 =
            if x2 > x1 then String.make (x2 - x1 |> abs) 'v' else ""
          in
          let componetn3 =
            if x1 > x2 then String.make (x1 - x2 |> abs) '^' else ""
          in
          let component4 =
            if y2 > y1 then String.make (y2 - y1 |> abs) '>' else ""
          in
          let path =
            String.concat [ component1; component2; componetn3; component4 ]
          in
          let path =
            if tuple_equal invalid (x1, y2) || tuple_equal invalid (x2, y1) then
              String.rev path
            else path
          in

          Hashtbl.set graph
            ~key:(String.of_list [ key1; key2 ])
            ~data:(String.concat [ path; "A" ])));

  graph

let translate_instructions code graph =
  let instruction = ref "" in
  let prev = ref 'A' in
  String.iter code ~f:(fun cur ->
      instruction :=
        !instruction ^ Hashtbl.find_exn graph (String.of_list [ !prev; cur ]);
      prev := cur);
  !instruction

let translate_instructions_p2 code =
  let numeric_graph = create_graph numeric_keypad (3, 0) in
  let dir_graph = create_graph directional_keypad (0, 0) in
  let cache = Hashtbl.create (module String) in

  let rec get_instruction_length code_seq iter ~init =
    match
      Hashtbl.find cache (String.concat [ code_seq; Int.to_string iter ])
    with
    | Some cache_len -> cache_len
    | None ->
        let result =
          if iter = 26 then String.length code_seq
          else
            let graph = if init then numeric_graph else dir_graph in
            let rec loop seq prev_ch total_len =
              match seq with
              | [] -> total_len
              | cur_ch :: rest ->
                  let lookup_key = String.of_char_list [ prev_ch; cur_ch ] in
                  let path = Hashtbl.find_exn graph lookup_key in
                  let len =
                    get_instruction_length path (iter + 1) ~init:false
                  in
                  loop rest cur_ch (total_len + len)
            in
            loop (String.to_list code_seq) 'A' 0
        in
        Hashtbl.set cache
          ~key:(String.concat [ code_seq; Int.to_string iter ])
          ~data:result;
        result
  in

  get_instruction_length code 0 ~init:true

let solve_part_1 codes =
  let numeric_graph = create_graph numeric_keypad (3, 0) in
  let directional_graph = create_graph directional_keypad (0, 0) in

  let rec solver lst acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        let robo_instruction1 = translate_instructions hd numeric_graph in
        let robo_instruction2 =
          translate_instructions robo_instruction1 directional_graph
        in
        let robo_instruction3 =
          translate_instructions robo_instruction2 directional_graph
        in
        let instruction_len = String.length robo_instruction3 in
        let numeric_code =
          Int.of_string (String.chop_suffix_exn hd ~suffix:"A")
        in
        solver tl (acc + (instruction_len * numeric_code))
  in

  solver codes 0 |> Int.to_string

let solve_part_2 codes =
  let rec solver lst acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        let numeric_code =
          Int.of_string (String.chop_suffix_exn hd ~suffix:"A")
        in
        solver tl (acc + (translate_instructions_p2 hd * numeric_code))
  in

  solver codes 0 |> Int.to_string

let part1 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_1

let part2 (file_name : string) : string =
  file_name |> Read_input.read_input_file |> Read_input.string_to_lines
  |> solve_part_2
