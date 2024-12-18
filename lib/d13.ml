open Z3
open Read_input
open Base

(***
  find smallest number of tokens to move the claw machine by the desired amount
  a_cost - tokens per a button press
  b_cost - tokens per b button press
  a_x_coeff, a_y_coeff - movement in x and y direction per press of a
  b_x_coeff, b_y_coeff - movement in x and y direction per press of b

  solve using mixed integer programming formulation in z3

  Returns
  (tokens spent, a presses, b presses)
  ***)
let solver a_cost b_cost
    (((a_x_coeff, a_y_coeff), (b_x_coeff, b_y_coeff)), (x_rhs, y_rhs)) part =
  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let ctx = mk_context cfg in

  let opt = Optimize.mk_opt ctx in

  let a = Arithmetic.Integer.mk_const_s ctx "a" in
  let b = Arithmetic.Integer.mk_const_s ctx "b" in
  let x = Arithmetic.Real.mk_const_s ctx "x" in
  let y = Arithmetic.Real.mk_const_s ctx "y" in

  let obj =
    Arithmetic.mk_add ctx
      [
        Arithmetic.mk_mul ctx [ Arithmetic.Real.mk_numeral_i ctx a_cost; a ];
        Arithmetic.mk_mul ctx [ Arithmetic.Real.mk_numeral_i ctx b_cost; b ];
      ]
  in

  let c1 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_add ctx [ a ])
      (Arithmetic.Real.mk_numeral_i ctx 100)
  in
  let c2 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_add ctx [ b ])
      (Arithmetic.Real.mk_numeral_i ctx 100)
  in
  let c3 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx
         [
           x;
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx a_x_coeff; a ];
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx b_x_coeff; b ];
         ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in
  let c4 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx
         [
           y;
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx a_y_coeff; a ];
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx b_y_coeff; b ];
         ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in
  let c5 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx
         [
           x;
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx a_x_coeff; a ];
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx b_x_coeff; b ];
         ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in
  let c6 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx
         [
           y;
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx a_y_coeff; a ];
           Arithmetic.mk_mul ctx
             [ Arithmetic.Real.mk_numeral_i ctx b_y_coeff; b ];
         ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in
  let c7 = Arithmetic.mk_le ctx x (Arithmetic.Real.mk_numeral_i ctx x_rhs) in
  let c8 = Arithmetic.mk_le ctx y (Arithmetic.Real.mk_numeral_i ctx y_rhs) in
  let c9 = Arithmetic.mk_ge ctx x (Arithmetic.Real.mk_numeral_i ctx x_rhs) in
  let c10 = Arithmetic.mk_ge ctx y (Arithmetic.Real.mk_numeral_i ctx y_rhs) in

  if part = 1 then Optimize.add opt [ c1; c2; c3; c4; c5; c6; c7; c8; c9; c10 ]
  else Optimize.add opt [ c3; c4; c5; c6; c7; c8; c9; c10 ];

  let _ = Optimize.minimize opt obj in

  (* optimal values *)
  let z_opt = ref "" in

  (* Check if the problem is feasible/optimal and extract the soln *)
  match Optimize.check opt with
  | Solver.SATISFIABLE -> (
      match Optimize.get_model opt with
      | Some model ->
          let opt_val = Model.eval model obj true in
          let opt_val =
            match opt_val with
            | Some e -> e
            | None -> failwith "could not evaluate the objective value."
          in
          let simplified_opt = Expr.simplify opt_val None in
          z_opt := Expr.to_string simplified_opt;
          !z_opt
      | None ->
          failwith "Error: Model not available despite satisfiable result.\n")
  | _ -> "0"
(* it didn't solve, so there was no way to move the claw to the location *)

let parse_part line discriminator puzzle_part =
  let sep = ref ' ' in
  if Stdlib.String.equal discriminator "prize" then sep := '='
  else if Stdlib.String.equal discriminator "button" then sep := '+';

  let parts = Stdlib.String.split_on_char ',' line in
  let extract_value part =
    let idx = Stdlib.String.index part !sep in
    Int.of_string
      (Stdlib.String.sub part (idx + 1) (String.length part - idx - 1))
  in
  if puzzle_part = 2 && Stdlib.String.equal discriminator "prize" then
    ( extract_value (Stdlib.List.nth parts 0) + 10000000000000,
      extract_value (Stdlib.List.nth parts 1) + 10000000000000 )
  else
    ( extract_value (Stdlib.List.nth parts 0),
      extract_value (Stdlib.List.nth parts 1) )

(* in the input file each machine's info is in 3 lines*)
let parse_block lines part =
  match lines with
  | [ button_a; button_b; prize ] ->
      let button_a_coeffs = parse_part button_a "button" part in
      let button_b_coeffs = parse_part button_b "button" part in
      let prize_coords = parse_part prize "prize" part in
      ((button_a_coeffs, button_b_coeffs), prize_coords)
  | _ -> failwith "hahaha"

let input_parser input_lines part =
  let rec gather_machine_input acc current_machine rem =
    match rem with
    | [] ->
        if Stdlib.List.length current_machine = 0 then Stdlib.List.rev acc
        else List.rev (parse_block (Stdlib.List.rev current_machine) part :: acc)
    | hd :: tl ->
        if Stdlib.String.starts_with ~prefix:"Button A:" hd then
          (* start the next machine info *)
          let acc =
            if Stdlib.List.length current_machine = 0 then acc
            else parse_block (Stdlib.List.rev current_machine) part :: acc
          in
          gather_machine_input acc [ hd ] tl
        else
          (* next line of current machine *)
          gather_machine_input acc (hd :: current_machine) tl
  in
  gather_machine_input [] [] input_lines

let solve_it input part =
  let rec solve_all lst tokens =
    match lst with
    | [] -> tokens
    | hd :: tl ->
        solve_all tl tokens
        + Int.of_float (Float.of_string (solver 3 1 hd part))
  in
  solve_all input 0 |> Base.Int.to_string

let part1 (file_name : string) : string =
  let unparsed =
    file_name |> Read_input.read_input_file
    |> Stdlib.String.split_on_char '\n'
    |> Stdlib.List.filter (fun line ->
           not (Stdlib.String.equal (Stdlib.String.trim line) ""))
  in
  solve_it (input_parser unparsed 1) 1

let part2 (file_name : string) : string =
  let unparsed =
    file_name |> Read_input.read_input_file
    |> Stdlib.String.split_on_char '\n'
    |> Stdlib.List.filter (fun line ->
           not (Stdlib.String.equal (Stdlib.String.trim line) ""))
  in
  solve_it (input_parser unparsed 2) 2
