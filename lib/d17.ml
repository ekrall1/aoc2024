open Read_input
open Base

(** Chronospatial Computer **)

type registers = Registers of { a : int; b : int; c : int }
type input = Input of { reg : registers; prg : int array }

let parse_register_val lines n =
  n |> List.nth_exn lines |> String.split ~on:':' |> List.rev |> List.hd_exn
  |> String.strip |> Int.of_string

let parse_program lines n =
  n |> List.nth_exn lines |> String.split ~on:':' |> List.rev |> List.hd_exn
  |> String.strip |> String.split ~on:','
  |> List.map ~f:(fun x -> Int.of_string x)
  |> List.to_array

let parse_input input =
  let lines = Read_input.string_to_lines input in
  let reg_init_a = parse_register_val lines 0 in
  let reg_init_b = parse_register_val lines 1 in
  let reg_init_c = parse_register_val lines 2 in
  let program = parse_program lines 4 in

  Input
    {
      reg = Registers { a = reg_init_a; b = reg_init_b; c = reg_init_c };
      prg = program;
    }

let adv operand reg =
  (*The adv instruction (opcode 0) performs division.
    The numerator is the value in the A register.
    The denominator is found by raising 2 to the power of the instruction's combo operand.
    The result of the division operation is truncated to an integer and then written to the A register.*)
  match reg with
  | Registers { a = reg_a; b = reg_b; c = reg_c } ->
      Registers { a = reg_a / Int.pow 2 operand; b = reg_b; c = reg_c }

let bxl operand reg =
  (*The bxl instruction (opcode 1) calculates the bitwise XOR of register B,
    and the instruction's literal operand, then stores the result in register B*)
  match reg with
  | Registers { a = reg_a; b = reg_b; c = reg_c } ->
      Registers { a = reg_a; b = Int.bit_xor reg_b operand; c = reg_c }

let bst operand reg =
  (*The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits),
    then writes that value to the B register.*)
  match reg with
  | Registers { a = reg_a; b = _; c = reg_c } ->
      Registers { a = reg_a; b = Int.rem operand 8; c = reg_c }

let jnz (operand : int) reg =
  (*The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero,
    it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps,
    the instruction pointer is not increased by 2 after this instruction.*)
  let reg_a = match reg with Registers { a = reg_a; b = _; c = _ } -> reg_a in

  if Int.equal reg_a 0 then None else Some operand

let bxc operand reg =
  (*The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
    then stores the result in register B.*)
  Stdlib.Printf.printf "Ignoring operand %d in bxc.\n" operand;
  match reg with
  | Registers { a = reg_a; b = reg_b; c = reg_c } ->
      Registers { a = reg_a; b = Int.bit_xor reg_b reg_c; c = reg_c }

let out operand =
  (*The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
    then outputs that value*)
  String.concat [ ","; Int.to_string (Int.rem operand 8) ]

let bdv operand reg =
  (*The bdv instruction (opcode 6) performs division.
    The numerator is the value in the A register.
    The denominator is found by raising 2 to the power of the instruction's combo operand.
    The result of the division operation is truncated to an integer and then written to the B register.*)
  match reg with
  | Registers { a = reg_a; b = _; c = reg_c } ->
      Registers { a = reg_a; b = reg_a / Int.pow 2 operand; c = reg_c }

let cdv operand reg =
  (*The cdv instruction (opcode 7) performs division.
    The numerator is the value in the A register.
    The denominator is found by raising 2 to the power of the instruction's combo operand.
    The result of the division operation is truncated to an integer and then written to the C register.*)
  match reg with
  | Registers { a = reg_a; b = reg_b; c = _ } ->
      Registers { a = reg_a; b = reg_b; c = reg_a / Int.pow 2 operand }

let get_combo_operand operand reg =
  let reg_a, reg_b, reg_c =
    match reg with
    | Registers { a = reg_a; b = reg_b; c = reg_c } -> (reg_a, reg_b, reg_c)
  in

  match operand with
  | 0 | 1 | 2 | 3 -> operand
  | 4 -> reg_a
  | 5 -> reg_b
  | 6 -> reg_c
  | 7 -> operand
  | _ -> failwith "invalid operand"

let solve_part_1 input =
  let data = parse_input input in

  let reg, prg = match data with Input { reg; prg } -> (reg, prg) in

  let reg_ref = ref reg in
  let reg_prg = ref prg in

  let len = Array.length prg in

  let rec runit cmds ptr acc =
    if !ptr >= len then acc
    else
      match (cmds.(!ptr), cmds.(!ptr + 1)) with
      | 0, n ->
          reg_ref := adv (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | 1, n ->
          reg_ref := bxl (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | 2, n ->
          reg_ref := bst (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | 3, n ->
          let jmp = jnz (get_combo_operand n !reg_ref) !reg_ref in
          (match jmp with None -> ptr := !ptr + 2 | Some _ -> ptr := n);
          runit cmds ptr acc
      | 4, n ->
          reg_ref := bxc (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | 5, n ->
          let output = out (get_combo_operand n !reg_ref) in
          ptr := !ptr + 2;
          runit cmds ptr (String.concat [ acc; output ])
      | 6, n ->
          reg_ref := bdv (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | 7, n ->
          reg_ref := cdv (get_combo_operand n !reg_ref) !reg_ref;
          ptr := !ptr + 2;
          runit cmds ptr acc
      | _ -> failwith "invalid instruction"
  in

  let final_reg, int_out = (!reg_ref, runit !reg_prg (ref 0) "") in
  match String.chop_prefix int_out ~prefix:(String.prefix int_out 1) with
  | Some final_out -> (final_reg, final_out)
  | None -> (final_reg, int_out)

let solve_part_2 input = input

let part1_register (file_name : string) : registers =
  let res = file_name |> Read_input.read_input_file |> solve_part_1 in
  fst res

let part1 (file_name : string) : string =
  let res = file_name |> Read_input.read_input_file |> solve_part_1 in
  snd res

let part2 (file_name : string) =
  file_name |> Read_input.read_input_file |> solve_part_2
