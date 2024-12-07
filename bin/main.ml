open Stdio

let day_of_advent = ref 0
let part = ref 0
let input_file = ref ""
let anon_fun filename = input_file := filename

let arglist =
  [
    ("-day-of-advent", Arg.Set_int day_of_advent, "day number");
    ("-part", Arg.Set_int part, "part");
    ("-input", Arg.Set_string input_file, "input file");
  ]

let usage_msg =
  {|aoc2024 -day-of-advent <number> -part <number> -input <filename>|}

let () =
  let () = Arg.parse arglist anon_fun usage_msg in
  let part1_fn, part2_fn =
    match !day_of_advent with
    | 1 -> (Days.D1.part1, Days.D1.part2)
    | 2 -> (Days.D2.part1, Days.D2.part2)
    | 3 -> (Days.D3.part1, Days.D3.part2)
    | 4 -> (Days.D4.part1, Days.D4.part2)
    | 5 -> (Days.D5.part1, Days.D5.part2)
    | 6 -> (Days.D6.part1, Days.D6.part2)
    | 7 -> (Days.D7.part1, Days.D7.part2)
    | 8 -> (Days.D8.part1, Days.D8.part2)
    | _ -> failwith "Invalid day"
  in

  match !part with
  | 1 -> printf "part 1 solution is %s\n\n" (part1_fn !input_file)
  | 2 -> printf "part 2 solution is %s\n\n" (part2_fn !input_file)
  | _ -> failwith "Invalid part"
