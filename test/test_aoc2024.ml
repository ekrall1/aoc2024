(* day 1 tests *)
let day1_test_input = "../test_data/d1_test"
let () = assert (Days.D1.part1 day1_test_input = "11")
let () = assert (Days.D1.part2 day1_test_input = "31")

(* day 2 tests *)
let day2_test_input = "../test_data/d2_test"
let () = assert (Days.D2.part1 day2_test_input = "2")
let () = assert (Days.D2.part2 day2_test_input = "4")

(* day 3 tests *)
let day3_test_input = "../test_data/d3_test"
let day3_test_input2 = "../test_data/d3_test2"
let () = assert (Days.D3.part1 day3_test_input = "161")
let () = assert (Days.D3.part2 day3_test_input2 = "48")

(* day 4 tests *)
let day4_test_input = "../test_data/d4_test"
let () = assert (Days.D4.part1 day4_test_input = "18")
let () = assert (Days.D4.part2 day4_test_input = "9")

(* day 5 tests *)
let day5_test_input = "../test_data/d5_test"
let () = assert (Days.D5.part1 day5_test_input = "143")
let () = assert (Days.D5.part2 day5_test_input = "123")

(* day 6 tests*)
let day6_test_input = "../test_data/d6_test"
let () = assert (Days.D6.part1 day6_test_input = "41")
let () = assert (Days.D6.part2 day6_test_input = "6")

(* day 7 tests*)
let day7_test_input = "../test_data/d7_test"
let () = assert (Days.D7.part1 day7_test_input = "3749")
let () = assert (Days.D7.part2 day7_test_input = "11387")

(* day 8 tests*)
let day8_test_input = "../test_data/d8_test"
let () = assert (Days.D8.part1 day8_test_input = "14")
let () = assert (Days.D8.part2 day8_test_input = "34")

(* day 9 tests*)
let day9_test_input = "../test_data/d9_test"
let () = assert (Days.D9.part1 day9_test_input = "1928")
let () = assert (Days.D9.part2 day9_test_input = "2858")

(* day 10 tests*)
let day10_test_input = "../test_data/d10_test"
let () = assert (Days.D10.part1 day10_test_input = "36")
let () = assert (Days.D10.part2 day10_test_input = "81")

(* day 11 tests*)
let day11_test_input = "../test_data/d11_test"
let () = assert (Days.D11.part1 day11_test_input = "55312")
let () = assert (Days.D11.part2 day11_test_input = "65601038650482")

(* day 12 tests*)
let day12_test_input = "../test_data/d12_test"
let () = assert (Days.D12.part1 day12_test_input = "1930")
let () = assert (Days.D12.part2 day12_test_input = "1206")

(* day 13 tests *)
let day13_test_input = "../test_data/d13_test"
let () = assert (Days.D13.part1 day13_test_input = "480")
let () = assert (Days.D13.part2 day13_test_input = "875318608908")

(* day 14 tests *)
let day14_test_input = "../test_data/d14_test"
let () = assert (Days.D14.part1 day14_test_input = "12")

(* day 15 tests *)
let day15_test_input = "../test_data/d15_test"
let day15_test_input2 = "../test_data/d15_test2"
let day15_test_input3 = "../test_data/d15_test3"
let () = assert (Days.D15.part1 day15_test_input = "2028")
let () = assert (Days.D15.part1 day15_test_input2 = "10092")
let () = assert (Days.D15.part1 day15_test_input3 = "908")
let () = assert (Days.D15.part2 day15_test_input = "1751")
let () = assert (Days.D15.part2 day15_test_input2 = "9021")
let () = assert (Days.D15.part2 day15_test_input3 = "618")

(* day 16 tests *)
let day16_test_input = "../test_data/d16_test"
let day16_test_input2 = "../test_data/d16_test2"
let day16_test_input3 = "../test_data/d16_test3"
let () = assert (Days.D16.part1 day16_test_input = "7036")
let () = assert (Days.D16.part1 day16_test_input2 = "11048")
let () = assert (Days.D16.part1 day16_test_input3 = "4013")
let () = assert (Days.D16.part2 day16_test_input = "45")
let () = assert (Days.D16.part2 day16_test_input2 = "64")
let () = assert (Days.D16.part2 day16_test_input3 = "14")

(* day 17 tests *)
let day17_test_input = "../test_data/d17_test"
let day17_test_input2 = "../test_data/d17_test2"
let day17_test_input3 = "../test_data/d17_test3"
let day17_test_input4 = "../test_data/d17_test4"
let day17_test_input5 = "../test_data/d17_test5"
let day17_test_input6 = "../test_data/d17_test6"
let day17_test_input7 = "../test_data/d17_test7"

let () =
  assert (
    Days.D17.part1_register day17_test_input
    = Days.D17.Registers { a = 0; b = 1; c = 9 })

let () = assert (Days.D17.part1 day17_test_input = "")
let () = assert (Days.D17.part1 day17_test_input2 = "0,1,2")

let () =
  assert (
    Days.D17.part1_register day17_test_input3
    = Days.D17.Registers { a = 0; b = 0; c = 0 })

let () = assert (Days.D17.part1 day17_test_input3 = "4,2,5,6,7,7,7,7,3,1,0")

let () =
  assert (
    Days.D17.part1_register day17_test_input4
    = Days.D17.Registers { a = 0; b = 26; c = 0 })

let () = assert (Days.D17.part1 day17_test_input4 = "")

let () =
  assert (
    Days.D17.part1_register day17_test_input5
    = Days.D17.Registers { a = 0; b = 44354; c = 43690 })

let () = assert (Days.D17.part1 day17_test_input5 = "")
let () = assert (Days.D17.part1 day17_test_input6 = "4,6,3,5,6,3,5,2,1,0")
let () = assert (Days.D17.part1 day17_test_input7 = "5,7,3,0")
let () = assert (Days.D17.part2 day17_test_input7 = "117440")

(* day 18 tests *)
let day18_test_input = "../test_data/d18_test"
let () = assert (Days.D18.part1 day18_test_input = "22")
let () = assert (Days.D18.part2 day18_test_input = "6,1")

(* day 19 tests *)
let day19_test_input = "../test_data/d19_test"
let () = assert (Days.D19.part1 day19_test_input = "6")
let () = assert (Days.D19.part2 day19_test_input = "16")

(* day 20 tests *)
let day20_test_input = "../test_data/d20_test"
let () = assert (Days.D20.solve_part_1 day20_test_input 2 = (44, 14))
let () = assert (Days.D20.solve_part_1 day20_test_input 4 = (30, 14))
let () = assert (Days.D20.solve_part_1 day20_test_input 6 = (16, 2))
let () = assert (Days.D20.solve_part_1 day20_test_input 8 = (14, 4))
let () = assert (Days.D20.solve_part_1 day20_test_input 10 = (10, 2))
let () = assert (Days.D20.solve_part_1 day20_test_input 12 = (8, 3))
let () = assert (Days.D20.solve_part_1 day20_test_input 20 = (5, 1))
let () = assert (Days.D20.solve_part_1 day20_test_input 36 = (4, 1))
let () = assert (Days.D20.solve_part_1 day20_test_input 38 = (3, 1))
let () = assert (Days.D20.solve_part_1 day20_test_input 40 = (2, 1))
let () = assert (Days.D20.solve_part_1 day20_test_input 64 = (1, 1))
let () = assert (Days.D20.solve_part_2 day20_test_input 50 = (285, 32))
let () = assert (Days.D20.solve_part_2 day20_test_input 52 = (253, 31))
let () = assert (Days.D20.solve_part_2 day20_test_input 54 = (222, 29))
let () = assert (Days.D20.solve_part_2 day20_test_input 56 = (193, 39))
let () = assert (Days.D20.solve_part_2 day20_test_input 58 = (154, 25))
let () = assert (Days.D20.solve_part_2 day20_test_input 60 = (129, 23))
let () = assert (Days.D20.solve_part_2 day20_test_input 62 = (106, 20))
let () = assert (Days.D20.solve_part_2 day20_test_input 64 = (86, 19))
let () = assert (Days.D20.solve_part_2 day20_test_input 66 = (67, 12))
let () = assert (Days.D20.solve_part_2 day20_test_input 68 = (55, 14))
let () = assert (Days.D20.solve_part_2 day20_test_input 70 = (41, 12))
let () = assert (Days.D20.solve_part_2 day20_test_input 72 = (29, 22))
let () = assert (Days.D20.solve_part_2 day20_test_input 74 = (7, 4))
let () = assert (Days.D20.solve_part_2 day20_test_input 76 = (3, 3))

(* day 21 tests *)
let day21_test_input = "../test_data/d21_test"
let () = assert (Days.D21.part1 day21_test_input = "126384")
let () = assert (Days.D21.part2 day21_test_input = "154115708116294")

(* day 22 tests *)
let day22_test_input = "../test_data/d22_test"
let day22_test_input2 = "../test_data/d22_test2"
let () = assert (Days.D22.part1 day22_test_input = "37327623")
let () = assert (Days.D22.part2 day22_test_input = "24")
let () = assert (Days.D22.part2 day22_test_input2 = "23")

(* day 23 tests *)
let day23_test_input = "../test_data/d23_test"
let () = assert (Days.D23.part1 day23_test_input = "7")
let () = assert (Days.D23.part2 day23_test_input = "co,de,ka,ta")

(* day 24 tests *)
let day24_test_input = "../test_data/d24_test"
let () = assert (Days.D24.part1 day24_test_input = "2024")

let () =
  assert (
    Days.D24.part2 day24_test_input
    = "ffh,mjb,tgd,wpb,z02,z03,z05,z06,z07,z08,z10,z11")
