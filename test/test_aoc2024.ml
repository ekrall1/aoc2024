(* day 1 tests *)
let day1_test_input = "../../../test/d1_test"
let () = assert (Days.D1.part1 day1_test_input = "11")
let () = assert (Days.D1.part2 day1_test_input = "31")

(* day 2 tests *)
let day2_test_input = "../../../test/d2_test"
let () = assert (Days.D2.part1 day2_test_input = "2")
let () = assert (Days.D2.part2 day2_test_input = "4")

(* day 3 tests *)
let day3_test_input = "../../../test/d3_test"
let day3_test_input2 = "../../../test/d3_test2"
let () = assert (Days.D3.part1 day3_test_input = "161")
let () = assert (Days.D3.part2 day3_test_input2 = "48")
