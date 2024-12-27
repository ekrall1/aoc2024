open Read_input
open Base
module IntSet = Stdlib.Set.Make (Int)
module IntMap = Stdlib.Map.Make (Int)

(**day 15 robot puzzle
As the robot (@) attempts to move, if there are any boxes (O) in the way, the robot will also attempt to push those boxes. 
However, if this action would cause the robot or a box to move into a wall (#), nothing moves instead, including the robot.
*)

let initialize_hm lst =
  let hm = Hashtbl.create (module Char) in
  let rec populate rem col row =
    match rem with
    | hd :: tl ->
        if Char.equal hd '\n' then populate tl 0 (row + 1)
        else (
          Hashtbl.update hm hd ~f:(function
            | None -> [ (col, row) ]
            | Some x -> x @ [ (col, row) ]);
          populate tl (col + 1) row)
    | [] -> hm
  in
  populate lst 0 0

let transform_grid_p2 grid =
  let r = Array.length grid in
  let robot = ref (0, 0) in

  for i = 0 to r - 1 do
    let row = Array.to_list grid.(i) in
    let transformed_row =
      List.foldi row ~init:[] ~f:(fun x acc ch ->
          match ch with
          | '#' -> '#' :: '#' :: acc
          | '.' -> '.' :: '.' :: acc
          | '@' ->
              robot := (i, x * 2);
              '.' :: '.' :: acc
          | 'O' -> ']' :: '[' :: acc
          | _ -> ch :: acc)
    in
    grid.(i) <- Array.of_list (List.rev transformed_row)
  done;
  (grid, !robot)

let move_around_p2 grid directions robot_in =
  let m = Array.length grid in
  let robot = ref robot_in in

  String.iter directions ~f:(fun ch ->
      let i, j = !robot in
      match ch with
      | '<' ->
          let k = ref (j - 1) in
          while Char.equal grid.(i).(!k) ']' do
            k := !k - 2
          done;
          if Char.equal grid.(i).(!k) '.' then (
            for l = !k to j - 1 do
              let tmp = grid.(i).(l) in
              grid.(i).(l) <- grid.(i).(l + 1);
              grid.(i).(l + 1) <- tmp
            done;
            robot := (i, j - 1))
      | '>' ->
          let k = ref (j + 1) in
          while Char.equal grid.(i).(!k) '[' do
            k := !k + 2
          done;
          if Char.equal grid.(i).(!k) '.' then (
            for l = !k downto j + 1 do
              let tmp = grid.(i).(l) in
              grid.(i).(l) <- grid.(i).(l - 1);
              grid.(i).(l - 1) <- tmp
            done;
            robot := (i, j + 1))
      | '^' | 'v' ->
          let dir =
            if Char.equal ch '^' then (-1, fun x -> x - 1)
            else (1, fun x -> x + 1)
          in
          let queue = ref [ (i + fst dir, j) ] in
          let rows = Hashtbl.create (module Int) in
          let can_update = ref true in

          let rec bfs () =
            match !queue with
            | [] -> ()
            | (x, y) :: rest -> (
                queue := rest;
                match grid.(x).(y) with
                | '#' ->
                    can_update := false;
                    ()
                | '[' ->
                    (let set =
                       IntSet.union (IntSet.of_list [ y ])
                         (Hashtbl.find rows x
                         |> Option.value ~default:IntSet.empty)
                     in

                     Hashtbl.update rows x ~f:(function
                       | None -> set
                       | Some existing_set -> IntSet.union existing_set set);

                     if Char.equal ch '^' then
                       queue := (x - 1, y) :: (x - 1, y + 1) :: !queue
                     else queue := (x + 1, y) :: (x + 1, y + 1) :: !queue);

                    bfs ()
                | ']' ->
                    (let set =
                       IntSet.union (IntSet.of_list [ y ])
                         (Hashtbl.find rows x
                         |> Option.value ~default:IntSet.empty)
                     in

                     Hashtbl.update rows x ~f:(function
                       | None -> set
                       | Some existing_set -> IntSet.union existing_set set);
                     if Char.equal ch '^' then
                       queue := (x - 1, y) :: (x - 1, y - 1) :: !queue
                     else queue := (x + 1, y) :: (x + 1, y - 1) :: !queue);

                    bfs ()
                | '.' ->
                    let set =
                      IntSet.add y
                        (Hashtbl.find rows x
                        |> Option.value ~default:IntSet.empty)
                    in
                    Hashtbl.update rows x ~f:(function
                      | None -> set
                      | Some existing_set -> IntSet.union existing_set set);
                    bfs ()
                | _ -> bfs ())
          in
          bfs ();

          if !can_update then (
            let rowsort =
              if Char.equal '^' ch then
                List.sort (Hashtbl.keys rows) ~compare:Poly.compare
              else
                List.rev (List.sort (Hashtbl.keys rows) ~compare:Poly.compare)
            in
            List.iter rowsort ~f:(fun x ->
                let ysset =
                  Hashtbl.find rows x |> Option.value ~default:IntSet.empty
                in
                let ys_unsorted = IntSet.to_list ysset in
                let ys = List.sort ys_unsorted ~compare:Poly.compare in
                List.iter ys ~f:(fun y ->
                    if Char.equal '^' ch then (
                      let tmp = grid.(x).(y) in
                      grid.(x).(y) <- grid.(x + 1).(y);
                      grid.(x + 1).(y) <- tmp)
                    else
                      let tmp = grid.(x).(y) in
                      grid.(x).(y) <- grid.(x - 1).(y);
                      grid.(x - 1).(y) <- tmp));

            robot := (snd dir (fst !robot), j))
      | _ -> ());

  let total = ref 0 in
  for i = 0 to m - 1 do
    for j = 0 to Array.length grid.(i) - 1 do
      Stdlib.Printf.printf "%c" grid.(i).(j);
      if Char.equal grid.(i).(j) '[' then total := !total + ((100 * i) + j)
    done;
    Stdlib.Printf.printf "\n"
  done;
  Stdlib.Printf.printf "%d,%d\n" (fst !robot) (snd !robot);
  !total

let remove_element lst elem =
  List.filter lst ~f:(fun x -> not (fst x = fst elem && snd x = snd elem))

let add_element lst elem = lst @ [ elem ]

(* push blocks *)
let push circles dots robot dxdy =
  let circleref = ref circles in
  let dotref = ref dots in
  let rec calculate_push pos new_circles =
    let nxt = (fst pos + fst dxdy, snd pos + snd dxdy) in
    if List.exists circles ~f:(fun x -> fst x = fst nxt && snd x = snd nxt) then (
      circleref := remove_element !circleref nxt;
      calculate_push nxt (new_circles @ [ nxt ]))
    else if List.exists dots ~f:(fun x -> fst x = fst nxt && snd x = snd nxt)
    then (
      dotref := remove_element !dotref nxt;
      let robot_update, cir_update =
        match new_circles with
        | hd :: tl -> (hd, tl @ [ nxt ])
        | [] -> (robot, new_circles)
      in
      ((!circleref @ cir_update, !dotref @ [ robot ]), [ robot_update ]))
    else ((circles, dots), [ robot ])
  in
  calculate_push (fst robot, snd robot) []

(* look ahead in the next direction, determine if movement happens *)
let look_ahead dir hm =
  let dxdy =
    match dir with
    | '>' -> (1, 0)
    | '<' -> (-1, 0)
    | '^' -> (0, -1)
    | 'v' -> (0, 1)
    | _ -> failwith "invalid direction"
  in

  let robot = List.hd_exn (Hashtbl.find hm '@' |> Option.value ~default:[]) in

  let nxt = (fst robot + fst dxdy, snd robot + snd dxdy) in

  let dots = Hashtbl.find hm '.' |> Option.value ~default:[] in
  let circles = Hashtbl.find hm 'O' |> Option.value ~default:[] in

  if List.exists dots ~f:(fun x -> fst x = fst nxt && snd x = snd nxt) then (
    let dots = remove_element dots nxt in
    let dots = add_element dots robot in
    Hashtbl.update hm '.' ~f:(function None -> dots | Some _ -> dots);
    Hashtbl.update hm '@' ~f:(function None -> [ nxt ] | Some _ -> [ nxt ]);
    hm)
  else if List.exists circles ~f:(fun x -> fst x = fst nxt && snd x = snd nxt)
  then (
    let updates = push circles dots robot dxdy in
    Hashtbl.update hm 'O' ~f:(function
      | None -> fst (fst updates)
      | Some _ -> fst (fst updates));
    Hashtbl.update hm '.' ~f:(function
      | None -> snd (fst updates)
      | Some _ -> snd (fst updates));
    Hashtbl.update hm '@' ~f:(function
      | None -> snd updates
      | Some _ -> snd updates);
    hm)
  else hm

let traverse hm directions =
  let rec move_around lst cur_hm =
    match lst with
    | hd :: tl ->
        let new_hm = look_ahead hd hm in
        move_around tl new_hm
    | [] -> cur_hm
  in
  move_around directions hm

let split_grid_and_movements input =
  let regex = Str.regexp "\n\n" in
  Str.split regex input

let solve_part_1 input_hm directions =
  let final_pos = traverse input_hm directions in
  let circles = Hashtbl.find final_pos 'O' |> Option.value ~default:[] in
  let robot = Hashtbl.find final_pos '@' |> Option.value ~default:[] in
  List.iter robot ~f:(fun x ->
      Stdlib.Printf.printf "robot\n%d,%d\n" (fst x) (snd x));
  List.fold_left circles ~init:0 ~f:(fun acc x -> acc + (100 * snd x) + fst x)
  |> Int.to_string

let solve_part_2 grid directions robot =
  move_around_p2 grid directions robot |> Int.to_string

let part1 (file_name : string) : string =
  let input =
    Read_input.read_input_file file_name |> split_grid_and_movements
  in
  let grid_char_lst = String.to_list (List.hd_exn input) in
  let directions_lst =
    List.filter
      (String.to_list (List.nth_exn input 1))
      ~f:(fun x -> not (Char.equal x '\n'))
  in
  let hm = initialize_hm grid_char_lst in
  solve_part_1 hm directions_lst

let part2 (file_name : string) =
  let input =
    Read_input.read_input_file file_name |> split_grid_and_movements
  in
  let grid =
    List.map
      (String.split ~on:'\n' (List.hd_exn input))
      ~f:(fun row -> Array.of_list (String.to_list row))
  in
  let grid_array = grid |> Array.of_list in
  let directions = List.last_exn input in
  let transform = transform_grid_p2 grid_array in
  let transformed_grid = fst transform in
  let robot = snd transform in
  solve_part_2 transformed_grid directions robot
