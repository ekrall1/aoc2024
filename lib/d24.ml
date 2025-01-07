open Z3
open Read_input
open Base

(*** Logic circuit solver - Advent of code day 24 ***)

type var = Var of { name : string; value : int }

type gate =
  | Gate of { logical : string; lhs1 : string; lhs2 : string; rhs : string }

type input = Input of { vars : var list; gates : gate list }

let make_opt_var ctx name = Arithmetic.Integer.mk_const_s ctx name

let initialize_vars ctx (Input input : input) =
  let var_hm : (string, Expr.expr) Base.Hashtbl.t =
    Hashtbl.create (module String)
  in

  let var_set = ref (Set.empty (module String)) in

  let vars = input.vars in
  let gate_data = input.gates in

  List.iter vars ~f:(fun (Var x) ->
      let var = make_opt_var ctx x.name in
      Hashtbl.set var_hm ~key:x.name ~data:var;
      var_set := Set.add !var_set x.name);

  List.iter gate_data ~f:(fun (Gate g) ->
      let vars = [ g.lhs1; g.lhs2; g.rhs ] in
      List.iter vars ~f:(fun x ->
          if not (Set.mem !var_set x) then (
            let var = make_opt_var ctx x in
            Hashtbl.set var_hm ~key:x ~data:var;
            var_set := Set.add !var_set x)));

  var_hm

let make_obj ctx =
  let z = Arithmetic.Real.mk_const_s ctx "z" in
  let obj =
    Arithmetic.mk_add ctx
      [ Arithmetic.mk_mul ctx [ Arithmetic.Real.mk_numeral_i ctx 1; z ] ]
  in
  let obj_c1 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_add ctx [ z ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in
  let obj_c2 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_add ctx [ z ])
      (Arithmetic.Real.mk_numeral_i ctx 0)
  in

  ((z, obj), (obj_c1, obj_c2))

let make_wire_constraints ctx (Input input) var_hm constraint_hm =
  List.iteri input.vars ~f:(fun idx (Var x) ->
      let var = Hashtbl.find_exn var_hm x.name in
      let c1 =
        Arithmetic.mk_ge ctx
          (Arithmetic.mk_add ctx [ var ])
          (Arithmetic.Integer.mk_numeral_i ctx x.value)
      in
      let c2 =
        Arithmetic.mk_le ctx
          (Arithmetic.mk_add ctx [ var ])
          (Arithmetic.Integer.mk_numeral_i ctx x.value)
      in
      Hashtbl.set constraint_hm
        ~key:(Printf.sprintf "%d_c1_%s" idx x.name)
        ~data:c1;
      Hashtbl.set constraint_hm
        ~key:(Printf.sprintf "%d_c2_%s" idx x.name)
        ~data:c2)

let make_var_constriants ctx (var_hm : (string, Expr.expr) Base.Hashtbl.t)
    constraint_hm =
  List.iteri (Hashtbl.to_alist var_hm) ~f:(fun idx (k, var) ->
      let c1 =
        Arithmetic.mk_ge ctx
          (Arithmetic.mk_add ctx [ var ])
          (Arithmetic.Integer.mk_numeral_i ctx 0)
      in
      let c2 =
        Arithmetic.mk_le ctx
          (Arithmetic.mk_add ctx [ var ])
          (Arithmetic.Integer.mk_numeral_i ctx 1)
      in
      Hashtbl.set constraint_hm
        ~key:(Printf.sprintf "var_%d_c1_%s" idx k)
        ~data:c1;
      Hashtbl.set constraint_hm
        ~key:(Printf.sprintf "var_%d_c2_%s" idx k)
        ~data:c2)

(**
z <= x
z <= y
z >= x + y - 1
*)
let make_and_constraints ctx (Gate g) constraint_hm var_hm idx =
  let lhs1 = Hashtbl.find_exn var_hm g.lhs1 in
  let lhs2 = Hashtbl.find_exn var_hm g.lhs2 in
  let rhs = Hashtbl.find_exn var_hm g.rhs in
  let c1 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx [ rhs; lhs1 ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c2 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx [ rhs; lhs2 ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c3 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx
         [
           rhs;
           Arithmetic.mk_add ctx
             [ lhs1; lhs2; Arithmetic.Integer.mk_numeral_i ctx (-1) ];
         ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_AND_c1_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c1;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_AND_c2_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c2;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_AND_c3_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c3;
  ()

(**
z >= x
z >= y
z <= x + y
*)
let make_or_constraints ctx (Gate g) constraint_hm var_hm idx =
  let lhs1 = Hashtbl.find_exn var_hm g.lhs1 in
  let lhs2 = Hashtbl.find_exn var_hm g.lhs2 in
  let rhs = Hashtbl.find_exn var_hm g.rhs in
  let c1 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx [ rhs; lhs1 ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c2 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx [ rhs; lhs2 ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c3 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx [ rhs; Arithmetic.mk_add ctx [ lhs1; lhs2 ] ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_OR_c1_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c1;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_OR_c2_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c2;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_OR_c3_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c3;
  ()

(**
z <= x + y
z >= x - y
z >= y - x
z <= 2 - x - y
*)
let make_xor_constraints ctx (Gate g) constraint_hm var_hm idx =
  let lhs1 = Hashtbl.find_exn var_hm g.lhs1 in
  let lhs2 = Hashtbl.find_exn var_hm g.lhs2 in
  let rhs = Hashtbl.find_exn var_hm g.rhs in
  let c1 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx [ rhs; Arithmetic.mk_add ctx [ lhs1; lhs2 ] ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c2 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx [ rhs; Arithmetic.mk_sub ctx [ lhs1; lhs2 ] ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c3 =
    Arithmetic.mk_ge ctx
      (Arithmetic.mk_sub ctx [ rhs; Arithmetic.mk_sub ctx [ lhs2; lhs1 ] ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  let c4 =
    Arithmetic.mk_le ctx
      (Arithmetic.mk_sub ctx
         [
           rhs;
           Arithmetic.mk_sub ctx
             [ Arithmetic.Integer.mk_numeral_i ctx 2; lhs1; lhs2 ];
         ])
      (Arithmetic.Integer.mk_numeral_i ctx 0)
  in
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_XOR_c1_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c1;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_XOR_c2_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c2;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_XOR_c3_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c3;
  Hashtbl.set constraint_hm
    ~key:(Printf.sprintf "%d_XOR_c4_%s_%s_%s" idx g.lhs1 g.lhs2 g.rhs)
    ~data:c4;
  ()

let parse_z_result k v model =
  let nm_lst = String.split ~on:'z' k in
  let ord = Int.of_string (List.nth_exn nm_lst 1) in
  let z_value = Model.eval model v true |> Option.value_exn |> Expr.to_string in
  (ord, z_value)

let solver (Input input : input) =
  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let ctx = mk_context cfg in
  let opt = Optimize.mk_opt ctx in

  (* initialize variables *)
  let var_hm = initialize_vars ctx (Input input) in
  let constraint_hm = Hashtbl.create (module String) in

  (* objective fcn *)
  let (_, obj), (obj_c1, obj_c2) = make_obj ctx in

  (* 0, 1 bounds *)
  let () = make_var_constriants ctx var_hm constraint_hm in

  (* initial wire value constraints *)
  let () = make_wire_constraints ctx (Input input) var_hm constraint_hm in

  List.iteri input.gates ~f:(fun idx (Gate g) ->
      match g.logical with
      | "AND" -> make_and_constraints ctx (Gate g) constraint_hm var_hm idx
      | "OR" -> make_or_constraints ctx (Gate g) constraint_hm var_hm idx
      | "XOR" -> make_xor_constraints ctx (Gate g) constraint_hm var_hm idx
      | _ -> failwith "bad logic gate input");

  Optimize.add opt ([ obj_c1; obj_c2 ] @ Hashtbl.data constraint_hm);

  let _ = Optimize.minimize opt obj in

  let res =
    match Optimize.check opt with
    | Solver.SATISFIABLE -> (
        match Optimize.get_model opt with
        | Some model ->
            let filtered_hm =
              Hashtbl.filter_keys var_hm ~f:(fun k ->
                  String.is_prefix k ~prefix:"z")
            in
            let z_expr = Hashtbl.to_alist filtered_hm in
            let z_vals =
              List.map z_expr ~f:(fun (k, v) -> parse_z_result k v model)
            in
            List.sort z_vals ~compare:(fun (x1, _) (x2, _) -> Int.compare x2 x1)
        | None ->
            failwith "Error: Model not available despite satisfiable result.\n")
    | _ -> failwith "model did not solve"
  in

  res

let convert_var info_lst =
  (List.hd_exn info_lst, Int.of_string (String.strip (List.nth_exn info_lst 1)))

let parse_vars lst =
  let lst = String.split ~on:'\n' lst in
  List.map lst ~f:(fun var ->
      let info_lst = String.split ~on:':' var in
      let converted = convert_var info_lst in
      Var { name = fst converted; value = snd converted })

let parse_gates lst =
  let lst = String.split ~on:'\n' lst in
  let split_lst = List.map lst ~f:(fun x -> String.split ~on:' ' x) in
  let filter_lst =
    List.filter split_lst ~f:(fun e -> Int.equal (List.length e) 5)
  in
  Stdlib.Printf.printf "%d\n" (List.length split_lst);
  List.map filter_lst ~f:(fun x ->
      Gate
        {
          logical = List.nth_exn x 1;
          lhs1 = List.hd_exn x;
          lhs2 = List.nth_exn x 2;
          rhs = List.nth_exn x 4;
        })

let solve_part_1 input_str =
  let unparsed = input_str |> Str.split (Str.regexp "\n\n+") in

  let model_input =
    Input
      {
        gates = parse_gates (List.nth_exn unparsed 1);
        vars = parse_vars (List.hd_exn unparsed);
      }
  in

  let z_results = solver model_input in
  Int.of_string ("0b" ^ String.concat (List.map z_results ~f:(fun x -> snd x)))

let solve_part_2 input_str =
  let unparsed = input_str |> Str.split (Str.regexp "\n\n+") in

  let model_input =
    Input
      {
        gates = parse_gates (List.nth_exn unparsed 1);
        vars = parse_vars (List.hd_exn unparsed);
      }
  in

  let z_results = solver model_input in
  "0b" ^ String.concat (List.map z_results ~f:(fun x -> snd x))

let part1 (file_name : string) =
  let input_str = file_name |> Read_input.read_input_file in
  solve_part_1 input_str |> Int.to_string

let part2 (file_name : string) =
  let input_str = file_name |> Read_input.read_input_file in
  solve_part_2 input_str
