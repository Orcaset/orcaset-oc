open Orcaset
module S = Series.Make ()

(* --- Configuration -------------------------------------------------------- *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()
let quarters = Period.make_seq ~start_date ~offset
let initial_period = Period.make start_date (Date.shift offset start_date)
let useful_life = 4
let starting_ppe = -75.0
let neg_offset n = Offset.make ~months:(-(n * 3)) ~month_end:true ()

(* --- Capex ---------------------------------------------------------------- *)

let capex =
  let step period =
    S.Period.step ~period
      (S.Period.Query.self ~period:(Period.prev (neg_offset 1) period) ~reduce:S.Period.reduce_sum)
      (fun prev -> prev -. 100.0)
  in
  let rec cells last () =
    let p = Period.next offset last in
    Seq.Cons (step p, cells p)
  in
  S.Period.unfold_self ~label:"Capex" ~cells:(fun () ->
      Seq.cons (S.Period.const ~period:initial_period (fun () -> -100.0)) (cells initial_period))

(* --- Existing PPE Depreciation Run-off ------------------------------------ *)

let existing_runoff =
  let p1 = Period.make (Date.make 2025 12 31) (Date.make 2026 3 31) in
  let p2 = Period.make (Date.make 2026 3 31) (Date.make 2026 6 30) in
  S.Period.of_seq ~label:"Existing Runoff"
    (List.to_seq
       [
         Period_cell.const p1 (fun () -> -50.0) Period_cell.proportional_split;
         Period_cell.const p2 (fun () -> -25.0) Period_cell.proportional_split;
       ])

(* --- Depreciation by Lookback Offset -------------------------------------- *)

(* dep_offset.(i) in period P = capex(P - i*offset) / useful_life. *)
let dep_offsets =
  List.init useful_life (fun i ->
      S.Period.unfold
        ~label:(Printf.sprintf "Dep Offset %d" i)
        ~deps:(S.Period.dep_period (lazy capex))
        ~cells:(fun capex_dep ->
          Seq.map
            (fun period ->
              let lb = Period.shift (neg_offset i) period in
              S.Period.step ~period
                (S.Period.Query.period capex_dep ~period:lb ~reduce:S.Period.reduce_sum) (fun cv ->
                  cv /. Float.of_int useful_life))
            quarters))

(* --- Total Depreciation --------------------------------------------------- *)

let depreciation =
  let total_offsets =
    S.Period.sum ~label:"Total Future Depreciation" (List.map (fun o -> lazy o) dep_offsets)
  in
  S.Period.sum ~label:"Depreciation" [ lazy existing_runoff; lazy total_offsets ]

(* --- PPE ------------------------------------------------------------------ *)

let ppe_change = S.Period.sub ~label:"PPE Change" (lazy capex) (lazy depreciation)

let ppe_net =
  S.Point.accum ~label:"PPE Net" ~start_date ~initial_value:starting_ppe (lazy ppe_change)

(* --- Output --------------------------------------------------------------- *)

let num_periods = 8
let query_periods = List.of_seq (Seq.take num_periods quarters)
let period_end_dates = List.map Period.end_date query_periods

let extract_value (r : _ S.eval_result) =
  match r with
  | Period { value = Amount v; _ } -> v
  | Point { point = Some (_, Amount v); _ } -> v
  | Point { point = None; _ } -> 0.0

let format_number v =
  let s = Printf.sprintf "%.0f" (Float.abs v) in
  let len = String.length s in
  let buf = Buffer.create (len + (len / 3)) in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf c)
    s;
  let formatted = Buffer.contents buf in
  if v < -0.5 then "(" ^ formatted ^ ")" else formatted

let lw = 24
let cw = 14
let pad_right n s = if String.length s >= n then s else s ^ String.make (n - String.length s) ' '
let pad_left n s = if String.length s >= n then s else String.make (n - String.length s) ' ' ^ s

let print_header dates =
  Printf.printf "%s" (pad_right lw "");
  List.iter (fun d -> Printf.printf "%s" (pad_left cw (Date.to_string d))) dates;
  print_newline ();
  Printf.printf "%s\n" (String.make (lw + (cw * List.length dates)) '-')

let print_row label values =
  Printf.printf "%s" (pad_right lw label);
  List.iter (fun v -> Printf.printf "%s" (pad_left cw (format_number v))) values;
  print_newline ()

let eval_period s = List.map extract_value (S.eval_many (S.Period.query query_periods s))
let eval_point s ds = List.map extract_value (S.eval_many (S.Point.query_many ds s))

(* --- Summary -------------------------------------------------------------- *)

let print_summary () =
  Printf.printf "\n=== Summary ===\n\n";
  print_header period_end_dates;
  print_row "Capex" (eval_period capex);
  print_row "Depreciation" (eval_period depreciation);
  print_newline ();
  print_row "PPE, net" (eval_point ppe_net period_end_dates);
  print_row "Starting PPE, net" [ starting_ppe ]

(* --- Depreciation Schedule ------------------------------------------------ *)

(* Transpose the offset-series view into a per-vintage view. For vintage v
   (capex acquired in period v), depreciation in period p is
   capex(v) / useful_life when v <= p < v + useful_life, else 0. *)
let print_schedule () =
  let n = List.length query_periods in
  let capex_values = eval_period capex in
  let runoff_values = eval_period existing_runoff in

  Printf.printf "\n=== Depreciation Schedule ===\n\n";
  Printf.printf "%s%s\n\n" (pad_right lw "Use life (quarters)") (string_of_int useful_life);
  print_header period_end_dates;
  print_row "Existing PPE run-off" runoff_values;

  List.iteri
    (fun v capex_v ->
      let dep_per_period = capex_v /. Float.of_int useful_life in
      let values =
        List.init n (fun p -> if p >= v && p < v + useful_life then dep_per_period else 0.0)
      in
      print_row (Date.to_string (List.nth period_end_dates v)) values)
    capex_values

(* --- Dependency Graph ----------------------------------------------------- *)

let print_dep_graph () =
  let dot_path = "examples/6-Depreciation/depreciation_deps.dot" in
  let oc = open_out dot_path in
  let ppf = Format.formatter_of_out_channel oc in
  Graph.pp_dot ppf [ S.period_to_graph depreciation ];
  Format.pp_print_flush ppf ();
  close_out oc;
  Printf.printf "\n=== Dependency Graph ===\n\n";
  Printf.printf "Written to %s\n" dot_path

let () =
  print_summary ();
  print_schedule ();
  print_dep_graph ();
  print_newline ()
