open Orcaset
module S = Series.Make ()

(* Basic financial model example with three line items: revenue, expenses, and income.
   Revenue: Grows at a constant monthly rate using Unfold
   Expenses: Fixed percentage of revenue
   Income: Sum of revenue and expenses
 *)

let initial_value = 1000.0
let expense_margin = -0.6
let initial_start_date = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()
let growth_rate = 0.25
let initial_period = Period.make initial_start_date (Date.shift offset initial_start_date)

let revenue_step curr_period last_period =
  S.Period.Step
    {
      period = curr_period;
      queries = [ S.Period.self ~period:last_period ~reduce:S.Period.reduce_sum ];
      f =
        (fun values ->
          let yf = Yf.cmonthly (Period.start_date curr_period) (Period.end_date curr_period) in
          let growth = (1.0 +. growth_rate) ** yf in
          match values with [ last_value ] -> last_value *. growth | _ -> 0.0);
    }

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.shift offset last_period in
    Seq.Cons (revenue_step current_period last_period, generate_cells current_period)
  in
  S.Period.unfold ~label:"Revenue"
    ~deps:(fun _ctx -> ())
    ~cells:(fun () ->
      Seq.cons
        (S.Period.Const { period = initial_period; f = (fun () -> initial_value) })
        (generate_cells initial_period))

(* let revenue =
  let periods = Period.make_seq ~start_date:initial_start_date ~offset in
  let revenue_cells =
    Seq.map
      (fun period ->
        let period_start = Period.start_date period in
        let growth = (1. +. growth_rate) ** Yf.cmonthly initial_start_date period_start in
        Period_cell.const period (fun () -> initial_value *. growth) Period_cell.proportional_split)
      periods
  in
  S.Period.of_seq ~label:"Revenue" revenue_cells *)

let expenses = S.Period.map ~label:"Expenses" (fun r -> r *. expense_margin) (lazy revenue)
let income = S.Period.sum ~label:"Income" (lazy revenue) (lazy expenses)

(* ---- Print output --- *)
let num_periods = 6

let query_periods =
  let query_offset = Offset.make ~months:1 ~month_end:true () in
  List.of_seq
    (Seq.take num_periods (Period.make_seq ~start_date:initial_start_date ~offset:query_offset))

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

let lw = 16
let cw = 14
let pad_right n s = if String.length s >= n then s else s ^ String.make (n - String.length s) ' '
let pad_left n s = if String.length s >= n then s else String.make (n - String.length s) ' ' ^ s

let () =
  let rows =
    List.map
      (fun s ->
        (S.Period.label s, List.map extract_value (S.eval_many (S.Period.query query_periods s))))
      [ revenue; expenses; income ]
  in
  Printf.printf "%s" (pad_right lw "");
  List.iter (fun d -> Printf.printf "%s" (pad_left cw (Date.to_string d))) period_end_dates;
  print_newline ();
  Printf.printf "%s\n" (String.make (lw + (cw * List.length period_end_dates)) '-');
  List.iter
    (fun (label, values) ->
      Printf.printf "%s" (pad_right lw label);
      List.iter (fun v -> Printf.printf "%s" (pad_left cw (format_number v))) values;
      print_newline ())
    rows;

  (* Write dependency graph for income to a dot file *)
  let oc = open_out "examples/1-Basic/income_deps.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Graph.pp_dot ppf [ S.period_to_graph income ];
  Format.pp_print_flush ppf ();
  close_out oc
