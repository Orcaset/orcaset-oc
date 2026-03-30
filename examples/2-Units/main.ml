open Orcaset
module S = Series.Make ()

(* This example shows how Orcaset protects against unit/currency mismatches at compile time. *)

(* Cells and series carry a phantom type tag (e.g. [`USD] or [`EUR] for currencies) that is checked
   by the OCaml type system. Types parameterized by different unit tags are considered different types
   in OCaml, so they are not interchangeable without explicit conversion.
   
   In code, the library exposes Period_cell.t and S.Period.t as below (with analogous 
   definitions for point-wise cells and series):

     type +'c Period_cell.t
     type  'c S.Period.t

   The constraints cost nothing at runtime. They are erased after type-checking. The compiler can 
   also often infer the correct units from context, so unit tags don't litter the codebase.

   Tags are open polymorphic variants, so you can define whatever units are relevant. Currencies
   are a natural example, but you could also use tags to denote assumption frequencies (e.g.
   [`Monthly], [`Quarterly], etc.) or other domain-specific units.
    *)

(* ── Helpers ──────────────────────────────────────────────────────────────── *)

let start_date = Date.make 2025 12 31
let end_date = Date.make 2026 3 31

(* Helper function to create a constant series with a given value and phantom type tag *)
let const_series (type c) ~label (value : float) : c S.Period.t =
  S.Period.const ~label
    (Seq.return
       (Period_cell.const (Period.make start_date end_date)
          (fun () -> value)
          Period_cell.proportional_split))

(* ── Line items denominated in different currencies ───────────────────────────── *)

(* Revenues in GBP *)
let revenue : [ `GBP ] S.Period.t = const_series ~label:"Revenue (GBP)" 10_000.0

(* Costs in USD *)
let costs : [ `USD ] S.Period.t = const_series ~label:"Costs (USD)" (-6_000.0)

(* ── Type-safe arithmetic failure case ─────────────────────────────────────────────────── *)

(* Impossible to directly add revenues and costs since they have different currencies.
   The line below raises a compile-time error and cannot be executed. *)

(* Uncomment the line below to confirm it raises a compile-time error. *)

(* let profit = S.Period.sum ~label:"Bad Profit" revenue costs *)

(* ── Combining requires explicit currency conversion ────────────────────────────────────────── *)

(* To combine GBP and USD values you must explicitly provide a conversion function. *)
let usd_revenue = S.Period.convert ~label:"Revenue (USD)" (fun _ r -> r *. 1.34) (lazy revenue)
let profit = S.Period.sum ~label:"Profit (USD)" (lazy usd_revenue) (lazy costs)

(* ── Print results ────────────────────────────────────────────────────────── *)

let () =
  match S.Period.to_seq [ usd_revenue; costs; profit ] with
  | [ revenue_seq; costs_seq; profit_seq ] -> (
      let r_cell = Seq.uncons revenue_seq |> Option.get |> fst in
      let c_cell = Seq.uncons costs_seq |> Option.get |> fst in
      let p_cell = Seq.uncons profit_seq |> Option.get |> fst in
      let period = Period_cell.period r_cell in
      match
        Eval.many [ [ Eval.PeriodCell r_cell; Eval.PeriodCell c_cell; Eval.PeriodCell p_cell ] ]
      with
      | [ [ rv; cv; pv ] ] ->
          Printf.printf "%-25s %12s %16s %14s\n" "Period" "Revenue (USD)" "Costs (USD)"
            "Profit (USD)";
          Printf.printf "%s\n" (String.make 70 '-');
          Printf.printf "%-25s %12.2f %16.2f %14.2f\n" (Period.to_string period) rv cv pv
      | _ -> assert false)
  | _ -> assert false
