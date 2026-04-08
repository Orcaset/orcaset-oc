[@@@warning "-32"]

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
  S.Period.of_seq ~label
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
let profit = S.Period.sum ~label:"Profit (USD)" [ lazy usd_revenue; lazy costs ]

(* ── Output ───────────────────────────────────────────────────────────────── *)

let query_periods = [ Period.make start_date end_date ]

let () =
  let open S.Stmt in
  let stmt = period_total profit [ period_line usd_revenue; period_line costs ] in
  print_string (pp stmt query_periods)
