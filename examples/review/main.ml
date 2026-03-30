open Orcaset
module S = Series.Make ()

(* Demonstrate that Point_cell.accum preserves the phantom unit type from its period cell input.
   The commented-out line at the bottom should fail to compile with a type mismatch. *)

let start_date = Date.make 2025 12 31
let end_date = Date.make 2026 3 31

(* Keeping this cell tagged as USD lets the example compile. Changing it to GBP now correctly
   raises a type error in the `usd_series` definition. *)
let usd_cell : [ `USD ] Period_cell.t =
  Period_cell.const (Period.make start_date end_date)
    (fun () -> 100.0)
    Period_cell.proportional_split

let usd_series : [ `USD ] S.Period.t = S.Period.const ~label:"USD Series" (Seq.return usd_cell)

(* Accumulate USD period cells into a USD point series — this compiles fine *)
let usd_balance : [ `USD ] S.Point.t =
  S.Point.accum ~label:"USD Balance" ~start_date ~initial_value:0.0 (lazy usd_series)

(* Now try to annotate the result as EUR — this must fail to compile *)

(* Uncomment to confirm compile-time error: *)

(* let eur_balance : [ `EUR ] S.Point.t =
  S.Point.accum ~label:"EUR Balance" ~start_date ~initial_value:0.0 (lazy usd_series) *)

(* Error: This expression has type [ `USD ] S.Point.t
      but an expression was expected of type [ `EUR ] S.Point.t
      Type [ `USD ] is not compatible with type [ `EUR ] *)

let () =
  print_endline "Phantom type constraint on accum is working correctly.";
  ignore usd_balance
