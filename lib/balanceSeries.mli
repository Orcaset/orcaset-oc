(** A balance series represents a continuous balance that can be queried at any date.

    Unlike a sequence of point-in-time snapshots, a balance series knows how to compute its value at
    any date by applying accumulated flow (from accruals, transactions, or custom sources) to an
    initial balance.

    This design ensures that interim queries (e.g., querying a balance on June 30 when the
    underlying flow has annual periods) return correct pro-rata values rather than stale snapshots.
*)

type point = { date : CalendarLib.Date.t; amount : float Lazy.t }
(** A point-in-time balance snapshot. Used for materialized output. *)

type t
(** A queryable balance series. Abstract to ensure balances are constructed with proper flow
    information. *)

val from_accruals :
  initial_date:CalendarLib.Date.t -> initial_amount:float Lazy.t -> Accrual.t Seq.t Lazy.t -> t
(** Create a balance series from an accrual sequence. The balance at any date is computed as:
    initial_amount + sum of accrued values from initial_date to query_date. Accruals are split
    pro-rata when the query date falls within an accrual period. *)

val from_transactions :
  initial_date:CalendarLib.Date.t -> initial_amount:float Lazy.t -> Transaction.t Seq.t Lazy.t -> t
(** Create a balance series from a transaction sequence. The balance at any date is computed as:
    initial_amount + sum of transactions from initial_date to query_date. Transactions on the
    query_date are included. *)

val from_flow :
  initial_date:CalendarLib.Date.t ->
  initial_amount:float Lazy.t ->
  sum_between:(start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> float) ->
  t
(** Create a balance series from a custom flow function. The sum_between function should return the
    total flow amount between the given dates (inclusive of start, exclusive of end, or as
    appropriate for the domain). This is the most flexible constructor, allowing arbitrary flow
    logic. *)

val constant : float Lazy.t -> t
(** Create a balance series with a constant value at all dates. *)

val on : t -> CalendarLib.Date.t -> point
(** Query the balance at a specific date. Returns a point with the computed balance. *)

val combine : (float -> float -> float) -> t -> t -> t
(** Combine two balance series using a binary function. The resulting series computes its value at
    any date by applying the function to the values of the input series at that date. Common usage:
    [combine ( +. ) assets liabilities] *)

val sum : t -> t -> t
(** [sum a b] is equivalent to [combine ( +. ) a b]. *)

val sub : t -> t -> t
(** [sub a b] is equivalent to [combine ( -. ) a b]. *)

val at_dates : t -> CalendarLib.Date.t Seq.t -> point Seq.t
(** Materialize the balance series at a sequence of dates. Returns a sequence of point snapshots,
    one for each input date. *)

val at_periods : t -> Period.t Seq.t -> point Seq.t
(** Materialize the balance series at the end date of each period. Convenience function equivalent
    to [at_dates t (Seq.map (fun p -> p.Period.end_date) periods)]. *)

val point_to_string : point -> string
(** Convert a point to a human-readable string representation. *)
