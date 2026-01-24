type t = { date : CalendarLib.Date.t; value : float Lazy.t }
(** A transaction represents a change on a specific date. Unlike Balance which represents a
    snapshot, a transaction represents a discrete change (e.g. a cash payment, an increase in
    inventory from physical delivery). The value is lazily evaluated to allow mutual dependencies.
*)

val make : date:CalendarLib.Date.t -> value:float Lazy.t -> t
(** Create a transaction from a date and lazy value. *)

val combine : (float -> float -> float) -> t -> t -> t Seq.t
(** Combine two transactions using a function. If transactions have the same date, returns a
    single-element sequence with the function applied to both values. If dates differ, returns a
    two-element sequence in chronological order, where the function is applied with the transaction
    value and 0.0 (preserving argument order based on which transaction is present). *)

val combine_seq : (float -> float -> float) -> t Seq.t -> t Seq.t -> t Seq.t
(** Combine two sequences of transactions using a function. Merges the sequences by date, applying
    the function to the values from both sequences when dates match, or to the value and 0.0 when
    only one sequence has a transaction on that date. *)

val sum_over : t Seq.t -> start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> float
(** Sum all transaction values between two dates (inclusive of start_date, inclusive of end_date).
*)
