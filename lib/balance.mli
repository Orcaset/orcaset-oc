type t = { date : CalendarLib.Date.t; amount : float Lazy.t }
(** A balance represents a value snapshot at a point in time. The value is considered current until
    the next balance in a sequence. The amount is lazy to support circular dependencies. *)

val make : date:CalendarLib.Date.t -> amount:float Lazy.t -> t
(** Create a balance from a date and lazy amount. *)

val on : t Seq.t -> CalendarLib.Date.t -> t
(** Get the balance value on a specific date. If the date is before the sequence begins, returns a
    zero-value balance. If the date falls between balances, returns the most recent balance value
    with the query date. If the date is after all balances, returns the last known value. *)

val combine : t -> t -> (float -> float -> float) -> t Seq.t
(** Combine two balances using a function. If the balances have the same date, returns a
    single-element sequence with the combined amount. If dates differ, returns a two-element
    sequence: the earlier date with the function applied to the earlier amount and zero (preserving
    argument order), then the later date with the function applied to both amounts. *)

val combine_seq : (float -> float -> float) -> t Seq.t -> t Seq.t -> t Seq.t
(** Combine two sequences of balances using a function. Merges the sequences by date, applying the
    function to the current balance from each sequence at each date where either sequence changes.
    Before a sequence's first balance, its value is treated as 0.0. *)

val to_string : t -> string
(** Convert a balance to a human-readable string representation including date and amount. *)

val avg :
  (CalendarLib.Date.t -> CalendarLib.Date.t -> float) ->
  CalendarLib.Date.t ->
  CalendarLib.Date.t ->
  t Seq.t ->
  float
(** Compute the weighted average balance over a date range. Takes a year fraction function for
    computing weights, a start date, an end date, and a balance sequence. Each sub-period is
    weighted by its year fraction. Returns 0 if the total year fraction is zero. *)
