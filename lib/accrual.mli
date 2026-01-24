type split_fn = period:Period.t -> split_date:CalendarLib.Date.t -> value:float -> float * float
(** Function type for splitting an accrual value at a given date within a period. Returns a tuple of
    (value_before, value_after) the split date. *)

type t = { period : Period.t; value : float Lazy.t; split_fn : split_fn }
(** An accrual represents a value spread over a time period with a customizable split function. The
    value is lazily evaluated to allow mutual dependencies between accrual sequences. *)

val default_split_fn : split_fn
(** Default split function that distributes value proportionally based on the number of days before
    and after the split date. *)

val force_value : t -> float
(** Force and return the lazy value of an accrual. *)

val make : period:Period.t -> value:float Lazy.t -> split_fn:split_fn -> t
(** Create an accrual from a period, lazy value, and split function. *)

val make_from_dates :
  start_date:CalendarLib.Date.t ->
  end_date:CalendarLib.Date.t ->
  value:float Lazy.t ->
  split_fn:split_fn ->
  t
(** Create an accrual from start/end dates, lazy value, and split function. *)

val map : (float -> float) -> t -> t
(** Apply a function to transform the accrual's value. The transformation is applied lazily. *)

val split : t -> CalendarLib.Date.t -> t * t
(** Split an accrual at the given date, returning two accruals: one for before and one for after. If
    the split date is outside the accrual's period, a zero-value stub is created for the portion
    outside the period. *)

val clip : t -> start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> t
(** Clip an accrual to the given date range, returning only the portion within the start and end
    dates. *)

val print : t -> unit
(** Print an accrual's period and value to stdout for debugging. *)

val sum_seq : t Seq.t -> t Seq.t -> t Seq.t
(** Sum two sequences of accruals element-wise with automatic period alignment. *)

val sub_seq : t Seq.t -> t Seq.t -> t Seq.t
(** Subtract two sequences of accruals element-wise with automatic period alignment. *)

val sum : t -> t -> t Seq.t
(** Sum two accruals together. Returns a contiguous sequence of one or more accruals that span the
    entire date range. Any gap periods have zero value. Partial period overlap is automatically
    interpolated. *)

val sub : t -> t -> t Seq.t
(** Subtract the second accrual from the first. Returns a contiguous sequence of one or more
    accruals that span the entire date range. Any gap periods have zero value. Partial period
    overlap is automatically interpolated. *)

val const_annual_growth_seq :
  start_date:CalendarLib.Date.t ->
  initial_value:float ->
  rate:float ->
  freq:Period.offset ->
  yf:(CalendarLib.Date.t -> CalendarLib.Date.t -> float) ->
  t Seq.t
(** Create an accrual sequence with values that growth at a constant annual growth rate. *)

val after : CalendarLib.Date.t -> t Seq.t -> t Seq.t
(** Get the accruals in a sequence after a given date. Prepends a zero-value accrual for any stub
    periods from the date to the series start, if applicable. Interpolates a partial stub period if
    the split date falls within a period. *)

val accrue : CalendarLib.Date.t -> CalendarLib.Date.t -> t Seq.t -> float
(** Accumulate accrual between two dates over a sequence of accruals. *)

val accrue_periods : Period.t list -> t Seq.t -> float list
(** Accumulate accrual values for a list of periods in a single pass through the accrual sequence.
    Returns a list of floats corresponding to each input period. Assumes periods are sorted
    chronologically and non-overlapping for optimal performance. *)
