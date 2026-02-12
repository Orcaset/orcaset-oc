type t = { start_date : CalendarLib.Date.t; end_date : CalendarLib.Date.t }

type offset = {
  days : int;
  weeks : int;
  months : int;
  quarters : int;
  years : int;
  month_end : bool;
}

val make : start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> t

val make_offset :
  ?days:int ->
  ?weeks:int ->
  ?months:int ->
  ?quarters:int ->
  ?years:int ->
  ?month_end:bool ->
  unit ->
  offset

val days : t -> int
(** Number of days in the period (end_date - start_date). *)

val contains : t -> CalendarLib.Date.t -> bool
(** [contains period date] returns [true] if [date] falls within [period.start_date] and
    [period.end_date] (inclusive). *)

val add_offset_to_date : offset -> CalendarLib.Date.t -> CalendarLib.Date.t
(** Apply an [offset] to a single date. *)

val add_offset : offset -> t -> t
(** Shift both endpoints of a period by [offset]. *)

val equal : t -> t -> bool
val print : t -> unit

val make_seq : start_date:CalendarLib.Date.t -> offset:offset -> t Seq.t
(** Create an infinite sequence of periods starting from [start_date], each offset by [offset]. *)
