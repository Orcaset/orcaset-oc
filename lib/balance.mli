(** A point-in-time balance snapshot. *)

type t = { date : CalendarLib.Date.t; value : float Lazy.t }
(** A balance at a specific date. *)

val make : date:CalendarLib.Date.t -> value:float Lazy.t -> t

val to_string : t -> string
(** Convert a balance to a human-readable string representation. *)
