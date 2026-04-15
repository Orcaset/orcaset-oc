open Orcaset

type t
(** A parsed CSV with period-start/period-end header rows and named data rows. *)

val of_file : string -> t
(** [of_file path] reads and parses a CSV file. The first row must be [period-start,...] and the
    second [period-end,...]. *)

val periods : t -> Period.t list
(** The periods defined by the header rows. *)

val find : t -> string -> float list
(** [find t name] returns the values for the row named [name].
    @raise Not_found if no such row exists. *)

val cells : t -> string -> [ `USD ] Period_cell.t list
(** [cells t name] builds period cells for the named row. *)

(** {1 Quarterly-data CSVs} *)

type quarterly_csv
(** A parsed CSV with a single header row of quarter-end dates and named data rows. *)

val quarterly_of_file : string -> quarterly_csv
(** [quarterly_of_file path] reads a CSV where the first row is [Metric,date1,date2,...]. Periods
    are inferred as quarterly intervals ending on each date. *)

val quarterly_periods : quarterly_csv -> Period.t list
(** The quarterly periods inferred from the header dates. *)

val quarterly_find : quarterly_csv -> string -> float option list
(** [quarterly_find t name] returns the optional values for the named row.
    @raise Not_found if no such row exists. *)

val quarterly_cells : quarterly_csv -> string -> [ `USD ] Period_cell.t list
(** [quarterly_cells t name] builds period cells for the named row. Missing values become 0.0. *)

(** {1 Point-data CSVs} *)

type point_csv
(** A parsed CSV with a single [date] header row and named data rows. *)

val point_of_file : string -> point_csv
(** [point_of_file path] reads and parses a point-data CSV file. The first row must be
    [date,d1,d2,...]. *)

val dates : point_csv -> Date.t list
(** The dates defined by the header row. *)

val point_find : point_csv -> string -> float option list
(** [point_find t name] returns the values for the row named [name].
    @raise Not_found if no such row exists. *)

val point_find_nth : point_csv -> string -> int -> float option list
(** [point_find_nth t name n] returns the values for the [n]-th (0-indexed) row named [name]. Useful
    when a CSV has duplicate row names. *)

val points : point_csv -> string -> (Date.t * float) list
(** [points t name] returns [(date, value)] pairs for the named row, skipping missing values. *)

val points_nth : point_csv -> string -> int -> (Date.t * float) list
(** [points_nth t name n] returns [(date, value)] pairs for the [n]-th occurrence of [name],
    skipping missing values. *)
