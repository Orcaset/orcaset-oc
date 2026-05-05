(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type evaluated =
  | Span_values of (Period.t * float option) list
  | Point_values of (Date.t * float option) list

type stmt

type resolved =
  | RGroup of resolved list
  | RTotal of { label : string option; values : evaluated option; children : resolved list }
  | RLine of { label : string option; values : evaluated option }

(* Statement constructors *)
val group : stmt list -> stmt
(** [group children] groups [children] into a single statement. *)

val span_line : Series.Spans.t -> stmt
(** [span_line series] creates a line statement with a span series. *)

val span_lines : Series.Spans.t list -> stmt list
(** [span_lines series] converts each span series in [series] to a line statement. *)

val span_total : Series.Spans.t -> stmt list -> stmt
(** [span_total total children] creates a total statement with a span total and statement children.
*)

val point_line : Series.Points.t -> stmt
(** [point_line series] creates a line statement with a point series. *)

val point_lines : Series.Points.t list -> stmt list
(** [point_lines series] converts each point series in [series] to a line statement. *)

val point_total : Series.Points.t -> stmt list -> stmt
(** [point_total total children] creates a total statement with a point total and statement
    children. *)

(* Evaluators *)

val eval_periods : Period.t list -> stmt -> resolved
(** [eval_periods periods stmt] evaluates [stmt] for each period in [periods] and returns a
    [resolved] statement. Point types are resolved to point values at the set of all unique period
    start and end dates. Every line and total has labels copied from the source series and
    [values = Some _]. *)

val eval_dates : Date.t list -> stmt -> resolved
(** [eval_dates dates stmt] evaluates [stmt] for each date in [dates] and returns a [resolved]
    statement. Point types are resolved to [values = Some _]. Span types are resolved to
    [values = None] because date-only span evaluation is not applicable, but labels are still copied
    from the source series. *)

(* Formatters *)

val fixed_width : resolved -> string
(** [fixed_width resolved] renders [resolved] as a fixed-width table. Dates are rendered as
    ["YYYY-MM-DD"] column headers; span values use period end dates and point values use their point
    dates. *)
