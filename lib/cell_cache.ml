(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

module PeriodHash = Hashtbl.Make (struct
  type t = Period.t

  let equal = Period.equal
  let hash = Period.hash
end)

module DateHash = Hashtbl.Make (struct
  type t = Date.t

  let equal = Date.equal
  let hash = Date.hash
end)

(** Status of a cell in the evaluation cache. [Resolved] cells have a final value that will not
    change. [Unresolved] cells are part of a dependency cycle and carry the current iteration guess
    and the last iteration number in which they were updated (used as a Gauss-Seidel guard to avoid
    redundant re-evaluation within a single sweep). *)
type cell_status = Resolved of float | Unresolved of float * int

type t = {
  period : (int, cell_status PeriodHash.t) Hashtbl.t;
  point : (int, cell_status DateHash.t) Hashtbl.t;
}

let create () = { period = Hashtbl.create 16; point = Hashtbl.create 16 }

(** Look up or create the per-cell cache for a given cell ID in the period cache. Then look up
    the period in the per-cell cache. Returns [Some (period, status)] on a hit, [None] on a miss
    (after ensuring the per-cell cache exists). *)
let find_period cache id period =
  let cell_cache =
    match Hashtbl.find_opt cache.period id with
    | Some cc -> cc
    | None ->
        let cc = PeriodHash.create 12 in
        Hashtbl.replace cache.period id cc;
        cc
  in
  match PeriodHash.find_opt cell_cache period with
  | Some status -> Some (period, status)
  | None -> None

(** Store a cell status in the period cache for a given cell ID and period. *)
let store_period cache id period status =
  let cell_cache =
    match Hashtbl.find_opt cache.period id with
    | Some cc -> cc
    | None ->
        let cc = PeriodHash.create 12 in
        Hashtbl.replace cache.period id cc;
        cc
  in
  PeriodHash.replace cell_cache period status

(** Look up a point cell status by cell ID and date. Returns [Some status] on a hit, [None] on a
    miss. *)
let find_point cache id date =
  let cell_cache =
    match Hashtbl.find_opt cache.point id with
    | Some cc -> cc
    | None ->
        let cc = DateHash.create 12 in
        Hashtbl.replace cache.point id cc;
        cc
  in
  DateHash.find_opt cell_cache date

(** Store a point cell status in the cache for a given cell ID and date. *)
let store_point cache id date status =
  let cell_cache =
    match Hashtbl.find_opt cache.point id with
    | Some cc -> cc
    | None ->
        let cc = DateHash.create 12 in
        Hashtbl.replace cache.point id cc;
        cc
  in
  DateHash.replace cell_cache date status

let max_iterations = 1000
let convergence_threshold = 1e-6
