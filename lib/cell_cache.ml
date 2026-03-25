(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

module PeriodHash = Hashtbl.Make (struct
  type t = Period.t

  let equal = Period.equal
  let hash = Period.hash
end)

(** Status of a cell in the evaluation cache. [Resolved] cells have a final value that will not
    change. [Unresolved] cells are part of a dependency cycle and carry the current iteration guess
    and the last iteration number in which they were updated (used as a Gauss-Seidel guard to avoid
    redundant re-evaluation within a single sweep). *)
type cell_status = Resolved of float | Unresolved of float * int

type t = (int, cell_status PeriodHash.t) Hashtbl.t

let create () = Hashtbl.create 16

(** Look up or create the per-cell cache for a given cell ID in the top-level cache. Then look up
    the period in the per-cell cache. Returns [Some (period, status)] on a hit, [None] on a miss
    (after ensuring the per-cell cache exists). *)
let find cache id period =
  let cell_cache =
    match Hashtbl.find_opt cache id with
    | Some cc -> cc
    | None ->
        let cc = PeriodHash.create 12 in
        Hashtbl.replace cache id cc;
        cc
  in
  match PeriodHash.find_opt cell_cache period with
  | Some status -> Some (period, status)
  | None -> None

(** Store a cell status in the cache for a given cell ID and period. *)
let store cache id period status =
  let cell_cache =
    match Hashtbl.find_opt cache id with
    | Some cc -> cc
    | None ->
        let cc = PeriodHash.create 12 in
        Hashtbl.replace cache id cc;
        cc
  in
  PeriodHash.replace cell_cache period status

let max_iterations = 1000
let convergence_threshold = 1e-6
