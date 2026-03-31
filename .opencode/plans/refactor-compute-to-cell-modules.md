# Refactor compute_period / compute_point into their respective cell modules

## Goal

Move `compute_period` from `eval.ml` into `Period_cell` and `compute_point` into `Point_cell`, using an `eval_cell` callback parameter to break the mutual recursion.

## Changes

### 1. `lib/cell_types.ml` and `lib/cell_types.mli` — Add `eval_fn` type alias

Add after the `cell` type definition:

```ocaml
type eval_fn = Cell_cache.t -> int -> cell -> float * float
```

This gives a clean name to the callback signature used by both cell modules.

### 2. `lib/period_cell.ml` — Add `compute` function

Add at the end of the file (after the `dependency_tree` function):

```ocaml
let compute (eval_cell : Cell_types.eval_fn) cache iteration = function
  | RConst { f; _ } -> (f (), 0.0)
  | RDeps { deps; f; _ } ->
      let values, max_delta =
        List.fold_left
          (fun (vals, acc_delta) dep ->
            let v, d = eval_cell cache iteration (PeriodCell dep) in
            (v :: vals, Float.max acc_delta d))
          ([], 0.0) deps
      in
      (f (List.rev values), max_delta)
  | RMap { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PeriodCell inner) in
      (f v, d)
  | RConvert { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PeriodCell inner) in
      (f (period inner) v, d)
  | RMap2 { c1; c2; f; _ } ->
      let v1, d1 =
        match c1 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PeriodCell c) in
            (Some v, d)
      in
      let v2, d2 =
        match c2 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PeriodCell c) in
            (Some v, d)
      in
      (f v1 v2, Float.max d1 d2)
  | RRef { cell = Some c; _ } -> eval_cell cache iteration (PeriodCell c)
  | RRef { cell = None; _ } -> (0.0, 0.0)
```

### 3. `lib/period_cell.mli` — Expose `compute`

Add under the `{1 Evaluation}` section (or add such a section):

```ocaml
(** {1 Evaluation} *)

val compute : Cell_types.eval_fn -> Cell_cache.t -> int -> 'c t -> float * float
(** [compute eval_cell cache iteration cell] computes the value of [cell] for a single evaluation
    step. [eval_cell] is used to recursively evaluate dependencies. *)
```

### 4. `lib/point_cell.ml` — Add `compute` function

Add at the end of the file:

```ocaml
let compute (eval_cell : Cell_types.eval_fn) cache iteration = function
  | TConst { value; _ } -> (value, 0.0)
  | TMap { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PointCell inner) in
      (f v, d)
  | TConvert { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PointCell inner) in
      (f (date inner) v, d)
  | TDep2 { c1; c2; f; _ } ->
      let v1, d1 =
        match c1 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PointCell c) in
            (Some v, d)
      in
      let v2, d2 =
        match c2 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PointCell c) in
            (Some v, d)
      in
      (f v1 v2, Float.max d1 d2)
```

### 5. `lib/point_cell.mli` — Expose `compute`

Replace the existing evaluation comment section with:

```ocaml
(** {1 Evaluation} *)

val compute : Cell_types.eval_fn -> Cell_cache.t -> int -> 'c t -> float * float
(** [compute eval_cell cache iteration cell] computes the value of [cell] for a single evaluation
    step. [eval_cell] is used to recursively evaluate dependencies. *)
```

### 6. `lib/eval.ml` — Remove `compute_period`, `compute_point`, simplify `compute_value`

Remove the `compute_period` and `compute_point` functions entirely (lines 76-135). Replace `compute_value` with:

```ocaml
and compute_value cache iteration = function
  | PeriodCell pc -> Period_cell.compute eval_cell cache iteration pc
  | PointCell tc -> Point_cell.compute eval_cell cache iteration tc
```

The `eval_cell` / `compute_value` pair stays `rec`/`and`-bound, but `compute_value` now just dispatches.

## Verification

Run `dune build` to confirm compilation succeeds, then run the existing test suite.
