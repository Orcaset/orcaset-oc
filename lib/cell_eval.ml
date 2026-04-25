(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

exception Resolution_failed of { iterations : int; tolerance : float; max_delta : float }

let max_iterations = 1_000
let tolerance = 1e-6

type context = { context_id : int; slots : slot Dynarray.t }
and formula = context -> float
and status = Building | Missing | Ready of formula

and slot = {
  slot_id : int;
  mutable status : status;
  mutable current : float;
  mutable last_delta : float;
  mutable generation : int;
}

let next_context_id = Atomic.make 1
let next_slot_id = Atomic.make 0

let create_context () =
  { context_id = Atomic.fetch_and_add next_context_id 1; slots = Dynarray.create () }

let create_slot () =
  {
    slot_id = Atomic.fetch_and_add next_slot_id 1;
    status = Building;
    current = 0.0;
    last_delta = 0.0;
    generation = 0;
  }

let set_ready slot formula = slot.status <- Ready formula

let set_missing slot =
  slot.status <- Missing;
  slot.current <- 0.0;
  slot.last_delta <- 0.0

let is_missing slot = match slot.status with Missing -> true | Building | Ready _ -> false
let current slot = slot.current

let touch ctx slot =
  if slot.generation <> ctx.context_id then begin
    slot.generation <- ctx.context_id;
    Dynarray.add_last ctx.slots slot
  end

let read ctx slot =
  touch ctx slot;
  match slot.status with Missing -> None | Building | Ready _ -> Some slot.current

let eval_slot ctx slot =
  touch ctx slot;
  match slot.status with
  | Ready formula -> Some (formula ctx)
  | Building -> Some slot.current
  | Missing -> None

let prime ctx =
  let i = ref 0 in
  while !i < Dynarray.length ctx.slots do
    let slot = Dynarray.get ctx.slots !i in
    ignore (eval_slot ctx slot);
    incr i
  done

let solve ctx =
  prime ctx;
  let rec loop iteration =
    if iteration > max_iterations then begin
      let max_delta =
        Dynarray.fold_left (fun acc slot -> Float.max acc slot.last_delta) 0.0 ctx.slots
      in
      raise (Resolution_failed { iterations = max_iterations; tolerance; max_delta })
    end;

    let max_delta = ref 0.0 in
    let i = ref 0 in
    while !i < Dynarray.length ctx.slots do
      let slot = Dynarray.get ctx.slots !i in
      (match slot.status with
      | Ready formula ->
          let old_value = slot.current in
          let new_value = formula ctx in
          let delta = Float.abs (new_value -. old_value) in
          slot.current <- new_value;
          slot.last_delta <- delta;
          max_delta := Float.max !max_delta delta
      | Building -> slot.last_delta <- 0.0
      | Missing -> slot.last_delta <- 0.0);
      incr i
    done;
    if !max_delta < tolerance then () else loop (iteration + 1)
  in
  loop 1
