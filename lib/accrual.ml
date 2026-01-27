type split_fn = period:Period.t -> split_date:CalendarLib.Date.t -> value:float -> float * float
(** Function type for splitting an accrual value at a given date within a period. Returns a tuple of
    (value_before, value_after) the split date. *)

type t = { period : Period.t; value : float Lazy.t; split_fn : split_fn }
(** An accrual represents a value spread over a time period with a customizable split function. The
    value is lazily evaluated to allow mutual dependencies between accrual sequences. *)

(** Force and return the lazy value of an accrual. *)
let force_value accrual = Lazy.force accrual.value

(** Helper function to compute days difference between two dates *)
let diff_days d1 d2 = CalendarLib.Date.sub d1 d2 |> CalendarLib.Date.Period.nb_days

(** Default split function that distributes value proportionally based on the number of days before
    and after the split date. *)
let default_split_fn ~period ~split_date ~value =
  let total_days = Period.days period |> float_of_int in
  let days_before = diff_days split_date period.start_date |> float_of_int in
  let ratio_before = days_before /. total_days in
  let value_before = value *. ratio_before in
  let value_after = value -. value_before in
  (value_before, value_after)

(** Create an accrual from a period, lazy value, and split function. *)
let make ~period ~value ~split_fn = { period; value; split_fn }

(** Create an accrual from start/end dates, lazy value, and split function. *)
let make_from_dates ~start_date ~end_date ~value ~split_fn =
  let period = Period.make ~start_date ~end_date in
  { period; value; split_fn }

(** Apply a function to transform the accrual's value. The transformation is applied lazily. *)
let map f accrual =
  let { period; value; split_fn } = accrual in
  { period; value = lazy (f (Lazy.force value)); split_fn }

(** Split an accrual at the given date, returning two accruals: one for before and one for after. If
    the split date is outside the accrual's period, a zero-value stub is created for the portion
    outside the period. The split values are computed lazily. *)
let split accrual split_date =
  let { period; value; split_fn } = accrual in
  if CalendarLib.Date.compare split_date period.start_date <= 0 then
    let stub_before =
      {
        period = Period.make ~start_date:split_date ~end_date:period.start_date;
        value = lazy 0.0;
        split_fn;
      }
    in
    (stub_before, accrual)
  else if CalendarLib.Date.compare split_date period.end_date >= 0 then
    let stub_after =
      {
        period = Period.make ~start_date:period.end_date ~end_date:split_date;
        value = lazy 0.0;
        split_fn;
      }
    in
    (accrual, stub_after)
  else
    let period_before = Period.make ~start_date:period.start_date ~end_date:split_date in
    let period_after = Period.make ~start_date:split_date ~end_date:period.end_date in
    let value_before = lazy (fst (split_fn ~period ~split_date ~value:(Lazy.force value))) in
    let value_after = lazy (snd (split_fn ~period ~split_date ~value:(Lazy.force value))) in
    ( { period = period_before; value = value_before; split_fn },
      { period = period_after; value = value_after; split_fn } )

(** Clip an accrual to the given date range, returning only the portion within the start and end
    dates. *)
let clip accrual ~start_date ~end_date =
  let _, after_start = split accrual start_date in
  let before_end, _ = split after_start end_date in
  before_end

(** Print an accrual's period and value to stdout for debugging. Forces the lazy value. *)
let print { period; value; _ } =
  Printf.printf "Accrual { period = %s to %s; value = %f }\n"
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" period.start_date)
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" period.end_date)
    (Lazy.force value)

(* Private helper functions for combining accruals with matching periods. Caller's responsibility to ensure periods match. *)
let combine_matching_accruals op acc1 acc2 =
  if Period.equal acc1.period acc2.period then
    let split_fn ~period ~split_date ~value:_ =
      let v1_before, v1_after = acc1.split_fn ~period ~split_date ~value:(Lazy.force acc1.value) in
      let v2_before, v2_after = acc2.split_fn ~period ~split_date ~value:(Lazy.force acc2.value) in
      (op v1_before v2_before, op v1_after v2_after)
    in
    let combined_value = lazy (op (Lazy.force acc1.value) (Lazy.force acc2.value)) in
    make ~period:acc1.period ~value:combined_value ~split_fn
  (* This should never be reached. If the caller passes non-equal periods something went wrong. *)
    else failwith "combine_matching_accruals called with non-equal periods"

(** Private helper function for combining sequences of accruals. *)
let rec combine_seq op seq1 seq2 () =
  match (seq1 (), seq2 ()) with
  | Seq.Nil, Seq.Nil -> Seq.Nil
  | Seq.Nil, node | node, Seq.Nil -> node
  | Seq.Cons (acc1, rest1), Seq.Cons (acc2, rest2) ->
      let p1 = acc1.period in
      let p2 = acc2.period in
      if Period.equal p1 p2 then
        Seq.Cons (combine_matching_accruals op acc1 acc2, combine_seq op rest1 rest2)
      else if CalendarLib.Date.compare p1.end_date p2.start_date <= 0 then
        Seq.Cons (acc1, combine_seq op rest1 (fun () -> Seq.Cons (acc2, rest2)))
      else if CalendarLib.Date.compare p2.end_date p1.start_date <= 0 then
        Seq.Cons (acc2, combine_seq op (fun () -> Seq.Cons (acc1, rest1)) rest2)
      else if CalendarLib.Date.compare p1.start_date p2.start_date < 0 then
        let acc1_before, acc1_after = split acc1 p2.start_date in
        Seq.Cons
          ( acc1_before,
            combine_seq op (Seq.cons acc1_after rest1) (fun () -> Seq.Cons (acc2, rest2)) )
      else if CalendarLib.Date.compare p2.start_date p1.start_date < 0 then
        let acc2_before, acc2_after = split acc2 p1.start_date in
        Seq.Cons
          ( acc2_before,
            combine_seq op (fun () -> Seq.Cons (acc1, rest1)) (Seq.cons acc2_after rest2) )
      else if CalendarLib.Date.compare p1.end_date p2.end_date < 0 then
        let acc2_before, acc2_after = split acc2 p1.end_date in
        Seq.Cons
          ( combine_matching_accruals op acc1 acc2_before,
            combine_seq op rest1 (Seq.cons acc2_after rest2) )
      else
        let acc1_before, acc1_after = split acc1 p2.end_date in
        Seq.Cons
          ( combine_matching_accruals op acc1_before acc2,
            combine_seq op (Seq.cons acc1_after rest1) rest2 )

(** Sum two sequences of accruals element-wise with automatic period alignment. *)
let sum_seq = combine_seq ( +. )

(** Subtract two sequences of accruals element-wise with automatic period alignment. *)
let sub_seq = combine_seq ( -. )

(** Sum two accruals together. Returns a contiguous sequence of one or more accruals that span the
    entire date range. Any gap periods have zero value. Partial period overlap is automatically
    interpolated. *)
let sum acc1 acc2 = sum_seq (Seq.return acc1) (Seq.return acc2)

(** Subtract the second accrual from the first. Returns a contiguous sequence of one or more
    accruals that span the entire date range. Any gap periods have zero value. Partial period
    overlap is automatically interpolated. *)
let sub acc1 acc2 = sub_seq (Seq.return acc1) (Seq.return acc2)

(** Create an accrual sequence with values that growth at a constant annual growth rate. *)
let const_annual_growth_seq ~start_date ~initial_value ~rate ~(freq : Period.offset)
    ~(yf : CalendarLib.Date.t -> CalendarLib.Date.t -> float) =
  let total_days = freq.days + (freq.weeks * 7) in
  let total_months = freq.months + (freq.quarters * 3) + (freq.years * 12) in

  let initial_end_date =
    (* Use clamped month addition for initial period *)
    let date_with_months = Period.add_months_clamped start_date total_months in
    let date_with_days =
      if total_days = 0 then date_with_months
      else CalendarLib.Date.add date_with_months (CalendarLib.Date.Period.day total_days)
    in
    if freq.month_end then Period.last_day_of_month date_with_days else date_with_days
  in
  let initial_accrual =
    let period = Period.make ~start_date ~end_date:initial_end_date in
    make ~period ~value:(lazy initial_value) ~split_fn:default_split_fn
  in
  Seq.iterate
    (fun prior_accrual ->
      let next_period = Period.add_offset freq prior_accrual.period in
      let growth_factor = 1. +. (rate *. yf next_period.start_date next_period.end_date) in
      let next_value = Lazy.force prior_accrual.value *. growth_factor in
      make ~period:next_period ~value:(lazy next_value) ~split_fn:default_split_fn)
    initial_accrual

(** Get the accruals in a sequence after a given date. Prepends a zero-value accrual for any stub
    periods from the date to the series start, if applicable. Interpolates a partial stub period if
    the split date falls within a period. *)
let after date seq =
  let rec aux seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (accrual, rest) when CalendarLib.Date.equal accrual.period.start_date date ->
        Seq.Cons (accrual, rest)
    | Seq.Cons (accrual, rest) when CalendarLib.Date.compare accrual.period.end_date date <= 0 ->
        aux rest ()
    | Seq.Cons (accrual, rest) when CalendarLib.Date.compare accrual.period.start_date date < 0 ->
        let _, after_split = split accrual date in
        Seq.Cons (after_split, rest)
    | Seq.Cons (accrual, rest) ->
        let stub_period = Period.make ~start_date:date ~end_date:accrual.period.start_date in
        let stub_accrual = make ~period:stub_period ~value:(lazy 0.0) ~split_fn:default_split_fn in
        Seq.Cons (stub_accrual, fun () -> Seq.Cons (accrual, rest))
  in
  aux seq

(** Accumulate accrual between two dates over a sequence of accruals. Forces lazy values. *)
let accrue start_date end_date seq =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> acc
    | Seq.Cons (accrual, _) when CalendarLib.Date.compare accrual.period.start_date end_date >= 0 ->
        acc
    | Seq.Cons (accrual, rest) ->
        let clipped = clip accrual ~start_date ~end_date in
        let new_acc = acc +. Lazy.force clipped.value in
        aux new_acc rest
  in
  aux 0.0 seq

(** Accumulate accrual values for a list of periods in a single pass through the accrual sequence.
    Returns a list of floats corresponding to each input period. Assumes periods are sorted
    chronologically and non-overlapping for optimal performance. Forces lazy values. *)
let accrue_periods periods accrual_seq =
  let rec aux current_seq periods acc =
    match periods with
    | [] -> List.rev acc
    | period :: rest_periods ->
        let start_date = period.Period.start_date in
        let end_date = period.Period.end_date in
        (* Skip accruals that end before the current period starts *)
        let rec skip seq =
          match seq () with
          | Seq.Nil -> Seq.empty
          | Seq.Cons (accrual, rest)
            when CalendarLib.Date.compare accrual.period.end_date start_date <= 0 ->
              skip rest
          | node -> fun () -> node
        in
        let relevant_seq = skip current_seq in
        (* Accumulate value for this period *)
        let rec accum value seq =
          match seq () with
          | Seq.Nil -> (value, Seq.empty)
          | Seq.Cons (accrual, _)
            when CalendarLib.Date.compare accrual.period.start_date end_date >= 0 ->
              (value, seq)
          | Seq.Cons (accrual, rest) ->
              let clipped = clip accrual ~start_date ~end_date in
              accum (value +. Lazy.force clipped.value) rest
        in
        let period_value, next_seq = accum 0.0 relevant_seq in
        aux next_seq rest_periods (period_value :: acc)
  in
  aux accrual_seq periods []
