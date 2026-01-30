type split_fn = period:Period.t -> split_date:CalendarLib.Date.t -> value:float -> float * float

type t =
  | Simple of { period : Period.t; value : float Lazy.t; split_fn : split_fn }
  | Combined of { op : float -> float -> float; left : t; right : t }

let rec period = function Simple { period; _ } -> period | Combined { left; _ } -> period left

let rec value = function
  | Simple { value; _ } -> Lazy.force value
  | Combined { op; left; right } -> op (value left) (value right)

let force_value accrual = value accrual
let diff_days d1 d2 = CalendarLib.Date.sub d1 d2 |> CalendarLib.Date.Period.nb_days

let default_split_fn ~period ~split_date ~value =
  let total_days = Period.days period |> float_of_int in
  let days_before = diff_days split_date period.start_date |> float_of_int in
  let ratio_before = days_before /. total_days in
  let value_before = value *. ratio_before in
  let value_after = value -. value_before in
  (value_before, value_after)

let make ~period ~value ~split_fn = Simple { period; value; split_fn }

let make_from_dates ~start_date ~end_date ~value ~split_fn =
  let period = Period.make ~start_date ~end_date in
  Simple { period; value; split_fn }

let map f accrual =
  let p = period accrual in
  Simple { period = p; value = lazy (f (value accrual)); split_fn = default_split_fn }

let rec split accrual split_date =
  match accrual with
  | Simple { period; value; split_fn } ->
      if CalendarLib.Date.compare split_date period.start_date <= 0 then
        let stub_before =
          Simple
            {
              period = Period.make ~start_date:split_date ~end_date:period.start_date;
              value = lazy 0.0;
              split_fn;
            }
        in
        (stub_before, accrual)
      else if CalendarLib.Date.compare split_date period.end_date >= 0 then
        let stub_after =
          Simple
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
        ( Simple { period = period_before; value = value_before; split_fn },
          Simple { period = period_after; value = value_after; split_fn } )
  | Combined { op; left; right } ->
      let left_before, left_after = split left split_date in
      let right_before, right_after = split right split_date in
      ( Combined { op; left = left_before; right = right_before },
        Combined { op; left = left_after; right = right_after } )

let clip accrual ~start_date ~end_date =
  let _, after_start = split accrual start_date in
  let before_end, _ = split after_start end_date in
  before_end

let print accrual =
  Printf.printf "Accrual { period = %s to %s; value = %f }\n"
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" (period accrual).start_date)
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" (period accrual).end_date)
    (value accrual)

let combine op left right =
  let p_left = period left in
  let p_right = period right in
  if not (Period.equal p_left p_right) then failwith "combine: periods must match"
  else Combined { op; left; right }

let rec combine_seq op seq1 seq2 () =
  match (seq1 (), seq2 ()) with
  | Seq.Nil, Seq.Nil -> Seq.Nil
  | Seq.Nil, node | node, Seq.Nil -> node
  | Seq.Cons (acc1, rest1), Seq.Cons (acc2, rest2) ->
      let p1 = period acc1 in
      let p2 = period acc2 in
      if Period.equal p1 p2 then Seq.Cons (combine op acc1 acc2, combine_seq op rest1 rest2)
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
        Seq.Cons (combine op acc1 acc2_before, combine_seq op rest1 (Seq.cons acc2_after rest2))
      else
        let acc1_before, acc1_after = split acc1 p2.end_date in
        Seq.Cons (combine op acc1_before acc2, combine_seq op (Seq.cons acc1_after rest1) rest2)

let sum_seq = combine_seq ( +. )
let sub_seq = combine_seq ( -. )
let sum acc1 acc2 = sum_seq (Seq.return acc1) (Seq.return acc2)
let sub acc1 acc2 = sub_seq (Seq.return acc1) (Seq.return acc2)

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
      let next_period = Period.add_offset freq (period prior_accrual) in
      let growth_factor = 1. +. (rate *. yf next_period.start_date next_period.end_date) in
      let next_value = value prior_accrual *. growth_factor in
      make ~period:next_period ~value:(lazy next_value) ~split_fn:default_split_fn)
    initial_accrual

let after date seq =
  let rec aux seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (accrual, rest) when CalendarLib.Date.equal (period accrual).start_date date ->
        Seq.Cons (accrual, rest)
    | Seq.Cons (accrual, rest) when CalendarLib.Date.compare (period accrual).end_date date <= 0 ->
        aux rest ()
    | Seq.Cons (accrual, rest) when CalendarLib.Date.compare (period accrual).start_date date < 0 ->
        let _, after_split = split accrual date in
        Seq.Cons (after_split, rest)
    | Seq.Cons (accrual, rest) ->
        let stub_period = Period.make ~start_date:date ~end_date:(period accrual).start_date in
        let stub_accrual = make ~period:stub_period ~value:(lazy 0.0) ~split_fn:default_split_fn in
        Seq.Cons (stub_accrual, fun () -> Seq.Cons (accrual, rest))
  in
  aux seq

let accrue start_date end_date seq =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> acc
    | Seq.Cons (accrual, _) when CalendarLib.Date.compare (period accrual).start_date end_date >= 0
      ->
        acc
    | Seq.Cons (accrual, rest) ->
        let clipped = clip accrual ~start_date ~end_date in
        let new_acc = acc +. value clipped in
        aux new_acc rest
  in
  aux 0.0 seq

let accrue_periods periods accrual_seq =
  let rec aux current_seq periods acc =
    match periods with
    | [] -> List.rev acc
    | p :: rest_periods ->
        let start_date = p.Period.start_date in
        let end_date = p.Period.end_date in
        (* Skip accruals that end before the current period starts *)
        let relevant_seq =
          Seq.drop_while
            (fun accrual -> CalendarLib.Date.compare (period accrual).end_date start_date <= 0)
            current_seq
        in
        (* Accumulate value for this period *)
        let rec accum acc_value seq =
          match seq () with
          | Seq.Nil -> (acc_value, Seq.empty)
          | Seq.Cons (accrual, _)
            when CalendarLib.Date.compare (period accrual).start_date end_date >= 0 ->
              (acc_value, seq)
          | Seq.Cons (accrual, rest) ->
              let clipped = clip accrual ~start_date ~end_date in
              let new_value = acc_value +. value clipped in
              (* If accrual extends past this period, keep it for the next period *)
              if CalendarLib.Date.compare (period accrual).end_date end_date > 0 then
                (new_value, seq)
              else accum new_value rest
        in
        let period_value, next_seq = accum 0.0 relevant_seq in
        aux next_seq rest_periods (period_value :: acc)
  in
  aux accrual_seq periods []
