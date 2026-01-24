type t = { date : CalendarLib.Date.t; amount : float Lazy.t }

let make ~date ~amount = { date; amount }

let on seq query_date =
  let rec aux last_amount seq =
    match seq () with
    | Seq.Nil -> { date = query_date; amount = lazy last_amount }
    | Seq.Cons (balance, rest) ->
        let cmp = CalendarLib.Date.compare query_date balance.date in
        if cmp < 0 then { date = query_date; amount = lazy last_amount }
        else if cmp = 0 then balance
        else aux (Lazy.force balance.amount) rest
  in
  aux 0. seq

let combine b1 b2 f =
  let cmp = CalendarLib.Date.compare b1.date b2.date in
  if cmp = 0 then
    Seq.return { date = b1.date; amount = lazy (f (Lazy.force b1.amount) (Lazy.force b2.amount)) }
  else if cmp < 0 then
    let first = { date = b1.date; amount = lazy (f (Lazy.force b1.amount) 0.) } in
    let second =
      { date = b2.date; amount = lazy (f (Lazy.force b1.amount) (Lazy.force b2.amount)) }
    in
    List.to_seq [ first; second ]
  else
    let first = { date = b2.date; amount = lazy (f 0. (Lazy.force b2.amount)) } in
    let second =
      { date = b1.date; amount = lazy (f (Lazy.force b1.amount) (Lazy.force b2.amount)) }
    in
    List.to_seq [ first; second ]

let combine_seq f seq1 seq2 =
  let step (curr1, curr2, s1, s2) =
    match (s1 (), s2 ()) with
    | Seq.Nil, Seq.Nil -> None
    | Seq.Cons (b1, rest1), Seq.Nil ->
        let b1_amount = Lazy.force b1.amount in
        let combined = { date = b1.date; amount = lazy (f b1_amount curr2) } in
        Some (combined, (b1_amount, curr2, rest1, s2))
    | Seq.Nil, Seq.Cons (b2, rest2) ->
        let b2_amount = Lazy.force b2.amount in
        let combined = { date = b2.date; amount = lazy (f curr1 b2_amount) } in
        Some (combined, (curr1, b2_amount, s1, rest2))
    | Seq.Cons (b1, rest1), Seq.Cons (b2, rest2) ->
        let cmp = CalendarLib.Date.compare b1.date b2.date in
        if cmp = 0 then
          let b1_amount = Lazy.force b1.amount in
          let b2_amount = Lazy.force b2.amount in
          let combined = { date = b1.date; amount = lazy (f b1_amount b2_amount) } in
          Some (combined, (b1_amount, b2_amount, rest1, rest2))
        else if cmp < 0 then
          let b1_amount = Lazy.force b1.amount in
          let combined = { date = b1.date; amount = lazy (f b1_amount curr2) } in
          Some (combined, (b1_amount, curr2, rest1, s2))
        else
          let b2_amount = Lazy.force b2.amount in
          let combined = { date = b2.date; amount = lazy (f curr1 b2_amount) } in
          Some (combined, (curr1, b2_amount, s1, rest2))
  in
  Seq.unfold step (0., 0., seq1, seq2)

let to_string b =
  let date_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" b.date in
  Printf.sprintf "Date: %s, Amount: %.2f" date_str (Lazy.force b.amount)

let avg year_frac start_date end_date seq =
  let total_frac = year_frac start_date end_date in
  if total_frac = 0. then 0.
  else
    let rec find_start_balance last_amount seq =
      match seq () with
      | Seq.Nil -> (last_amount, Seq.empty)
      | Seq.Cons (b, rest) ->
          let cmp = CalendarLib.Date.compare start_date b.date in
          if cmp < 0 then (last_amount, Seq.cons b rest)
          else if cmp = 0 then (Lazy.force b.amount, rest)
          else find_start_balance (Lazy.force b.amount) rest
    in
    let initial_amount, remaining_seq = find_start_balance 0. seq in
    let rec accumulate current_date current_amount acc seq =
      if CalendarLib.Date.compare current_date end_date >= 0 then acc
      else
        match seq () with
        | Seq.Nil ->
            let frac = year_frac current_date end_date in
            acc +. (current_amount *. frac)
        | Seq.Cons (b, rest) ->
            if CalendarLib.Date.compare b.date end_date >= 0 then
              let frac = year_frac current_date end_date in
              acc +. (current_amount *. frac)
            else if CalendarLib.Date.compare b.date current_date <= 0 then
              accumulate current_date (Lazy.force b.amount) acc rest
            else
              let frac = year_frac current_date b.date in
              let new_acc = acc +. (current_amount *. frac) in
              accumulate b.date (Lazy.force b.amount) new_acc rest
    in
    let weighted_sum = accumulate start_date initial_amount 0. remaining_seq in
    weighted_sum /. total_frac
