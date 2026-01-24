type t = { date : CalendarLib.Date.t; value : float Lazy.t }

let make ~date ~value = { date; value }

let combine f txn1 txn2 =
  let cmp = CalendarLib.Date.compare txn1.date txn2.date in
  if cmp = 0 then
    Seq.return
      { date = txn1.date; value = lazy (f (Lazy.force txn1.value) (Lazy.force txn2.value)) }
  else if cmp < 0 then
    let first = { date = txn1.date; value = lazy (f (Lazy.force txn1.value) 0.0) } in
    let second = { date = txn2.date; value = lazy (f 0.0 (Lazy.force txn2.value)) } in
    List.to_seq [ first; second ]
  else
    let first = { date = txn2.date; value = lazy (f 0.0 (Lazy.force txn2.value)) } in
    let second = { date = txn1.date; value = lazy (f (Lazy.force txn1.value) 0.0) } in
    List.to_seq [ first; second ]

let combine_seq f seq1 seq2 =
  let step (s1, s2) =
    match (s1 (), s2 ()) with
    | Seq.Nil, Seq.Nil -> None
    | Seq.Cons (txn1, rest1), Seq.Nil ->
        let combined = { date = txn1.date; value = lazy (f (Lazy.force txn1.value) 0.0) } in
        Some (combined, (rest1, s2))
    | Seq.Nil, Seq.Cons (txn2, rest2) ->
        let combined = { date = txn2.date; value = lazy (f 0.0 (Lazy.force txn2.value)) } in
        Some (combined, (s1, rest2))
    | Seq.Cons (txn1, rest1), Seq.Cons (txn2, rest2) ->
        let cmp = CalendarLib.Date.compare txn1.date txn2.date in
        if cmp = 0 then
          let combined =
            { date = txn1.date; value = lazy (f (Lazy.force txn1.value) (Lazy.force txn2.value)) }
          in
          Some (combined, (rest1, rest2))
        else if cmp < 0 then
          let combined = { date = txn1.date; value = lazy (f (Lazy.force txn1.value) 0.0) } in
          Some (combined, (rest1, fun () -> Seq.Cons (txn2, rest2)))
        else
          let combined = { date = txn2.date; value = lazy (f 0.0 (Lazy.force txn2.value)) } in
          Some (combined, ((fun () -> Seq.Cons (txn1, rest1)), rest2))
  in
  Seq.unfold step (seq1, seq2)

let sum_over seq ~start_date ~end_date =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> acc
    | Seq.Cons (txn, rest) ->
        if CalendarLib.Date.compare txn.date end_date > 0 then acc
        else if CalendarLib.Date.compare txn.date start_date >= 0 then
          aux (acc +. Lazy.force txn.value) rest
        else aux acc rest
  in
  aux 0.0 seq
