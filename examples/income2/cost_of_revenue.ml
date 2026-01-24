module Accrual = Orcaset.Accrual

type t = { recurring : Accrual.t Seq.t; non_recurring : Accrual.t Seq.t; total : Accrual.t Seq.t }

let make ~revenue ~recurring_pct ~non_recurring_pct =
  let recurring =
    Seq.map (fun rev -> Accrual.map (fun v -> v *. recurring_pct) rev) revenue.Revenue.recurring
  in
  let non_recurring =
    Seq.map
      (fun rev -> Accrual.map (fun v -> v *. non_recurring_pct) rev)
      revenue.Revenue.non_recurring
  in

  let total = Accrual.sum_seq recurring non_recurring in
  { recurring; non_recurring; total }
