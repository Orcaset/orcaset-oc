type t =
  | Base of {
      initial_date : CalendarLib.Date.t;
      initial_value : float Lazy.t;
      sum_between : start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> float;
    }
  | Constant of float Lazy.t
  | Combined of { op : float -> float -> float; left : t; right : t }

let from_accruals ~initial_date ~initial_value accrual_seq =
  let sum_between ~start_date ~end_date =
    Accrual.accrue start_date end_date (Lazy.force accrual_seq)
  in
  Base { initial_date; initial_value; sum_between }

let from_transactions ~initial_date ~initial_value txn_seq =
  let sum_between ~start_date ~end_date =
    Transaction.sum_over (Lazy.force txn_seq) ~start_date ~end_date
  in
  Base { initial_date; initial_value; sum_between }

let from_flow ~initial_date ~initial_value ~sum_between =
  Base { initial_date; initial_value; sum_between }

let constant amount = Constant amount

let rec on balance query_date =
  match balance with
  | Constant amount -> Balance.{ date = query_date; value = amount }
  | Base { initial_date; initial_value; sum_between } ->
      if CalendarLib.Date.compare query_date initial_date = 0 then
        Balance.{ date = query_date; value = initial_value }
      else
        let delta = sum_between ~start_date:initial_date ~end_date:query_date in
        Balance.{ date = query_date; value = lazy (Lazy.force initial_value +. delta) }
  | Combined { op; left; right } ->
      let left_balance = on left query_date in
      let right_balance = on right query_date in
      Balance.
        {
          date = query_date;
          value = lazy (op (Lazy.force left_balance.value) (Lazy.force right_balance.value));
        }

let combine op left right = Combined { op; left; right }
let sum a b = combine ( +. ) a b
let sub a b = combine ( -. ) a b
let at_dates balance dates = Seq.map (fun d -> on balance d) dates
let at_periods balance periods = Seq.map (fun p -> on balance p.Period.end_date) periods
