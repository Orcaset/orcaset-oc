type point = { date : CalendarLib.Date.t; amount : float Lazy.t }

type t =
  | Base of {
      initial_date : CalendarLib.Date.t;
      initial_amount : float Lazy.t;
      sum_between : start_date:CalendarLib.Date.t -> end_date:CalendarLib.Date.t -> float;
    }
  | Constant of float Lazy.t
  | Combined of { op : float -> float -> float; left : t; right : t }

let from_accruals ~initial_date ~initial_amount accrual_seq =
  let sum_between ~start_date ~end_date =
    Accrual.accrue start_date end_date (Lazy.force accrual_seq)
  in
  Base { initial_date; initial_amount; sum_between }

let from_transactions ~initial_date ~initial_amount txn_seq =
  let sum_between ~start_date ~end_date =
    Transaction.sum_over (Lazy.force txn_seq) ~start_date ~end_date
  in
  Base { initial_date; initial_amount; sum_between }

let from_flow ~initial_date ~initial_amount ~sum_between =
  Base { initial_date; initial_amount; sum_between }

let constant amount = Constant amount

let rec on balance query_date =
  match balance with
  | Constant amount -> { date = query_date; amount }
  | Base { initial_date; initial_amount; sum_between } ->
      let delta = sum_between ~start_date:initial_date ~end_date:query_date in
      { date = query_date; amount = lazy (Lazy.force initial_amount +. delta) }
  | Combined { op; left; right } ->
      let left_pt = on left query_date in
      let right_pt = on right query_date in
      {
        date = query_date;
        amount = lazy (op (Lazy.force left_pt.amount) (Lazy.force right_pt.amount));
      }

let combine op left right = Combined { op; left; right }
let sum a b = combine ( +. ) a b
let sub a b = combine ( -. ) a b
let at_dates balance dates = Seq.map (fun d -> on balance d) dates
let at_periods balance periods = Seq.map (fun p -> on balance p.Period.end_date) periods

let point_to_string pt =
  let date_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" pt.date in
  Printf.sprintf "Date: %s, Amount: %.2f" date_str (Lazy.force pt.amount)
