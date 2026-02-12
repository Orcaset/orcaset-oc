type t = { start_date : CalendarLib.Date.t; end_date : CalendarLib.Date.t }

type offset = {
  days : int;
  weeks : int;
  months : int;
  quarters : int;
  years : int;
  month_end : bool;
}

let last_day_of_month date =
  let year = CalendarLib.Date.year date in
  let month = CalendarLib.Date.month date in
  CalendarLib.Date.days_in_month date
  |> CalendarLib.Date.make year (CalendarLib.Date.int_of_month month)

let add_months_clamped date total_months =
  if total_months = 0 then date
  else
    let orig_day = CalendarLib.Date.day_of_month date in
    let year = CalendarLib.Date.year date in
    let month_int = CalendarLib.Date.int_of_month (CalendarLib.Date.month date) in
    (* Calculate target year and month *)
    let total_month_index = (year * 12) + month_int - 1 + total_months in
    let target_year = total_month_index / 12 in
    let target_month_int = (total_month_index mod 12) + 1 in
    (* Get days in target month *)
    let temp_date = CalendarLib.Date.make target_year target_month_int 1 in
    let days_in_target = CalendarLib.Date.days_in_month temp_date in
    (* Clamp to the last day of the target month if necessary *)
    let clamped_day = min orig_day days_in_target in
    CalendarLib.Date.make target_year target_month_int clamped_day

let make ~start_date ~end_date = { start_date; end_date }

let make_offset ?(days = 0) ?(weeks = 0) ?(months = 0) ?(quarters = 0) ?(years = 0)
    ?(month_end = false) () =
  { days; weeks; months; quarters; years; month_end }

let days { start_date; end_date } =
  CalendarLib.Date.sub end_date start_date |> CalendarLib.Date.Period.nb_days

let contains period date =
  CalendarLib.Date.compare date period.start_date >= 0
  && CalendarLib.Date.compare date period.end_date <= 0

let add_offset_to_date offset date =
  let total_days = offset.days + (offset.weeks * 7) in
  let total_months = offset.months + (offset.quarters * 3) + (offset.years * 12) in
  let date_with_months = add_months_clamped date total_months in
  let date_with_days =
    if total_days = 0 then date_with_months
    else CalendarLib.Date.add date_with_months (CalendarLib.Date.Period.day total_days)
  in
  if offset.month_end then last_day_of_month date_with_days else date_with_days

let add_offset offset period =
  {
    start_date = add_offset_to_date offset period.start_date;
    end_date = add_offset_to_date offset period.end_date;
  }

let equal p1 p2 =
  CalendarLib.Date.equal p1.start_date p2.start_date
  && CalendarLib.Date.equal p1.end_date p2.end_date

let print { start_date; end_date } =
  Printf.printf "Period { start_date = %s; end_date = %s }\n"
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" start_date)
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" end_date)

let make_seq ~start_date ~offset =
  let end_date = add_offset_to_date offset start_date in
  let initial_period = { start_date; end_date } in
  Seq.unfold
    (fun period ->
      let next_period = add_offset offset period in
      Some (period, next_period))
    initial_period
