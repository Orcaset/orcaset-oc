type t = { date : CalendarLib.Date.t; value : float Lazy.t }

let make ~date ~value = { date; value }

let to_string balance =
  let date_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" balance.date in
  Printf.sprintf "Date: %s, Amount: %.2f" date_str (Lazy.force balance.value)
