(* Assumptions *)
let period_start = CalendarLib.Date.make 2025 1 1
let period_end = CalendarLib.Date.make 2025 4 1
let start_period = Orcaset.Period.make ~start_date:period_start ~end_date:period_end
let freq = Orcaset.Period.make_offset ~months:1 ()
let output_freq = Orcaset.Period.make_offset ~quarters:1 ()

(* Parameters *)
let software_first = 1000.
let software_growth = 0.1
let services_first = 500.
let services_multiple_opex = -0.5
let cogs_pct_rev = -0.3
let admin_first = -100.
let admin_rate = 0.05

(* Model *)
let yf = Orcaset.Yf.actual_360

let rec revenue =
  lazy
    (Revenue.make
       ~opex_total_lazy:(lazy (Lazy.force opex).Opex.total)
       ~start_period ~software_first ~software_growth ~services_first ~services_multiple_opex ~freq
       ~yf)

and opex =
  lazy
    (Opex.make ~revenue_lazy:revenue ~cogs_pct_rev ~admin_first ~admin_rate
       ~start_date:start_period.Orcaset.Period.start_date ~freq ~yf)

let revenue_model = Lazy.force revenue
let opex_model = Lazy.force opex
let income = Income.make ~revenue_total:revenue_model.total ~opex_total:opex_model.total

(* Statement *)
let income_statement =
  let open Orcaset.Statement in
  group "Income Statement"
    [
      group ~total:revenue_model.Revenue.total "Revenue"
        [
          line "Software" revenue_model.Revenue.software;
          line "Services" revenue_model.Revenue.services;
        ];
      group ~total:opex_model.Opex.total "Operating Expenses"
        [ line "COGS" opex_model.Opex.cogs; line "Admin" opex_model.Opex.admin ];
      line "Income" income;
    ]

(* Output *)
let print_statement ~periods item =
  let hdr p =
    Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Orcaset.Period.start_date)
  in
  let fmt v = Printf.sprintf "%14.2f" v in
  let print_row label seq =
    Printf.printf "%-30s%s\n" label
      (String.concat "" (List.map fmt (Orcaset.Accrual.accrue_periods periods seq)))
  in
  Printf.printf "%-30s%s\n" "" (String.concat "" (List.map hdr periods));
  let indent = ref 0 in
  Orcaset.Statement.iter item
    ~line_fn:(fun label seq -> print_row (String.make !indent ' ' ^ label) seq)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter -> indent := !indent + 2
      | `Exit ->
          Option.iter (print_row (String.make (!indent - 2) ' ' ^ "Total " ^ label)) total;
          print_newline ();
          indent := !indent - 2)

let () =
  let periods =
    Orcaset.Period.make_seq ~start_date:period_start ~offset:output_freq
    |> Seq.take 4 |> List.of_seq
  in
  print_statement ~periods income_statement
