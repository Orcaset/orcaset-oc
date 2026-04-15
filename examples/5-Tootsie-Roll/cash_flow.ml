open Orcaset

(* --- Helpers -------------------------------------------------------------- *)

let c csv label name = Series.Period.of_seq ~label (List.to_seq (Csv.quarterly_cells csv name))

let quarterly = Offset.make ~months:3 ~month_end:true ()
let prior_quarter = Offset.make ~months:(-3) ~month_end:true ()

let take_last n xs =
  let rec drop k ys =
    match (k, ys) with
    | 0, _ | _, [] -> ys
    | k, _ :: tail -> drop (k - 1) tail
  in
  let len = List.length xs in
  drop (max 0 (len - n)) xs

let average xs =
  match xs with
  | [] -> 0.0
  | _ -> List.fold_left ( +. ) 0.0 xs /. Float.of_int (List.length xs)

let trailing_average ?(n = 4) values = values |> List.filter_map Fun.id |> take_last n |> average

let latest_some_exn values =
  match List.find_map Fun.id (List.rev values) with
  | Some value -> value
  | None -> failwith "expected at least one historical value"

let latest_point_value_exn points = points |> List.rev |> List.hd |> snd
let safe_ratio numerator denominator = if Float.equal denominator 0.0 then 0.0 else numerator /. denominator

let flat_forecast ~label csv name ~amount =
  let hist = c csv (label ^ " (hist)") name in
  let forecast last_period =
    Series.Period.unfold_self ~label:(label ^ " (forecast)") ~cells:(fun () ->
        let rec unfold prev () =
          let cur = Period.next quarterly prev in
          let cell = Series.Period.const ~period:cur (fun () -> amount) in
          Seq.Cons (cell, unfold cur)
        in
        unfold last_period)
  in
  Series.Period.extend ~label hist forecast

(** Forecast a cash-flow line as the change in a target balance that scales with a driver. *)
let driver_delta_forecast ~label csv name ~ratio ~driver ~asset_like =
  let hist = c csv (label ^ " (hist)") name in
  let forecast last_period =
    Series.Period.unfold ~label:(label ^ " (forecast)") ~deps:(Series.Period.dep_period driver)
      ~cells:(fun driver_dep ->
        let rec unfold prev () =
          let cur = Period.next quarterly prev in
          let prev_period = Period.shift prior_quarter cur in
          let cell =
            Series.Period.step ~period:cur
              (let open Series.Period.Query in
               let+ current_driver =
                 period driver_dep ~period:cur ~reduce:Series.Period.reduce_sum
               and+ previous_driver =
                 period driver_dep ~period:prev_period ~reduce:Series.Period.reduce_sum in
               (current_driver, previous_driver))
              (fun (current_driver, previous_driver) ->
                let target_delta = ratio *. (current_driver -. previous_driver) in
                if asset_like then -.target_delta else target_delta)
          in
          Seq.Cons (cell, unfold cur)
        in
        unfold last_period)
  in
  Series.Period.extend ~label hist forecast

(* --- Public interface ----------------------------------------------------- *)

(** [make csv ctx] constructs the cash flow statement. [ctx] provides access to the income
    statement; the cash flow statement pulls [net_earnings] from it. *)
let make csv bs_csv is_csv (ctx : Types.ctx) =
  let net_earnings = (Lazy.force ctx.i).net_earnings in
  let i = Lazy.force ctx.i in
  let revenue = lazy i.revenue in
  let total_costs = lazy (Series.Period.map ~label:"Total Costs (positive)" Float.abs (lazy i.cogs)) in
  let sga = lazy (Series.Period.map ~label:"SGA (positive)" Float.abs (lazy i.sga)) in
  let tax_expense = lazy (Series.Period.map ~label:"Tax (positive)" Float.abs (lazy i.tax)) in
  let payable_driver =
    lazy (Series.Period.sum ~label:"Payables Driver" [ total_costs; sga ])
  in
  let last_revenue = latest_some_exn (Csv.quarterly_find is_csv "Total revenue") in
  let last_total_costs = latest_some_exn (Csv.quarterly_find is_csv "Total costs") in
  let last_sga =
    latest_some_exn
      (Csv.quarterly_find is_csv "Selling, marketing and administrative expenses")
  in
  let last_tax = latest_some_exn (Csv.quarterly_find is_csv "Provision for income taxes") in
  let ar_ratio =
    safe_ratio
      (latest_point_value_exn (Csv.points bs_csv "Accounts receivable trade, Net"))
      last_revenue
  in
  let other_receivables_ratio =
    safe_ratio (latest_point_value_exn (Csv.points bs_csv "Other receivables")) last_revenue
  in
  let inventory_ratio =
    let finished = latest_point_value_exn (Csv.points bs_csv "Finished goods and work-in-process") in
    let raw = latest_point_value_exn (Csv.points bs_csv "Raw materials and supplies") in
    safe_ratio (finished +. raw) last_total_costs
  in
  let prepaid_ratio =
    safe_ratio (latest_point_value_exn (Csv.points_nth bs_csv "Prepaid expenses" 0)) last_revenue
  in
  let ap_and_accrued_ratio =
    let accounts_payable = latest_point_value_exn (Csv.points bs_csv "Accounts payable") in
    let accrued = latest_point_value_exn (Csv.points bs_csv "Accrued liabilities") in
    safe_ratio (accounts_payable +. accrued) (last_total_costs +. last_sga)
  in
  let income_taxes_payable_ratio =
    safe_ratio (latest_point_value_exn (Csv.points bs_csv "Income taxes payable")) last_tax
  in
  (* --- Operating Activities ------------------------------------------------ *)

  (* Net earnings flows directly from the income statement.
     Historical from CSV, forecast from IS. *)
  let ne_hist = c csv "Net Earnings (hist)" "Net earnings" in
  let ne =
    Series.Period.extend ~label:"Net Earnings" ne_hist (fun last_period ->
        let hist_end_date = Period.end_date last_period in
        Series.Period.after ~label:"Net Earnings (forecast)" hist_end_date (lazy net_earnings))
  in

  let depreciation =
    flat_forecast ~label:"Depreciation" csv "Depreciation"
      ~amount:(trailing_average (Csv.quarterly_find csv "Depreciation"))
  in
  let deferred_income_taxes =
    flat_forecast ~label:"Deferred Income Taxes" csv "Deferred income taxes"
      ~amount:(trailing_average (Csv.quarterly_find csv "Deferred income taxes"))
  in
  let amortization_premiums =
    flat_forecast ~label:"Amort. Premiums" csv
      "Amortization of marketable security premiums and discounts, net"
      ~amount:(trailing_average (Csv.quarterly_find csv
                   "Amortization of marketable security premiums and discounts, net"))
  in
  let change_ar =
    driver_delta_forecast ~label:"Change in AR" csv "Accounts receivable" ~ratio:ar_ratio
      ~driver:revenue ~asset_like:true
  in
  let change_other_receivables =
    driver_delta_forecast ~label:"Change in Other Recv." csv "Other receivables"
      ~ratio:other_receivables_ratio ~driver:revenue ~asset_like:true
  in
  let change_inventories =
    driver_delta_forecast ~label:"Change in Inventories" csv "Inventories"
      ~ratio:inventory_ratio ~driver:total_costs ~asset_like:true
  in
  let change_prepaids =
    driver_delta_forecast ~label:"Change in Prepaids" csv "Prepaid expenses and other assets"
      ~ratio:prepaid_ratio ~driver:revenue ~asset_like:true
  in
  let change_ap_and_accrued =
    driver_delta_forecast ~label:"Change in AP & Accrued" csv
      "Accounts payable and accrued liabilities" ~ratio:ap_and_accrued_ratio
      ~driver:payable_driver ~asset_like:false
  in
  let change_income_taxes_payable =
    driver_delta_forecast ~label:"Change in Tax Payable" csv "Income taxes payable"
      ~ratio:income_taxes_payable_ratio ~driver:tax_expense ~asset_like:false
  in
  let change_postretirement =
    flat_forecast ~label:"Change in Postretirement" csv "Postretirement health care benefits"
      ~amount:(trailing_average (Csv.quarterly_find csv "Postretirement health care benefits"))
  in
  let change_deferred_comp =
    flat_forecast ~label:"Change in Deferred Comp" csv "Deferred compensation and other liabilities"
      ~amount:(trailing_average (Csv.quarterly_find csv "Deferred compensation and other liabilities"))
  in
  let cfo =
    Series.Period.sum ~label:"Cash from Operations"
      [
        lazy ne;
        lazy depreciation;
        lazy deferred_income_taxes;
        lazy amortization_premiums;
        lazy change_ar;
        lazy change_other_receivables;
        lazy change_inventories;
        lazy change_prepaids;
        lazy change_ap_and_accrued;
        lazy change_income_taxes_payable;
        lazy change_postretirement;
        lazy change_deferred_comp;
      ]
  in

  (* --- Investing Activities ------------------------------------------------ *)
  let capex = flat_forecast ~label:"Capex" csv "Capital expenditures" ~amount:(-8_000_000.0) in
  let purchases_trading =
    flat_forecast ~label:"Purchases Trading" csv "Purchases of trading securities"
      ~amount:(-300_000.0)
  in
  let sales_trading =
    flat_forecast ~label:"Sales Trading" csv "Sales of trading securities" ~amount:200_000.0
  in
  let purchases_afs =
    flat_forecast ~label:"Purchases AFS" csv "Purchase of available for sale securities"
      ~amount:(-20_000_000.0)
  in
  let sales_afs =
    flat_forecast ~label:"Sales/Maturity AFS" csv
      "Sale and maturity of available for sale securities" ~amount:15_000_000.0
  in
  let cfi =
    Series.Period.sum ~label:"Cash from Investing"
      [ lazy capex; lazy purchases_trading; lazy sales_trading; lazy purchases_afs; lazy sales_afs ]
  in

  (* --- Financing Activities ------------------------------------------------ *)
  let shares_repurchased =
    flat_forecast ~label:"Shares Repurchased" csv "Shares purchased and retired"
      ~amount:(-3_000_000.0)
  in
  let dividends_paid =
    flat_forecast ~label:"Dividends Paid" csv "Dividends paid in cash" ~amount:(-6_500_000.0)
  in
  let proceeds_bank_loans =
    flat_forecast ~label:"Proceeds Bank Loans" csv "Proceeds from bank loans" ~amount:800_000.0
  in
  let repayment_bank_loans =
    flat_forecast ~label:"Repayment Bank Loans" csv "Repayment of bank loans" ~amount:(-800_000.0)
  in
  let cff =
    Series.Period.sum ~label:"Cash from Financing"
      [
        lazy shares_repurchased;
        lazy dividends_paid;
        lazy proceeds_bank_loans;
        lazy repayment_bank_loans;
      ]
  in

  (* --- FX & Totals --------------------------------------------------------- *)
  let fx_effect =
    flat_forecast ~label:"FX Effect" csv "Effect of exchange rate changes on cash" ~amount:0.0
  in
  let change_in_cash =
    Series.Period.sum ~label:"Change in Cash" [ lazy cfo; lazy cfi; lazy cff; lazy fx_effect ]
  in
  {
    Types.net_earnings = ne;
    depreciation;
    deferred_income_taxes;
    amortization_premiums;
    change_ar;
    change_other_receivables;
    change_inventories;
    change_prepaids;
    change_ap_and_accrued;
    change_income_taxes_payable;
    change_postretirement;
    change_deferred_comp;
    cfo;
    capex;
    purchases_trading;
    sales_trading;
    purchases_afs;
    sales_afs;
    cfi;
    shares_repurchased;
    dividends_paid;
    proceeds_bank_loans;
    repayment_bank_loans;
    cff;
    fx_effect;
    change_in_cash;
  }
