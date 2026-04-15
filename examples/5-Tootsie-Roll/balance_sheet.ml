open Orcaset

(* --- Helpers -------------------------------------------------------------- *)

let quarterly = Offset.make ~months:3 ~month_end:true ()

let point_of_observations ~label observations =
  match observations with
  | [] -> Series.Point.const ~label 0.0
  | (first_date, first_value) :: rest ->
      let first_period = Period.make (Date.add_days (-1) first_date) first_date in
      let rec deltas prev_date prev_value = function
        | [] -> Seq.empty
        | (date, value) :: tail ->
            let _gap_start = prev_date in
            let period = Period.make (Date.add_days (-1) date) date in
            Seq.cons
              (Series.Point.const_cell ~period (fun () -> value -. prev_value))
              (deltas date value tail)
      in
      Series.Point.unfold_self ~label ~cells:(fun () ->
          Seq.cons
            (Series.Point.const_cell ~period:first_period (fun () -> first_value))
            (deltas first_date first_value rest))

let history_of_points ~label observations =
  point_of_observations ~label:(label ^ " (hist)") observations

let flat_from_points ~label observations =
  let hist = history_of_points ~label observations in
  Series.Point.map ~label Fun.id (lazy hist)

let last_point_exn points = List.hd (List.rev points)

let accumulate_period_flow_from_date ~label ~start_date flow =
  let periods = Period.make_seq ~start_date ~offset:quarterly in
  Series.Point.unfold ~label ~deps:(Series.Point.dep_period flow) ~cells:(fun flow_dep ->
      Seq.map
        (fun period ->
          Series.Point.step ~period
            (Series.Point.Query.period flow_dep ~period ~reduce:Series.Period.reduce_sum)
            Fun.id)
        periods)

let rollforward_from_history ~label ~last_date hist flow =
  let future_flow = Series.Period.after ~label:(label ^ " Future Flow") last_date flow in
  let forecast =
    accumulate_period_flow_from_date ~label:(label ^ " (forecast)") ~start_date:last_date
      (lazy future_flow)
  in
  Series.Point.sum ~label (lazy hist) (lazy forecast)

let rollforward_from_points ~label observations flow =
  let hist = history_of_points ~label observations in
  let last_date, _ = last_point_exn observations in
  rollforward_from_history ~label ~last_date hist flow

let points csv name =
  List.map2
    (fun date value -> (date, Option.value ~default:0.0 value))
    (Csv.dates csv) (Csv.point_find csv name)

let points_nth csv name n =
  List.map2
    (fun date value -> (date, Option.value ~default:0.0 value))
    (Csv.dates csv) (Csv.point_find_nth csv name n)

(** [sum_list ~label xs] folds a list of point series into a single sum. *)
let sum_list ~label xs =
  match xs with
  | [] -> Series.Point.const ~label 0.0
  | [ x ] -> Series.Point.map ~label Fun.id (lazy x)
  | first :: rest ->
      let partial =
        List.fold_left (fun acc x -> Series.Point.sum ~label (lazy acc) (lazy x)) first rest
      in
      Series.Point.map ~label Fun.id (lazy partial)

(* --- Public interface ----------------------------------------------------- *)

(** [make csv ctx] constructs the balance sheet. [ctx] provides access to the income statement and
    cash flow statement. The balance sheet uses cash-flow deltas to roll forward working-capital,
    investment, debt, and equity lines so the forecast stays linked across statements. *)
let make csv (ctx : Types.ctx) =
  let i = Lazy.force ctx.i in
  let cf = Lazy.force ctx.cf in
  let net_earnings = lazy i.net_earnings in
  let change_in_cash = lazy cf.change_in_cash in
  let capex = lazy cf.capex in
  let depreciation = lazy cf.depreciation in
  let deferred_income_taxes = lazy cf.deferred_income_taxes in
  let purchases_trading = lazy cf.purchases_trading in
  let sales_trading = lazy cf.sales_trading in
  let purchases_afs = lazy cf.purchases_afs in
  let sales_afs = lazy cf.sales_afs in
  let amortization_premiums = lazy cf.amortization_premiums in
  let change_ar = lazy cf.change_ar in
  let change_other_receivables = lazy cf.change_other_receivables in
  let change_inventories = lazy cf.change_inventories in
  let change_prepaids = lazy cf.change_prepaids in
  let change_ap_and_accrued = lazy cf.change_ap_and_accrued in
  let change_income_taxes_payable = lazy cf.change_income_taxes_payable in
  let change_postretirement = lazy cf.change_postretirement in
  let change_deferred_comp = lazy cf.change_deferred_comp in
  let dividends_paid = lazy cf.dividends_paid in
  let proceeds_bank_loans = lazy cf.proceeds_bank_loans in
  let repayment_bank_loans = lazy cf.repayment_bank_loans in
  let shares_repurchased = lazy cf.shares_repurchased in

  (* --- Current Assets ------------------------------------------------------ *)
  let cash =
    rollforward_from_points ~label:"Cash" (points csv "Cash and cash equivalents") change_in_cash
  in
  let restricted_cash =
    flat_from_points ~label:"Restricted Cash" (points csv "Restricted cash")
  in
  let trading_investments_change =
    let trading_cash_effect =
      Series.Period.sum ~label:"Trading Investments Cash Effect"
        [ purchases_trading; sales_trading ]
    in
    lazy
      (Series.Period.map ~label:"Trading Investments Change" (fun x -> -.x) (lazy trading_cash_effect))
  in
  let investments_current =
    rollforward_from_points ~label:"Investments (Current)" (points_nth csv "Investments" 0)
      trading_investments_change
  in
  let ar_flow =
    lazy (Series.Period.map ~label:"AR Change" (fun x -> -.x) change_ar)
  in
  let accounts_receivable =
    rollforward_from_points ~label:"Accounts Receivable"
      (points csv "Accounts receivable trade, Net")
      ar_flow
  in
  let other_receivables_flow =
    lazy
      (Series.Period.map ~label:"Other Receivables Change" (fun x -> -.x) change_other_receivables)
  in
  let other_receivables =
    rollforward_from_points ~label:"Other Receivables" (points csv "Other receivables")
      other_receivables_flow
  in
  let finished_goods_points = points csv "Finished goods and work-in-process" in
  let raw_materials_points = points csv "Raw materials and supplies" in
  let finished_goods_hist =
    history_of_points ~label:"Finished Goods" finished_goods_points
  in
  let raw_materials_hist =
    history_of_points ~label:"Raw Materials" raw_materials_points
  in
  let inventories_hist =
    Series.Point.sum ~label:"Inventories (hist)" (lazy finished_goods_hist) (lazy raw_materials_hist)
  in
  let inventories_flow =
    lazy (Series.Period.map ~label:"Inventories Change" (fun x -> -.x) change_inventories)
  in
  let inventories =
    let last_inventory_date, _ = last_point_exn finished_goods_points in
    rollforward_from_history ~label:"Inventories" ~last_date:last_inventory_date inventories_hist
      inventories_flow
  in
  let prepaid_flow =
    lazy (Series.Period.map ~label:"Prepaids Change" (fun x -> -.x) change_prepaids)
  in
  let prepaid_expenses =
    rollforward_from_points ~label:"Prepaid Expenses" (points_nth csv "Prepaid expenses" 0)
      prepaid_flow
  in
  let total_current_assets =
    sum_list ~label:"Total Current Assets"
      [
        cash;
        restricted_cash;
        investments_current;
        accounts_receivable;
        other_receivables;
        inventories;
        prepaid_expenses;
      ]
  in

  (* --- Non-Current Assets -------------------------------------------------- *)
  let ppe_change =
    let neg_capex = Series.Period.map ~label:"Neg Capex" (fun x -> -.x) capex in
    let neg_depr = Series.Period.map ~label:"Neg Depreciation" (fun x -> -.x) depreciation in
    lazy (Series.Period.sum ~label:"PP&E Change" [ lazy neg_capex; lazy neg_depr ])
  in
  let net_ppe =
    rollforward_from_points ~label:"Net PP&E"
      (points csv "Net property, plant and equipment")
      ppe_change
  in
  let goodwill = flat_from_points ~label:"Goodwill" (points csv "Goodwill") in
  let trademarks = flat_from_points ~label:"Trademarks" (points csv "Trademarks") in
  let afs_investments_change =
    let afs_cash_effect =
      Series.Period.sum ~label:"AFS Investments Cash Effect"
        [ purchases_afs; sales_afs; amortization_premiums ]
    in
    lazy (Series.Period.map ~label:"AFS Investments Change" (fun x -> -.x) (lazy afs_cash_effect))
  in
  let investments_lt =
    rollforward_from_points ~label:"Investments (LT)" (points_nth csv "Investments" 1)
      afs_investments_change
  in
  let other_lt_assets =
    flat_from_points ~label:"Other LT Assets" (points csv "Prepaid expenses and other assets")
  in
  let deferred_income_taxes_asset =
    flat_from_points ~label:"Deferred Income Taxes (Asset)" (points csv "Deferred income taxes")
  in
  let total_other_assets =
    sum_list ~label:"Total Other Assets"
      [ goodwill; trademarks; investments_lt; other_lt_assets; deferred_income_taxes_asset ]
  in
  let total_assets =
    sum_list ~label:"Total Assets" [ total_current_assets; net_ppe; total_other_assets ]
  in

  (* --- Current Liabilities ------------------------------------------------- *)
  let accounts_payable_points = points csv "Accounts payable" in
  let accrued_points = points csv "Accrued liabilities" in
  let last_ap = snd (last_point_exn accounts_payable_points) in
  let last_accrued = snd (last_point_exn accrued_points) in
  let ap_total = last_ap +. last_accrued in
  let ap_share = if Float.equal ap_total 0.0 then 0.5 else last_ap /. ap_total in
  let accrued_share = 1.0 -. ap_share in
  let accounts_payable_flow =
    lazy
      (Series.Period.map ~label:"Accounts Payable Flow" (fun x -> x *. ap_share)
         change_ap_and_accrued)
  in
  let accounts_payable =
    rollforward_from_points ~label:"Accounts Payable" accounts_payable_points
      accounts_payable_flow
  in
  let bank_loans_flow =
    let net_bank_loans =
      Series.Period.sum ~label:"Net Bank Loans Change"
        [ proceeds_bank_loans; repayment_bank_loans ]
    in
    lazy net_bank_loans
  in
  let bank_loans_current =
    rollforward_from_points ~label:"Bank Loans (Current)" (points csv "Bank loans")
      bank_loans_flow
  in
  let dividends_payable =
    flat_from_points ~label:"Dividends Payable" (points csv "Dividends payable")
  in
  let accrued_liabilities_flow =
    lazy
      (Series.Period.map ~label:"Accrued Liabilities Flow" (fun x -> x *. accrued_share)
         change_ap_and_accrued)
  in
  let accrued_liabilities =
    rollforward_from_points ~label:"Accrued Liabilities" accrued_points accrued_liabilities_flow
  in
  let postretirement_current_points =
    points_nth csv "Postretirement health care benefits" 0
  in
  let postretirement_lt_points =
    points_nth csv "Postretirement health care benefits" 1
  in
  let last_postretirement_current = snd (last_point_exn postretirement_current_points) in
  let last_postretirement_lt = snd (last_point_exn postretirement_lt_points) in
  let postretirement_total = last_postretirement_current +. last_postretirement_lt in
  let postretirement_current_share =
    if Float.equal postretirement_total 0.0 then 0.5
    else last_postretirement_current /. postretirement_total
  in
  let postretirement_lt_share = 1.0 -. postretirement_current_share in
  let postretirement_current_flow =
    lazy
      (Series.Period.map ~label:"Postretirement Current Flow"
         (fun x -> x *. postretirement_current_share)
         change_postretirement)
  in
  let postretirement_current =
    rollforward_from_points ~label:"Postretirement (Current)" postretirement_current_points
      postretirement_current_flow
  in
  let operating_lease_current =
    flat_from_points ~label:"Operating Lease (Current)"
      (points_nth csv "Operating lease liabilities" 0)
  in
  let income_taxes_payable =
    rollforward_from_points ~label:"Income Taxes Payable" (points csv "Income taxes payable")
      change_income_taxes_payable
  in
  let deferred_comp_current_points = points csv "Deferred compensation" in
  let deferred_comp_other_points = points csv "Deferred compensation and other liabilities" in
  let last_deferred_comp_current = snd (last_point_exn deferred_comp_current_points) in
  let last_deferred_comp_other = snd (last_point_exn deferred_comp_other_points) in
  let deferred_comp_total = last_deferred_comp_current +. last_deferred_comp_other in
  let deferred_comp_current_share =
    if Float.equal deferred_comp_total 0.0 then 0.5
    else last_deferred_comp_current /. deferred_comp_total
  in
  let deferred_comp_other_share = 1.0 -. deferred_comp_current_share in
  let deferred_comp_current_flow =
    lazy
      (Series.Period.map ~label:"Deferred Compensation Current Flow"
         (fun x -> x *. deferred_comp_current_share)
         change_deferred_comp)
  in
  let deferred_compensation_current =
    rollforward_from_points ~label:"Deferred Compensation (Current)"
      deferred_comp_current_points deferred_comp_current_flow
  in
  let total_current_liabilities =
    sum_list ~label:"Total Current Liabilities"
      [
        accounts_payable;
        bank_loans_current;
        dividends_payable;
        accrued_liabilities;
        postretirement_current;
        operating_lease_current;
        income_taxes_payable;
        deferred_compensation_current;
      ]
  in

  (* --- Non-Current Liabilities --------------------------------------------- *)
  let deferred_income_taxes_liability =
    rollforward_from_points ~label:"Deferred Income Taxes (Liability)"
      (points_nth csv "Deferred income taxes" 1)
      deferred_income_taxes
  in
  let postretirement_lt_flow =
    lazy
      (Series.Period.map ~label:"Postretirement LT Flow"
         (fun x -> x *. postretirement_lt_share)
         change_postretirement)
  in
  let postretirement_lt =
    rollforward_from_points ~label:"Postretirement (LT)" postretirement_lt_points
      postretirement_lt_flow
  in
  let industrial_dev_bond =
    flat_from_points ~label:"Industrial Dev Bond" (points csv "Industrial development bond")
  in
  let uncertain_tax_positions =
    flat_from_points ~label:"Uncertain Tax Positions"
      (points csv "Liability for uncertain tax positions")
  in
  let operating_lease_lt =
    flat_from_points ~label:"Operating Lease (LT)"
      (points_nth csv "Operating lease liabilities" 1)
  in
  let deferred_comp_other_flow =
    lazy
      (Series.Period.map ~label:"Deferred Compensation Other Flow"
         (fun x -> x *. deferred_comp_other_share)
         change_deferred_comp)
  in
  let deferred_comp_and_other =
    rollforward_from_points ~label:"Deferred Comp & Other" deferred_comp_other_points
      deferred_comp_other_flow
  in
  let total_noncurrent_liabilities =
    sum_list ~label:"Total Noncurrent Liabilities"
      [
        deferred_income_taxes_liability;
        postretirement_lt;
        industrial_dev_bond;
        uncertain_tax_positions;
        operating_lease_lt;
        deferred_comp_and_other;
      ]
  in

  (* --- Equity -------------------------------------------------------------- *)
  let common_stock =
    flat_from_points ~label:"Common Stock" (points csv "Common stock")
  in
  let class_b_stock =
    flat_from_points ~label:"Class B Common Stock" (points csv "Class B common stock")
  in
  let capital_in_excess =
    flat_from_points ~label:"Capital in Excess of Par"
      (points csv "Capital in excess of par value")
  in
  let retained_earnings_flow =
    lazy
      (Series.Period.sum ~label:"Retained Earnings Change"
         [ net_earnings; dividends_paid ])
  in
  let retained_earnings =
    rollforward_from_points ~label:"Retained Earnings" (points csv "Retained earnings")
      retained_earnings_flow
  in
  let accumulated_oci =
    flat_from_points ~label:"Accumulated OCI"
      (points csv "Accumulated other comprehensive loss")
  in
  let treasury_stock =
    rollforward_from_points ~label:"Treasury Stock" (points csv "Treasury stock, at cost")
      shares_repurchased
  in
  let total_shareholders_equity =
    sum_list ~label:"Total Shareholders' Equity"
      [
        common_stock;
        class_b_stock;
        capital_in_excess;
        retained_earnings;
        accumulated_oci;
        treasury_stock;
      ]
  in
  let noncontrolling_interests =
    flat_from_points ~label:"Noncontrolling Interests" (points csv "Noncontrolling interests")
  in
  let total_equity =
    sum_list ~label:"Total Equity" [ total_shareholders_equity; noncontrolling_interests ]
  in
  let total_liabilities_and_equity =
    sum_list ~label:"Total L&E"
      [ total_current_liabilities; total_noncurrent_liabilities; total_equity ]
  in
  let balance_check =
    Series.Point.sub ~label:"Balance Check" (lazy total_assets) (lazy total_liabilities_and_equity)
  in
  {
    Types.cash;
    restricted_cash;
    investments_current;
    accounts_receivable;
    other_receivables;
    inventories;
    prepaid_expenses;
    total_current_assets;
    net_ppe;
    goodwill;
    trademarks;
    investments_lt;
    other_lt_assets;
    deferred_income_taxes_asset;
    total_other_assets;
    total_assets;
    accounts_payable;
    bank_loans_current;
    dividends_payable;
    accrued_liabilities;
    postretirement_current;
    operating_lease_current;
    income_taxes_payable;
    deferred_compensation_current;
    total_current_liabilities;
    deferred_income_taxes_liability;
    postretirement_lt;
    industrial_dev_bond;
    uncertain_tax_positions;
    operating_lease_lt;
    deferred_comp_and_other;
    total_noncurrent_liabilities;
    common_stock;
    class_b_stock;
    capital_in_excess;
    retained_earnings;
    accumulated_oci;
    treasury_stock;
    total_shareholders_equity;
    noncontrolling_interests;
    total_equity;
    total_liabilities_and_equity;
    balance_check;
  }
