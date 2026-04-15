open Orcaset

type income = {
  net_product_sales : [ `USD ] Series.Period.t;
  rental_and_royalty_revenue : [ `USD ] Series.Period.t;
  revenue : [ `USD ] Series.Period.t;
  product_cogs : [ `USD ] Series.Period.t;
  rental_and_royalty_cost : [ `USD ] Series.Period.t;
  cogs : [ `USD ] Series.Period.t;
  product_gross_margin : [ `USD ] Series.Period.t;
  rental_and_royalty_gross_margin : [ `USD ] Series.Period.t;
  gross_margin : [ `USD ] Series.Period.t;
  sga : [ `USD ] Series.Period.t;
  ebit : [ `USD ] Series.Period.t;
  other_income : [ `USD ] Series.Period.t;
  ebt : [ `USD ] Series.Period.t;
  tax : [ `USD ] Series.Period.t;
  net_earnings : [ `USD ] Series.Period.t;
}

type cash_flow = {
  net_earnings : [ `USD ] Series.Period.t;
  depreciation : [ `USD ] Series.Period.t;
  deferred_income_taxes : [ `USD ] Series.Period.t;
  amortization_premiums : [ `USD ] Series.Period.t;
  change_ar : [ `USD ] Series.Period.t;
  change_other_receivables : [ `USD ] Series.Period.t;
  change_inventories : [ `USD ] Series.Period.t;
  change_prepaids : [ `USD ] Series.Period.t;
  change_ap_and_accrued : [ `USD ] Series.Period.t;
  change_income_taxes_payable : [ `USD ] Series.Period.t;
  change_postretirement : [ `USD ] Series.Period.t;
  change_deferred_comp : [ `USD ] Series.Period.t;
  cfo : [ `USD ] Series.Period.t;
  capex : [ `USD ] Series.Period.t;
  purchases_trading : [ `USD ] Series.Period.t;
  sales_trading : [ `USD ] Series.Period.t;
  purchases_afs : [ `USD ] Series.Period.t;
  sales_afs : [ `USD ] Series.Period.t;
  cfi : [ `USD ] Series.Period.t;
  shares_repurchased : [ `USD ] Series.Period.t;
  dividends_paid : [ `USD ] Series.Period.t;
  proceeds_bank_loans : [ `USD ] Series.Period.t;
  repayment_bank_loans : [ `USD ] Series.Period.t;
  cff : [ `USD ] Series.Period.t;
  fx_effect : [ `USD ] Series.Period.t;
  change_in_cash : [ `USD ] Series.Period.t;
}

type balance_sheet = {
  cash : [ `USD ] Series.Point.t;
  restricted_cash : [ `USD ] Series.Point.t;
  investments_current : [ `USD ] Series.Point.t;
  accounts_receivable : [ `USD ] Series.Point.t;
  other_receivables : [ `USD ] Series.Point.t;
  inventories : [ `USD ] Series.Point.t;
  prepaid_expenses : [ `USD ] Series.Point.t;
  total_current_assets : [ `USD ] Series.Point.t;
  net_ppe : [ `USD ] Series.Point.t;
  goodwill : [ `USD ] Series.Point.t;
  trademarks : [ `USD ] Series.Point.t;
  investments_lt : [ `USD ] Series.Point.t;
  other_lt_assets : [ `USD ] Series.Point.t;
  deferred_income_taxes_asset : [ `USD ] Series.Point.t;
  total_other_assets : [ `USD ] Series.Point.t;
  total_assets : [ `USD ] Series.Point.t;
  accounts_payable : [ `USD ] Series.Point.t;
  bank_loans_current : [ `USD ] Series.Point.t;
  dividends_payable : [ `USD ] Series.Point.t;
  accrued_liabilities : [ `USD ] Series.Point.t;
  postretirement_current : [ `USD ] Series.Point.t;
  operating_lease_current : [ `USD ] Series.Point.t;
  income_taxes_payable : [ `USD ] Series.Point.t;
  deferred_compensation_current : [ `USD ] Series.Point.t;
  total_current_liabilities : [ `USD ] Series.Point.t;
  deferred_income_taxes_liability : [ `USD ] Series.Point.t;
  postretirement_lt : [ `USD ] Series.Point.t;
  industrial_dev_bond : [ `USD ] Series.Point.t;
  uncertain_tax_positions : [ `USD ] Series.Point.t;
  operating_lease_lt : [ `USD ] Series.Point.t;
  deferred_comp_and_other : [ `USD ] Series.Point.t;
  total_noncurrent_liabilities : [ `USD ] Series.Point.t;
  common_stock : [ `USD ] Series.Point.t;
  class_b_stock : [ `USD ] Series.Point.t;
  capital_in_excess : [ `USD ] Series.Point.t;
  retained_earnings : [ `USD ] Series.Point.t;
  accumulated_oci : [ `USD ] Series.Point.t;
  treasury_stock : [ `USD ] Series.Point.t;
  total_shareholders_equity : [ `USD ] Series.Point.t;
  noncontrolling_interests : [ `USD ] Series.Point.t;
  total_equity : [ `USD ] Series.Point.t;
  total_liabilities_and_equity : [ `USD ] Series.Point.t;
  balance_check : [ `USD ] Series.Point.t;
}

type ctx = { i : income Lazy.t; cf : cash_flow Lazy.t; bs : balance_sheet Lazy.t }
