module Accrual = Orcaset.Accrual

let make ~start_date ~initial_value ~rate ~freq ~yf =
  Accrual.const_annual_growth_seq ~start_date ~initial_value ~rate ~freq ~yf
