module Accrual = Orcaset.Accrual

let make ~gross_profit ~admin = Accrual.sum_seq gross_profit admin
