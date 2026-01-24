module Accrual = Orcaset.Accrual

let make ~revenue_total ~cost_of_revenue_total = Accrual.sum_seq revenue_total cost_of_revenue_total
