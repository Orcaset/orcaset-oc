module Accrual = Orcaset.Accrual

let make ~revenue_total ~opex_total = Accrual.sum_seq revenue_total opex_total
