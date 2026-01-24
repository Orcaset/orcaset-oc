module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = { software : Accrual.t Seq.t; services : Accrual.t Seq.t; total : Accrual.t Seq.t }

let make ~opex_total_lazy ~start_period ~software_first ~software_growth ~services_first
    ~services_multiple_opex ~freq ~yf =
  let software =
    Accrual.const_annual_growth_seq ~start_date:start_period.Orcaset.Period.start_date
      ~initial_value:software_first ~rate:software_growth ~freq ~yf
  in
  let services =
    let first_accrual =
      Accrual.make ~period:start_period
        ~value:(lazy services_first)
        ~split_fn:Accrual.default_split_fn
    in
    let periods = Period.make_seq ~start_date:start_period.end_date ~offset:freq in

    Seq.cons first_accrual (fun () ->
        Seq.map
          (fun period ->
            let value =
              lazy
                (let opex =
                   Accrual.accrue period.Period.start_date period.end_date
                     (Lazy.force opex_total_lazy)
                 in
                 opex *. services_multiple_opex)
            in
            Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn)
          periods ())
  in
  let total = Accrual.sum_seq software services in
  { software; services; total }
