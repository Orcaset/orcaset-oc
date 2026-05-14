open Orcaset

(* ----- Assumptions & Helpers ----- *)
let sum_agg = Series.Agg.sum
let qtr_offset = Offset.make ~quarters:1 ()
let month_offset = Offset.make ~months:1 ()
let start_date = Date.make 2025 12 31

let quarter_label period =
  let start = Period.start period in
  let quarter = ((Date.month start - 1) / 3) + 1 in
  Printf.sprintf "Q%d %04d" quarter (Date.year start)

let period_compare a b =
  let start_cmp = Date.compare (Period.start a) (Period.start b) in
  if start_cmp <> 0 then start_cmp else Date.compare (Period.end_ a) (Period.end_ b)

let schedule_overlaps query_period ~schedule_start ~schedule_end =
  Date.(Period.start query_period < schedule_end && Period.end_ query_period > schedule_start)

let capex_cohorts_for query_period =
  let rec aux cohorts =
    match cohorts () with
    | Seq.Nil -> []
    | Seq.Cons (cohort, rest) ->
        if Date.(Period.end_ cohort >= Period.end_ query_period) then [] else cohort :: aux rest
  in
  Period.make_seq ~start:start_date ~offset:qtr_offset |> aux

(* ----- Create some demo capital expenditure data ----- *)
let monthly_period i =
  let start = Date.shift (Offset.make ~months:i ()) start_date in
  Period.make start (Date.shift month_offset start)

let capex =
  let monthly_capex =
    [ 120.0; 150.0; 90.0; 80.0; 110.0; 170.0; 240.0; 60.0; 120.0; 90.0; 150.0; 210.0 ]
    |> List.mapi (fun i amount -> (monthly_period i, amount))
  in
  Series.Spans.of_list ~label:"CapEx" ~split:Split.daily ~agg:sum_agg monthly_capex

(* ----- Create a depreciation schedule family ----- *)
let depreciation_by_cohort =
  let key : Period.t Series.key_ops =
    {
      equal = Period.equal;
      hash = Period.hash;
      compare = period_compare;
      to_string = quarter_label;
    }
  in
  let depreciation_schedule_end cohort =
    Date.shift (Offset.make ~quarters:4 ()) (Period.end_ cohort)
  in
  Series.Family.make ~id:"depreciation-by-cohort" ~key
    ~active_keys:(fun query_period ->
      capex_cohorts_for query_period
      |> List.filter (fun cohort ->
          schedule_overlaps query_period ~schedule_start:(Period.end_ cohort)
            ~schedule_end:(depreciation_schedule_end cohort)))
    ~member:(fun cohort ->
      Series.Spans.unfold
        ~label:("Depreciation - " ^ quarter_label cohort)
        ~agg:sum_agg ~init:(Period.next qtr_offset cohort)
        ~cells:(fun period ->
          if Date.(Period.start period >= depreciation_schedule_end cohort) then None
          else
            let formula =
              let open Series.Formula in
              let+ cohort_capex = span_query capex ~period:cohort in
              Option.map (fun amount -> -.amount /. 4.0) cohort_capex
            in
            Some
              (Series.Spans.cell ~period ~split:Split.daily formula, Period.next qtr_offset period))
        ())

let total_depreciation =
  Series.Spans.sum_family ~label:"Total depreciation" ~agg:sum_agg depreciation_by_cohort

(* ----- Print output including depreciation for each ----- *)
let stmt =
  Stmt.group
    [
      Stmt.span_line capex;
      Stmt.span_total total_depreciation [ Stmt.span_family_lines depreciation_by_cohort ];
    ]

let () =
  let query_periods =
    Period.make_seq ~start:start_date ~offset:qtr_offset |> Seq.take 8 |> List.of_seq
  in
  let resolved = Stmt.eval_periods query_periods stmt in
  Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)

(* 
                    2026-03-31  2026-06-30  2026-09-30  2026-12-30  2027-03-30  2027-06-30  2027-09-30  2027-12-30

CapEx                   360.00      360.00      420.00      450.00                                                

  Q4 2025                           -90.00      -90.00      -90.00      -90.00      -90.00                        
  Q1 2026                                       -90.00      -90.00      -90.00      -90.00                        
  Q2 2026                                                  -105.00     -105.00     -105.00     -105.00            
  Q3 2026                                                              -112.50     -112.50     -112.50     -112.50
  Q4 2026                                                                                                         
  Q1 2027                                                                                                         
  Q2 2027                                                                                                         

                    ----------  ----------  ----------  ----------  ----------  ----------  ----------  ----------
Total depreciation                  -90.00     -180.00     -285.00     -397.50     -397.50     -217.50     -112.50 *)
