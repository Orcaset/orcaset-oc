# Dynamically Creating Line Items

This example shows how you can build new line items series in response to user queries.

Specifically, this example creates a full depreciation schedule for a variable number of cohorts based on the queried date range. Capital expenditures are bucketed into quarters and expensed in equal parts over the following year. As longer spans of time are queried, more cohort lines are materialized.

## How dynamic series work

Dynamic series are represented with `Series.Family.make`. A family has two important functions:

* `active_keys` receives the period being queried and returns the keys that should exist for that period. In this example, the keys are quarterly capex cohorts whose depreciation schedules overlap the query period.
* `member` receives one key and builds the concrete series for it. Here, each cohort becomes a depreciation line generated with `Series.Spans.unfold`, advancing one quarter at a time with `Period.next`.

This keeps the model open-ended: the code does not need to predeclare every future cohort. Instead, Orcaset asks the family which members are active for the current query and materializes only those lines.

## Model structure

* **Capital expenditures:** Assumed monthly expenditures.
* **Cohort depreciation:** Depreciation schedule for each capex cohort.
* **Total depreciation:** The sum of depreciation from the cohort schedules.


## Output

Querying the statement for eight quarters produces the table below (note that there are no capital expenditures after Q4 2026). The number of cohort detail scheduels will expand or shrink as the query range grows or shrinks.

```txt
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
Total depreciation                  -90.00     -180.00     -285.00     -397.50     -397.50     -217.50     -112.50
```
