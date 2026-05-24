# Basic Three-Statement Financial Model

A simple three-statement model linking income, balance sheet, and cash flow.

Circular references between line items are resolved correctly (between depreciation and PPE)

Example:

```sh
dune build && dune exec examples/2-Basic-Three-Statement/main.exe
```

## Statement Structure

```text
Financial model
├── Income statement
│   └── Net income
│       ├── EBIT
│       │   ├── Gross profit
│       │   │   ├── Revenue
│       │   │   └── Cost of revenue
│       │   ├── Operating expenses
│       │   └── Depreciation
│       └── Income tax
├── Cash flow statement
│   └── Total cash flow
│       ├── Operating cash flow
│       │   ├── Net income
│       │   └── Depreciation add back
│       ├── Investing cash flow
│       │   └── Capital expenditures
│       └── Cash flow from financing
└── Balance sheet
    ├── Total assets
    │   ├── Cash
    │   └── PPE net
    ├── Total equity and liabilities
    │   ├── Common stock
    │   └── Retained earnings
    └── Balance sheet check
```

## Line Items

| Line Item                 | Logic                                                                     |
| ------------------------- | ------------------------------------------------------------------------- |
| **Revenue**               | Starts at $1,000/month, grows 20% annually (1 + r * Actual/360 day count).|
| **Cost of revenue**       | Revenue x -0.30 (30% of revenue).                                         |
| **Gross profit**          | Revenue + cost of revenue.                                                |
| **Operating expenses**    | Constant -$200/month.                                                     |
| **Depreciation**          | Prior period's PPE net x (10% / 12).                                      |
| **EBIT**                  | Gross profit + operating expenses + depreciation.                         |
| **Income tax**            | EBIT x -0.20 (20% tax rate).                                              |
| **Net income**            | EBIT + income tax.                                                        |
| **Depreciation add back** | Reverses the non-cash depreciation charge.                                |
| **Operating cash flow**   | Net income + depreciation add back.                                       |
| **Capital expenditures**  | Revenue x -0.05 (5% of revenue).                                          |
| **Investing cash flow**   | Capital expenditures.                                                     |
| **Financing cash flow**   | Constant $0.                                                              |
| **Total cash flow**       | Sum of cash flow from operations + investing + financing.                 |
| **Cash**                  | Starts at $1,000. Accumulates net cash change.                            |
| **PPE, net**              | Starts at $10,000. Increases by capex, decreases by depreciation.         |
| **Common stock**          | Constant $5,000.                                                          |
| **Retained earnings**     | Starts at $6,000 (initial assets - common stock). Accumulates net income. |
| **Balance sheet check**   | Total assets - total equity and liabilities. Should be $0.                |


## Output

Running the executable prints this output to the console.

```txt
                              2025-12-31  2026-01-31  2026-02-28  2026-03-31  2026-04-30  2026-05-31  2026-06-30

      Revenue                                1000.00     1015.56     1033.05     1050.26     1068.35     1086.16
      Cost of revenue                        -300.00     -304.67     -309.91     -315.08     -320.51     -325.85
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
    Gross profit                              700.00      710.89      723.13      735.18      747.85      760.31

    Operating expenses                       -200.00     -200.00     -200.00     -200.00     -200.00     -200.00
    Depreciation                              -83.33      -83.06      -82.79      -82.53      -82.28      -82.04
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
  EBIT                                        416.67      427.83      440.35      452.66      465.57      478.27

  Income tax                                  -83.33      -85.57      -88.07      -90.53      -93.11      -95.65
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
Net income                                    333.33      342.27      352.28      362.13      372.45      382.62

    Net income                                333.33      342.27      352.28      362.13      372.45      382.62
    Depreciation add back                      83.33       83.06       82.79       82.53       82.28       82.04
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
  Operating cash flow                         416.67      425.32      435.06      444.65      454.73      464.66

    Capital expenditures                      -50.00      -50.78      -51.65      -52.51      -53.42      -54.31
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
  Investing cash flow                         -50.00      -50.78      -51.65      -52.51      -53.42      -54.31

  Cash flow from financing                      0.00        0.00        0.00        0.00        0.00        0.00
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
Total cash flow                               366.67      374.54      383.41      392.14      401.31      410.35


  Cash                           1000.00     1366.67     1741.21     2124.62     2516.76     2918.08     3328.42
  PPE net                       10000.00     9966.67     9934.39     9903.25     9873.24     9844.38     9816.65
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
Total assets                    11000.00    11333.33    11675.60    12027.88    12390.00    12762.46    13145.08

  Common stock                   5000.00     5000.00     5000.00     5000.00     5000.00     5000.00     5000.00
  Retained earnings              6000.00     6333.33     6675.60     7027.88     7390.00     7762.46     8145.08
                              ----------  ----------  ----------  ----------  ----------  ----------  ----------
Total equity and liabilities    11000.00    11333.33    11675.60    12027.88    12390.00    12762.46    13145.08

Balance sheet check                 0.00        0.00        0.00        0.00        0.00        0.00        0.00
```
