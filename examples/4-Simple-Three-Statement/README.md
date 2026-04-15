# Three-Statement Financial Model

A simple three-statement model linking income, balance sheet, and cash flow.

* Circular references between line items are resolved correctly (between depreciation and PPE)
* Dynamic output: use the `-p` flag to change periodicity of output and `-n` flag to change the number of periods

Example:

```sh
dune build && dune exec examples/4-Simple-Three-Statement/main.exe -- -p monthly -n 24
```

## Statement Structure

```text
Financial Model
├── Income Statement
│   ├── Gross Profit
│   │   ├── Revenue
│   │   └── COGS
│   ├── Opex
│   ├── Depreciation
│   ├── Tax
│   └── Net Income
├── Cash Flow Statement
│   ├── Operations
│   │   ├── Net Income
│   │   └── Depreciation Add Back
│   ├── Investing
│   │   └── Capex
│   ├── CF Financing
│   └── Net Cash Change
└── Balance Sheet
    ├── Assets
    │   ├── Cash
    │   └── PPE Net
    ├── Liabilities & Equity
    │   ├── Common Stock
    │   └── Retained Earnings
    └── Check
        └── Balance Check
```

## Line Items

| Line Item                 | Logic                                                                     |
| ------------------------- | ------------------------------------------------------------------------- |
| **Revenue**               | Starts at $1,000/month, grows 5% annually (Actual/360 day count).         |
| **COGS**                  | Revenue x -0.30 (30% of revenue).                                         |
| **Gross Profit**          | Revenue + COGS.                                                           |
| **Opex**                  | Constant -$200/month.                                                     |
| **Depreciation**          | Prior period's PPE Net x (10% / 12).                                      |
| **Tax**                   | (Gross Profit + Opex + Depreciation) x -0.20 (20% tax rate).              |
| **Net Income**            | Earnings before tax + Tax.                                                |
| **Depreciation Add Back** | Reverses the non-cash depreciation charge.                                |
| **CF Operations**         | Net Income + Depreciation Add Back.                                       |
| **Capex**                 | Revenue x -0.05 (5% of revenue).                                          |
| **CF Financing**          | Constant $0.                                                              |
| **Net Cash Change**       | CF Operations + CF Investing + CF Financing.                              |
| **Cash**                  | Starts at $1,000. Accumulates Net Cash Change.                            |
| **PPE Net**               | Starts at $10,000. Increases by Capex, decreases by Depreciation.         |
| **Common Stock**          | Constant $5,000.                                                          |
| **Retained Earnings**     | Starts at $6,000 (Initial Assets - Common Stock). Accumulates Net Income. |
| **Balance Check**         | Total Assets - Total Liabilities & Equity. Should be $0.                  |


## Output

CLI flags are included for period frequency (quarterly default) and number of periods (defaults to 4).

```sh
Usage: main [-n <int>] [-p <monthly|quarterly|yearly>]
  -n Number of periods to output
  --num-periods Number of periods to output
  -p {monthly|quarterly|yearly} Output periodicity: monthly, quarterly, or yearly
  --periodicity {monthly|quarterly|yearly} Output periodicity: monthly, quarterly, or yearly
  -help  Display this list of options
  --help  Display this list of options
```

```txt
Period end               2025-12-31  2026-01-31  2026-02-28  2026-03-31  2026-04-30
-----------------------------------------------------------------------------------
    Revenue                               1,000       1,004       1,008       1,012
    COGS                                  (300)       (301)       (302)       (304)
                         ----------  ----------  ----------  ----------  ----------
  Gross Profit                              700         703         706         708
  Opex                                    (200)       (200)       (200)       (200)
  Depreciation                             (83)        (83)        (83)        (83)
  Tax                                      (83)        (84)        (85)        (85)
                         ----------  ----------  ----------  ----------  ----------
Net Income                                  333         336         338         341
  CF Operations                             417         419         421         423
  Capex                                    (50)        (50)        (50)        (51)
  CF Financing                                0           0           0           0
                         ----------  ----------  ----------  ----------  ----------
Net Cash Change                             367         369         371         373
    Cash                      1,000       1,367       1,735       2,106       2,479
    PPE Net                  10,000       9,967       9,934       9,901       9,870
                         ----------  ----------  ----------  ----------  ----------
  Total Assets               11,000      11,333      11,669      12,007      12,348
    Common Stock              5,000       5,000       5,000       5,000       5,000
    Retained Earnings         6,000       6,333       6,669       7,007       7,348
                         ----------  ----------  ----------  ----------  ----------
  Total Equity               11,000      11,333      11,669      12,007      12,348
                         ----------  ----------  ----------  ----------  ----------
Balance Check                     0           0           0           0           0
```